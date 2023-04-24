import BRAM::*;
import FIFO::*;
import SpecialFIFOs::*;
import MemTypes::*;
import Vector::*;
import Ehr::*;

interface Cache;
    method Action putFromProc(MainMemReq e);
    method ActionValue#(CacheResp) getToProc();
    method Action putFromMem(MainMemResp e);
    method ActionValue#(MainMemReq) getToMem();
endinterface

module mkCache(Cache);

  FIFO#(CacheResp) hitQ <- mkBypassFIFO;
  FIFO#(MainMemReq) memReqQ <- mkFIFO;
  FIFO#(MainMemResp) memRespQ <- mkFIFO;
  FIFO#(MainMemReq) stb <- mkSizedFIFO(1);

  Reg#(MainMemReq) missReq <- mkReg(?);
  Reg#(ReqStatus) mshr <- mkReg(Ready);

  BRAM_Configure cfg = defaultValue;
  BRAM1Port#(Bit#(7), Vector#(16, Bit#(32))) dataArray <- mkBRAM1ServerBE(cfg); //Fix this, makes an error

  Vector#(128, Vector#(16, Reg#(Maybe#(Bit#(19))))) tagArray <- replicateM(replicateM(mkReg(tagged Invalid))); 
  Vector#(128, Vector#(16, Reg#(Bit#(1)))) dirtyArray <- replicateM(replicateM(mkReg(0)));

  Ehr#(2, Bool) lockL1 <- mkEhr(False); //lock to give the processor priority

  Bool debug = False;
  
  //STOREBUFFER RULES
  rule mvStbToL1 (mshr == Ready && !lockL1[1]); 
    let e = stb.first();
    stb.deq();
    if(debug) $display("mvStbToL1 %x", e.addr);

    Bit#(4) offset = e.addr[5:2]; //check this
    Bit#(7) idx = e.addr[12:6];
    Bit#(19) tag = e.addr[31:13]; 

    if(fromMaybe(?, tagArray[idx][offset]) == tag) begin
      //start a hit
      //CHANGE TO WRITE TO BYTE ONLY
      dataArray.portA.request.put(BRAMRequest{write: True, //True for write
                  responseOnWrite: False,
                  address: idx,
                  datain: e.data});
      dirtyArray[idx][offset] <= 1;
    end else begin // we have a miss
      // check for data needed to write back
      let dirty_bit = dirtyArray[idx][offset]; 
      if(isValid(tagArray[idx][offset]) && dirty_bit == 1)begin
        dataArray.portA.request.put(BRAMRequest{write: False, // False for read
                  responseOnWrite: False,
                  address: idx,
                  datain: ?});
      end
      missReq <= e;
      mshr <= StartMiss;
    end
  endrule    

  rule clearL1Lock; // Rule to give priority to L1
    //if(debug) $display("clearlock");
    lockL1[1] <= False; 
  endrule 

  //MISS RULES
  rule startMiss(mshr == StartMiss);

    Bit#(7) idx = missReq.addr[12:6]; 
    let wb_tag = fromMaybe(?, tagArray[idx][offset]); 
    let dirty_bit = dirtyArray[idx][offset];
    if(debug) $display("startMiss %x", missReq.addr);

    if(isValid(tagArray[idx][offset]) && dirty_bit == 1)begin
      let data <- dataArray.portA.response.get();
      memReqQ.enq(MainMemReq{write : 1, addr : {wb_tag, idx, offset, 2'b0}, data : data });
    end 
    mshr <= SendFillReq;                      
  endrule
  
  rule sendFillReq (mshr == SendFillReq);
    if(debug) $display("sendFillReq %x", missReq.addr);
    memReqQ.enq(missReq);  
    mshr <= WaitFillResp;
  endrule

  rule waitFillResp(mshr == WaitFillResp);
    Bit#(4) offset = missReq.addr[5:2]; //check this
    Bit#(7) idx = missReq.addr[12:6];
    Bit#(19) tag = missReq.addr[31:13]; 
    if(debug) $display("waitFillResp %x", missReq.addr);

    tagArray[idx][offset] <= tagged Valid tag;

    //check of it's a load or a store
    if(missReq.write == 0) begin //it's a load
      let data = memRespQ.first();
      if(debug) $display("waitFillResp load %x", data);
      memRespQ.deq();
      dirtyArray[idx][offset] <= 0;
      dataArray.portA.request.put(BRAMRequest{write: True, //True for write
            responseOnWrite: False,
            address: idx,
            datain: data});
      hitQ.enq(data);
      
    end else begin //its a store
      let data = missReq.data;
      dirtyArray[idx][offset] <= 1; //this is dirty
      if(debug) $display("waitFillResp store %x", data);
      dataArray.portA.request.put(BRAMRequest{write: True, //True for write
            responseOnWrite: False,
            address: idx,
            datain: data});
    end
    mshr <= Ready;
  endrule

  rule getHit(mshr == GetHit);
    let hit_data <- dataArray.portA.response.get(); //get data from BRAM
    if(debug) $display("getHit hitenq %x", hit_data);
    hitQ.enq(hit_data);
    mshr <= Ready;
  endrule

  //METHODS
  method Action putFromProc(MainMemReq e) if(mshr == Ready);
    Bit#(4) offset = e.addr[5:2]; //check this
    Bit#(7) idx = e.addr[12:6];
    Bit#(19) tag = e.addr[31:13]; 
    if(debug) $display("putFromProc %x %x", e.addr, e.write);

    if(e.write == 0) begin // check if it's a load
      lockL1[0] <= True; //processor requests get priority. 
      let stb_val = stb.first(); 

      if(stb_val.addr == e.addr) begin //check for stb hit
        hitQ.enq(stb_val.data);
        if(debug) $display("stb hitenq %x", stb_val.data);

      end else begin //check for l1 hit
        if(fromMaybe(?, tagArray[idx][offset]) == tag) begin      
          //start a hit
          dataArray.portA.request.put(BRAMRequest{write: False, // False for read
                         responseOnWrite: False,
                         address: idx,
                         datain: ?});
          mshr <= GetHit;

        end else begin // we have a miss
          missReq <= e;
          let dirty_bit = dirtyArray[idx][offset];

          // recall the data to be replaced in the cache if needed
          if(isValid(tagArray[idx][offset]) && dirty_bit == 1)begin
            dataArray.portA.request.put(BRAMRequest{write: False, // False for read
                  responseOnWrite: False,
                  address: idx,
                  datain: ?});
          end
          mshr <= StartMiss;
        end

      end
    end else begin //it's a store, put it in the buffer
        stb.enq(e);
    end
  endmethod

  method ActionValue#(CacheResp) getToProc();
    hitQ.deq();
    if(debug) $display("getToProc");
    return hitQ.first();
  endmethod

  method Action putFromMem(MainMemResp e);
    if(debug) $display("putFromMem");
    memRespQ.enq(e);
  endmethod

  method ActionValue#(MainMemReq) getToMem();
    if(debug) $display("getToMem");
    memReqQ.deq();
    return memReqQ.first();
  endmethod
endmodule
