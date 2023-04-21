import BRAM::*;
import FIFO::*;
import SpecialFIFOs::*;
import MemTypes::*;
import Vector::*;
import Ehr::*;

interface Cache;
    method Action putFromProc(MainMemReq e);
    method ActionValue#(MainMemResp) getToProc();
    method Action putFromMem(MainMemResp e);
    method ActionValue#(MainMemReq) getToMem();
endinterface

module mkCache(Cache);

  FIFO#(MainMemResp) hitQ <- mkBypassFIFO;
  FIFO#(MainMemReq) memReqQ <- mkFIFO;
  FIFO#(MainMemResp) memRespQ <- mkFIFO;
  FIFO#(MainMemReq) stb <- mkSizedFIFO(1);

  Reg#(MainMemReq) missReq <- mkReg(?);
  Reg#(ReqStatus) mshr <- mkReg(Ready);

  BRAM_Configure cfg = defaultValue;
  BRAM1Port#(Bit#(7), MainMemResp) dataArray <- mkBRAM1Server(cfg);

  Vector#(128, Reg#(Maybe#(Bit#(19)))) tagArray <- replicateM(mkReg(tagged Invalid)); 
  Vector#(128, Reg#(Bit#(1))) dirtyArray <- replicateM(mkReg(0));

  Ehr#(2, Bool) lockL1 <- mkEhr(False); //lock to give the processor priority

  Bool debug = False;
  
  //STOREBUFFER RULES
  rule mvStbToL1 (mshr == Ready && !lockL1[1]);
    /*let the first element in stb be <addr,data>;
    stb.deq; 
    // move the oldest entry of stb into L1
        // may start allocation/evacuation
    ... get idx, tag and wOffset
    if (hit) then
      update dataArray and dirtyArray
    else  missReq <= r; 
          mshr <= StartMiss;*/ 
    let e = stb.first();
    stb.deq();
    if(debug) $display("mvStbToL1 %x", e.addr);

    Bit#(7) idx = truncate(e.addr);
    Bit#(19) tag = e.addr[25:7]; //check this

    if(fromMaybe(?, tagArray[idx]) == tag) begin
      //start a hit
      dataArray.portA.request.put(BRAMRequest{write: True, //True for write
                  responseOnWrite: False,
                  address: idx,
                  datain: e.data});
      dirtyArray[idx] <= 1;
    end else begin // we have a miss
      // check for data needed to write back
      let dirty_bit = dirtyArray[idx]; 
      if(isValid(tagArray[idx]) && dirty_bit == 1)begin
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
    /*extract idx from missing address; 
    extract tag from tagArray; 
    extract dirty bit from dirtyArrar;
    if tag is valid and line is dirty // write-back
    enque a store request for the dirty line in memReqQ; 
    mshr <= SendFillReq;*/
    Bit#(7) idx = truncate(missReq.addr); 
    let wb_tag = fromMaybe(?, tagArray[idx]); 
    let dirty_bit = dirtyArray[idx];
    if(debug) $display("startMiss %x", missReq.addr);

    if(isValid(tagArray[idx]) && dirty_bit == 1)begin
      let data <- dataArray.portA.response.get();
      memReqQ.enq(MainMemReq{write : 1, addr : {wb_tag, idx}, data : data });
    end 
    mshr <= SendFillReq;                      
  endrule
  
  rule sendFillReq (mshr == SendFillReq);
    if(debug) $display("sendFillReq %x", missReq.addr);
    memReqQ.enq(missReq);  
    mshr <= WaitFillResp;
  endrule

  rule waitFillResp(mshr == WaitFillResp);
    /*extract idx and tag from missReq.addr;
    let data = memRespQ.first;
    update tagArray at idx;
    if miss request is Ld then
      update dirtyArray and dataArray;
      enque appropriate word from data in hitQ; 
    else // miss request is St
      update the word in line;
      update dirtyArray and dataArray;
    memRespQ.deq; 
    mshr <= Ready;*/
    Bit#(7) idx = truncate(missReq.addr);
    Bit#(19) tag = missReq.addr[25:7];
    if(debug) $display("waitFillResp %x", missReq.addr);

    tagArray[idx] <= tagged Valid tag;

    //check of it's a load or a store
    if(missReq.write == 0) begin //it's a load
      let data = memRespQ.first();
      if(debug) $display("waitFillResp load %x", data);
      memRespQ.deq();
      dirtyArray[idx] <= 0;
      dataArray.portA.request.put(BRAMRequest{write: True, //True for write
            responseOnWrite: False,
            address: idx,
            datain: data});
      hitQ.enq(data);
      
    end else begin //its a store
      let data = missReq.data;
      dirtyArray[idx] <= 1; //this is dirty
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
    /*if(mshr == Ready);
    ... get idx, tag and wOffset
    if (request is a Ld) // search stb
    x = stb.search(r.addr); 
    if (x is valid) then enque x in hitQ;
    else // search L1
    if (hit) then
    enqueue the appropriate word in hitQ; 
    else  missReq <= r; mshr <= StartMiss;
    else // the request is a St
    enqueue <addr, data> in stb*/
    Bit#(7) idx = truncate(e.addr);
    Bit#(19) tag = e.addr[25:7]; //check this
    if(debug) $display("putFromProc %x %x", e.addr, e.write);

    if(e.write == 0) begin // check if it's a load
      lockL1[0] <= True; //processor requests get priority. 
      let stb_val = stb.first(); 

      if(stb_val.addr == e.addr) begin //check for stb hit
        hitQ.enq(stb_val.data);
        if(debug) $display("stb hitenq %x", stb_val.data);
      end else begin //check for l1 hit
        if(fromMaybe(?, tagArray[idx]) == tag) begin      
          //start a hit
          dataArray.portA.request.put(BRAMRequest{write: False, // False for read
                         responseOnWrite: False,
                         address: idx,
                         datain: ?});

          mshr <= GetHit;
        end else begin // we have a miss
          missReq <= e;
          let dirty_bit = dirtyArray[idx];

          // recall the data to be replaced in the cache if needed
          if(isValid(tagArray[idx]) && dirty_bit == 1)begin
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

  method ActionValue#(MainMemResp) getToProc();
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
