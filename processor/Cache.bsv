import BRAM::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import MemTypes::*;
import Vector::*;
import Ehr::*;

interface Cache;
    method Action putFromProc(Mem e);
    method ActionValue#(CacheResp) getToProc();
    method Action putFromMem(MainMemResp e);
    method ActionValue#(MainMemReq) getToMem();
endinterface
(* synthesize *)
module mkCache(Cache);

  FIFO#(CacheResp) hitQ <- mkBypassFIFO;
  FIFO#(MainMemReq) memReqQ <- mkFIFO;
  FIFO#(MainMemResp) memRespQ <- mkFIFO;
  FIFOF#(Mem) stb <- mkSizedFIFOF(1);

  Reg#(Mem) missReq <- mkReg(?);
  Reg#(Mem) hitReq <- mkReg(?);
  Reg#(ReqStatus) mshr <- mkReg(Ready);

  BRAM_Configure cfg = defaultValue;
  BRAM1PortBE#(Bit#(7), Vector#(16, Bit#(32)), 64) dataArray <- mkBRAM1ServerBE(cfg); //Fix this, makes an error

  Vector#(128, Reg#(Maybe#(Bit#(19)))) tagArray <- replicateM(mkReg(tagged Invalid)); 
  Vector#(128, Reg#(Bit#(1))) dirtyArray <- replicateM(mkReg(0));

  Ehr#(2, Bool) lockL1 <- mkEhr(False); //lock to give the processor priority

  Bool debug = True;
  
  //STOREBUFFER RULES
  rule mvStbToL1 (mshr == Ready && !lockL1[1]); 
    let e = stb.first();
    stb.deq();
    if(debug) $display("mvStbToL1 %x", e.addr);

    Bit#(4) offset = e.addr[5:2]; //check this
    Bit#(7) idx = e.addr[12:6];
    Bit#(19) tag = e.addr[31:13]; 
    Bit#(64) byteAddr = getByteAddr(offset);

    if(fromMaybe(tag + 1, tagArray[idx]) == tag) begin
      //start a hit
      Vector#(16, Bit#(32)) vdata_aux = replicate(32'b0);
      let data = getVectorData(offset, vdata_aux, e.data);
      dataArray.portA.request.put(BRAMRequestBE{
          writeen: byteAddr,
          responseOnWrite: False,
          address: idx,
          datain: data}); 

      dirtyArray[idx] <= 1;

    end else begin // we have a miss
      // check for data needed to write back
      let dirty_bit = dirtyArray[idx]; 
      if(isValid(tagArray[idx]) && dirty_bit == 1)begin

        dataArray.portA.request.put(BRAMRequestBE{
            writeen: 0,                       // 0 for a read
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
    Bit#(4) offset = missReq.addr[5:2];
    let wb_tag = fromMaybe(?, tagArray[idx]); 
    let dirty_bit = dirtyArray[idx];
    if(debug) $display("startMiss %x", missReq.addr);

    if(isValid(tagArray[idx]) && dirty_bit == 1)begin
      let data <- dataArray.portA.response.get();
      memReqQ.enq(MainMemReq{write : 1, addr : {0, wb_tag, idx}, data : pack(data) });
    end 
    mshr <= SendFillReq;                      
  endrule
  
  rule sendFillReq (mshr == SendFillReq);
    if(debug) $display("sendFillReq %x", missReq.addr);
    MainMemReq mainMissReq = getMainType(missReq);
    memReqQ.enq(mainMissReq);  
    mshr <= WaitFillResp;
  endrule

  rule waitFillResp(mshr == WaitFillResp);
    Bit#(4) offset = missReq.addr[5:2]; //check this
    Bit#(7) idx = missReq.addr[12:6];
    Bit#(19) tag = missReq.addr[31:13];
    Bit#(64) byteAddr = getByteAddr(offset); 
    if(debug) $display("waitFillResp %x", missReq.addr);

    tagArray[idx]<= tagged Valid tag;

    //check of it's a load or a store
    if(missReq.byte_en == 0) begin //it's a load
      let data = memRespQ.first();
      Vector#(16, Bit#(32)) vdata = unpack(data);
      if(debug) $display("waitFillResp load %x", data);
      dirtyArray[idx] <= 0;
      memRespQ.deq();

      dataArray.portA.request.put(BRAMRequestBE{
          writeen: 64'hFFFFFFFFFFFFFFFF,
          responseOnWrite: False,
          address: idx,
          datain: vdata});

      // check if we can do superscalar
      if(offset != 4'b1111) begin
        //can do multiple addresses
        hitQ.enq(CacheResp{
          first: vdata[offset],
          second: vdata[offset+1],
          second_valid: True}); 
      end else begin
        //addresses not in the cache
        hitQ.enq(CacheResp{
          first: vdata[offset],
          second: ?,
          second_valid: False}); 
      end
      
    end else begin //its a store
      let data = memRespQ.first();  
      Vector#(16, Bit#(32)) vdata_aux = unpack(data); 
      memRespQ.deq(); 
      let vdata = getVectorData(offset, vdata_aux, missReq.data);
      dirtyArray[idx] <= 1; //this is dirty
      if(debug) $display("waitFillResp store %x %x", missReq.data,  offset);

      dataArray.portA.request.put(BRAMRequestBE{
          writeen: 64'hFFFFFFFFFFFFFFFF,
          responseOnWrite: False,
          address: idx,
          datain: vdata});
    end
    mshr <= Ready;
  endrule

  rule getHit(mshr == GetHit);
    Bit#(4) offset = hitReq.addr[5:2]; //check this
    Bit#(7) idx = hitReq.addr[12:6];
    Bit#(19) tag = hitReq.addr[31:13];

    let hit_data <- dataArray.portA.response.get(); //get data from BRAM
    if(debug) $display("getHit hitenq %x", hit_data);

    // check if we can do superscalar
    if(offset != 4'b1111) begin
      //can do multiple addresses
      hitQ.enq(CacheResp{
        first: hit_data[offset],
        second: hit_data[offset+1],
        second_valid: True}); 
    end else begin
      //addresses not in the cache
      hitQ.enq(CacheResp{
        first: hit_data[offset],
        second: ?,
        second_valid: False}); 
    end

    mshr <= Ready;
  endrule

  //METHODS
  method Action putFromProc(Mem e) if(mshr == Ready);
    Bit#(4) offset = e.addr[5:2]; //check this
    Bit#(7) idx = e.addr[12:6];
    Bit#(19) tag = e.addr[31:13]; 
    if(debug) $display("putFromProc %x %x", e.addr, e.byte_en);

    if(e.byte_en == 0) begin // check if it's a load
      lockL1[0] <= True; //processor requests get priority. 
      Mem stb_val= unpack(0);
      Bool stb_valid = False;
      if (stb.notEmpty) begin 
        stb_val = stb.first(); 
        stb_valid = True;
      end
      if(stb_valid && stb_val.addr == e.addr) begin //check for stb hit
        hitQ.enq(CacheResp{
          first : stb_val.data,
          second : ?,
          second_valid : False});
        if(debug) $display("stb hitenq %x", stb_val.data);

      end else begin //check for l1 hit
        if(fromMaybe(?, tagArray[idx]) == tag) begin      
          //start a hit
          hitReq <= e;
          dataArray.portA.request.put(BRAMRequestBE{
              writeen: 0,
              responseOnWrite: False,
              address: idx,
              datain: ?});
      
          mshr <= GetHit;

        end else begin // we have a miss
          missReq <= e;
          let dirty_bit = dirtyArray[idx];

          // recall the data to be replaced in the cache if needed
          if(isValid(tagArray[idx]) && dirty_bit == 1)begin
            dataArray.portA.request.put(BRAMRequestBE{
                writeen: 0,
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

/*
 * This function converts offsets to their byte addressable bit counterparts
 */
function Bit#(64) getByteAddr(Bit#(4) offset);
    Bit#(6) offset_extended = zeroExtend(offset) << 2; //multiply by four, and pad to prevent rollover. 
    Bit#(64) byteAddr = {0, 4'b1111};
    byteAddr = byteAddr << offset_extended;
    return byteAddr;
endfunction

//This function creates a vector wrapper for the data
function Vector#(16, Bit#(32)) getVectorData(Bit#(4) offset, Vector#(16, Bit#(32)) fromMem, Bit#(32) data);
  Vector#(16, Bit#(32)) vectorData = fromMem;
  vectorData[offset] = data;
  return vectorData;
endfunction

function MainMemReq getMainType(Mem req);
    MainMemReq mainReq;
    mainReq.addr = req.addr >> 6;
    mainReq.data = ?;
    mainReq.write = 0;
    return mainReq;
endfunction
