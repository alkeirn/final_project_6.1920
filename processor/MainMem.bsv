import RVUtil::*;
import BRAM::*;
import FIFO::*;
import SpecialFIFOs::*;
import DelayLine::*;
import MemTypes::*;
import Cache::*;

interface MainMem;
    method Action put(MainMemReq req);
    method ActionValue#(MainMemResp) get();
endinterface

interface MemRef;
    method Action put(Mem req);
    method ActionValue#(CacheResp) get();
endinterface

(* synthesize *)
module mkMainMemFast(MemRef);
    BRAM_Configure cfg = defaultValue();
    //keilee: changed BRAM to read from memlines.vmh instead of mem.vmh
    cfg.loadFormat = tagged Hex "memlines.vmh";
    BRAM2Port#(LineAddr, Bit#(32)) bram <- mkBRAM2Server(cfg); 
    //keilee: changed BRAM to handle Lines instead of Words
    //BRAM2PortBE#(Bit#(30), Line, 4) bram <- mkBRAM2ServerBE(cfg);
    //BRAM1PortBE#(Bit#(12), Vector#(16, Bit#(32)), 64) bram <- mkBRAM1ServerBE(cfg);

    method Action put(Mem req);
        bram.portA.request.put(BRAMRequest{
                    write: req.byte_en != 0,
                    responseOnWrite: False, //this needs to be true for processor
                    address: req.addr,
                    datain: req.data});
                    
        if (req.byte_en == 0) begin
            bram.portB.request.put(BRAMRequest{
                        write: False,
                        responseOnWrite: False,
                        address: req.addr + 4,
                        datain: req.data});
        end
    endmethod

    method ActionValue#(CacheResp) get();
        let r1 <- bram.portA.response.get();
        let r2 <- bram.portB.response.get();
        return CacheResp{first : r1, second : r2, second_valid : True};
    endmethod
endmodule

(* synthesize *)
module mkMainMem(MainMem);
    BRAM_Configure cfg = defaultValue();
    cfg.loadFormat = tagged Hex "memlines.vmh";
    BRAM1Port#(LineAddr, Bit#(512)) bram <- mkBRAM1Server(cfg);
    DelayLine#(40, MainMemResp) dl <- mkDL(); // Delay by 20 cycles

    rule deq;
        let r <- bram.portA.response.get();
        dl.put(r);
    endrule    

    method Action put(MainMemReq req);
        bram.portA.request.put(BRAMRequest{
                    write: unpack(req.write),
                    responseOnWrite: False,
                    address: req.addr,
                    datain: req.data});
    endmethod

    method ActionValue#(MainMemResp) get();
        let r <- dl.get();
        return r;
    endmethod
endmodule

