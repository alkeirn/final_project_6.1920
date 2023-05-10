import RVUtil::*;
import BRAM::*;
import pipelined::*;
import FIFO::*;
import Cache::*;
import MemTypes::*;
import MainMem::*;
import Vector::*;
typedef Bit#(32) Word;
typedef Bit#(512) Line;

module mktop_pipelined(Empty);
    // Instantiate the dual ported memory
    BRAM_Configure cfg = defaultValue();
    //keilee: changed BRAM to read from memlines.vmh instead of mem.vmh
    cfg.loadFormat = tagged Hex "memlines.vmh";
    //keilee: changed BRAM to handle Lines instead of Words
    //BRAM2PortBE#(Bit#(30), Line, 4) bram <- mkBRAM2ServerBE(cfg);
    BRAM1PortBE#(Bit#(12), Vector#(16, Bit#(32)), 64) bram <- mkBRAM1ServerBE(cfg);
    //keilee: IMem and Dmem Caches and FIFO to keep up with them
    Cache iCache <- mkCache;
    Cache dCache <- mkCache;
    FIFO#(Bit#(1)) cacheQueue <- mkFIFO;

    //keilee: MainMem from Beveren.bsv
    MainMem mainMem <- mkMainMem();

    RVIfc rv_core <- mkpipelined;
    Reg#(Mem) ireq <- mkRegU;
    Reg#(Mem) dreq <- mkRegU;
    FIFO#(Mem) mmioreq <- mkFIFO;
    let debug = True;
    Reg#(Bit#(32)) cycle_count <- mkReg(0);

    rule tic;
	    cycle_count <= cycle_count + 1;
    endrule

    //keilee: change I and D requests and responses for Caches

    rule requestI;
        let req <- rv_core.getIReq;
        if (debug) $display("Get IReq", fshow(req));
        ireq <= req;
        iCache.putFromProc(req);
            // bram.portB.request.put(BRAMRequestBE{
            //         writeen: req.byte_en,
            //         responseOnWrite: True,
            //         address: truncate(req.addr >> 2),
            //         datain: req.data});
    endrule

    rule responseI;
        //let x <- bram.portB.response.get();
        let x <- iCache.getToProc;
        let req = ireq;
        if (debug) $display("Get IResp ", fshow(req), fshow(x));
        req.data = x.first;
        rv_core.getIResp(req);
    endrule

    rule requestD;
        let req <- rv_core.getDReq;
        dreq <= req;
        if (debug) $display("Get DReq ", fshow(req));
        dCache.putFromProc(req);
        // bram.portA.request.put(BRAMRequestBE{
        //   writeen: req.byte_en,
        //   responseOnWrite: True,
        //   address: truncate(req.addr >> 2),
        //   datain: req.data});
    endrule

    rule responseD;
        //let x <- bram.portA.response.get();
        let x <- dCache.getToProc;
        let req = dreq;
        if (debug) $display("Get DResp ", fshow(req), fshow(x));
        req.data = x.first;
        rv_core.getDResp(req);
    endrule

    //keilee: rules for Caches to MainMem interface including queue tracking 

    rule iToMemReq;
        let iReq <- iCache.getToMem();
        mainMem.put(iReq);
        cacheQueue.enq(0);
    endrule

    rule iFromMemResp
        if (cacheQueue.first() == 0); 
            let resp <- mainMem.get;
            iCache.putFromMem(resp);
            cacheQueue.deq();
    endrule

    rule dToMemReq;
        if (debug) $display("dToMemReq");
        let dReq <- dCache.getToMem();
        mainMem.put(dReq);
        cacheQueue.enq(1);
    endrule

    rule dFromMemResp
       if (cacheQueue.first() == 1); 
            if (debug) $display("dFromMemReq");
            let resp <- mainMem.get;
            dCache.putFromMem(resp);
            cacheQueue.deq();   
    endrule
  
    rule requestMMIO;
        let req <- rv_core.getMMIOReq;
        if (debug) $display("Get MMIOReq", fshow(req));
        if (req.byte_en == 'hf) begin
            if (req.addr == 'hf000_fff4) begin
                // Write integer to STDERR
                        $fwrite(stderr, "%0d", req.data);
                        $fflush(stderr);
            end
        end
        if (req.addr ==  'hf000_fff0) begin
                // Writing to STDERR
                $fwrite(stderr, "%c", req.data[7:0]);
                $fflush(stderr);
        end else
            if (req.addr == 'hf000_fff8) begin
            // Exiting Simulation
                if (req.data == 0) begin
                        $fdisplay(stderr, "  [0;32mPASS[0m");
                end
                else
                    begin
                        $fdisplay(stderr, "  [0;31mFAIL[0m (%0d)", req.data);
                    end
                $fflush(stderr);
                $finish;
            end

        mmioreq.enq(req);
    endrule

    rule responseMMIO;
        let req = mmioreq.first();
        mmioreq.deq();
        if (debug) $display("Put MMIOResp", fshow(req));
        rv_core.getMMIOResp(req);
    endrule
    
endmodule
