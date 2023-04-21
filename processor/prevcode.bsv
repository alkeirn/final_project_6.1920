import FIFO::*;
import SpecialFIFOs::*;
import RegFile::*;
import RVUtil::*;
import Vector::*;
import KonataHelper::*;
import Printf::*;
import Ehr::*;

typedef struct { Bit#(4) byte_en; Bit#(32) addr; Bit#(32) data; } Mem deriving (Eq, FShow, Bits);

interface RVIfc;
    method ActionValue#(Mem) getIReq();
    method Action getIResp(Mem a);
    method ActionValue#(Mem) getDReq();
    method Action getDResp(Mem a);
    method ActionValue#(Mem) getMMIOReq();
    method Action getMMIOResp(Mem a);
endinterface
typedef struct { Bool isUnsigned; Bit#(2) size; Bit#(2) offset; Bool mmio; } MemBusiness deriving (Eq, FShow, Bits);

function Bool isMMIO(Bit#(32) addr);
    Bool x = case (addr) 
        32'hf000fff0: True;
        32'hf000fff4: True;
        32'hf000fff8: True;
        default: False;
    endcase;
    return x;
endfunction

typedef struct { Bit#(32) pc;
                 Bit#(32) ppc;
                 Bit#(2) epoch; 
                 KonataId k_id; // <- This is a unique identifier per instructions, for logging purposes
             } F2D deriving (Eq, FShow, Bits);

typedef struct { 
    DecodedInst dInst;
    Bit#(32) pc;
    Bit#(32) ppc;
    Bit#(2) epoch;
    Bit#(32) rv1; 
    Bit#(32) rv2; 
    KonataId k_id; // <- This is a unique identifier per instructions, for logging purposes
    } D2E deriving (Eq, FShow, Bits);

typedef struct { 
    MemBusiness mem_business;
    Bit#(32) data;
    DecodedInst dInst;
    KonataId k_id; // <- This is a unique identifier per instructions, for logging purposes
} E2W deriving (Eq, FShow, Bits);

(* synthesize *)
module mkpipelined(RVIfc);
    // Interface with memory and devices
    FIFO#(Mem) toImem <- mkBypassFIFO;
    FIFO#(Mem) fromImem <- mkBypassFIFO;
    FIFO#(Mem) toDmem <- mkBypassFIFO;
    FIFO#(Mem) fromDmem <- mkBypassFIFO;
    FIFO#(Mem) toMMIO <- mkBypassFIFO;
    FIFO#(Mem) fromMMIO <- mkBypassFIFO;

    FIFO#(F2D) f2d <- mkBypassFIFO;
    FIFO#(D2E) d2e <- mkBypassFIFO;
    FIFO#(E2W) e2w <- mkBypassFIFO;


	// Code to support Konata visualization
    String dumpFile = "output.log" ;
    let lfh <- mkReg(InvalidFile);
	Reg#(KonataId) fresh_id <- mkReg(0);
	Reg#(KonataId) commit_id <- mkReg(0);

	FIFO#(KonataId) retired <- mkFIFO;
	FIFO#(KonataId) squashed <- mkFIFO;

    Reg#(Bit#(32)) ppc <- mkReg(32'h0000000);
    Vector#(32, Ehr#(2, Bit#(32))) rf <- replicateM(mkEhr(0));

    Ehr#(2, MemBusiness) mem_business <- mkEhr(?);

    //pipelining variables
    Ehr#(2, Bit#(32)) pc <- mkEhr(0);
    Vector#(32, Ehr#(2, Bit#(1))) scoreboard <- replicateM(mkEhr(0));
    Ehr#(2, Bit#(2)) epoch <- mkEhr(0);
    Ehr#(2, Bit#(32)) rvd <- mkEhr(0); //destination register

    Bool debug = False;
    Reg#(Bool) starting <- mkReg(True);
	rule do_tic_logging;
        if (starting) begin
            let f <- $fopen(dumpFile, "w") ;
            lfh <= f;
            $fwrite(f, "Kanata\t0004\nC=\t1\n");
            starting <= False;
        end
		konataTic(lfh);
	endrule
		
    rule fetch if (!starting);
        Bit#(32) pc_fetched = pc[1];

        if(ppc != pc_fetched) begin
            //misprediction, go to new pc
            ppc <= pc_fetched;
        end else begin
            ppc <= ppc + 4;
        end

        // You should put the pc that you fetch in pc_fetched
        // Below is the code to support Konata's visualization
        let iid <- fetch1Konata(lfh, fresh_id, 0);
        labelKonataLeft(lfh, iid, $format("PC %x",pc_fetched));

        let req = Mem {byte_en : 0,
            addr : ppc,
            data : 0};
        toImem.enq(req);   
        
        f2d.enq(F2D{
            pc : pc_fetched, //not sure about this
            ppc: ppc,
            epoch: epoch[1],
            k_id: iid});
        // iid is the unique identifier used by konata, that we will pass around everywhere for each instruction
    endrule

    rule decode if (!starting);
        let resp = fromImem.first();
        let instr = resp.data;
        let decodedInst = decodeInst(instr);

        let rs1_idx = getInstFields(instr).rs1;
        let rs2_idx = getInstFields(instr).rs2;
        let rd_idx = getInstFields(instr).rd;

        // check the score board and update for next stage
        if(scoreboard[rs1_idx][1] == 0 && scoreboard[rs2_idx][1] == 0) begin
            scoreboard[rd_idx][1] <= 1;

            let rs1 = (rs1_idx ==0 ? 0 : rf[rs1_idx][1]);
            let rs2 = (rs2_idx == 0 ? 0 : rf[rs2_idx][1]);

            let dInst = decodedInst; 

            // To add a decode event in Konata you will likely do something like:
            let from_fetch = f2d.first();
            f2d.deq();
            decodeKonata(lfh, from_fetch.k_id);
            labelKonataLeft(lfh,from_fetch.k_id, $format("Instr bits: %x",decodedInst.inst));

            d2e.enq(D2E{ 
                dInst: dInst,
                pc : from_fetch.pc,
                ppc : from_fetch.ppc,
                epoch : from_fetch.epoch,
                rv1 : rs1, 
                rv2 : rs2, 
                k_id : from_fetch.k_id 
            });

            fromImem.deq(); //deque the instruction              
        end 
    endrule

    rule execute if (!starting);
        let from_decode = d2e.first();
        d2e.deq();

        if (debug) $display("[Execute] ", fshow(from_decode.dInst));
		executeKonata(lfh, from_decode.k_id);

		let imm = getImmediate(from_decode.dInst);
		Bool mmio = False;
		let data = execALU32(from_decode.dInst.inst, from_decode.rv1, from_decode.rv2, imm, from_decode.pc);
		let isUnsigned = 0;
		let funct3 = getInstFields(from_decode.dInst.inst).funct3;
		let size = funct3[1:0];
		let addr = from_decode.rv1 + imm;
		Bit#(2) offset = addr[1:0];

        if(epoch[0] != from_decode.epoch) begin
            //differing epoch means a misprediction
            squashed.enq(from_decode.k_id);

        end else begin
            executeKonata(lfh, from_decode.k_id);

            if (isMemoryInst(from_decode.dInst)) begin
                // Technical details for load byte/halfword/word
                let shift_amount = {offset, 3'b0};
                let byte_en = 0;

                case (size) matches
                    2'b00: byte_en = 4'b0001 << offset;
                    2'b01: byte_en = 4'b0011 << offset;
                    2'b10: byte_en = 4'b1111 << offset;
                endcase

                data = from_decode.rv2 << shift_amount;
                addr = {addr[31:2], 2'b0};
                isUnsigned = funct3[2];
                let type_mem = (from_decode.dInst.inst[5] == 1) ? byte_en : 0;
                let req = Mem {byte_en : type_mem,
                        addr : addr,
                        data : data};

                if (isMMIO(addr)) begin 
                    if (debug) $display("[Execute] MMIO", fshow(req));
                    toMMIO.enq(req);
                    labelKonataLeft(lfh,from_decode.k_id, $format(" MMIO ", fshow(req)));
                    mmio = True;

                end else begin 
                    labelKonataLeft(lfh,from_decode.k_id, $format(" MEM ", fshow(req)));
                    toDmem.enq(req);
                end
            end
            else if (isControlInst(from_decode.dInst)) begin
                labelKonataLeft(lfh,from_decode.k_id, $format(" Ctrl instr "));
                data = from_decode.pc + 4;
                epoch[0] <= epoch[0] + 1; // update epoch
            end else begin 
                labelKonataLeft(lfh,from_decode.k_id, $format(" Standard instr "));
            end

            let controlResult = execControl32(from_decode.dInst.inst, from_decode.rv1, from_decode.rv2, imm, from_decode.pc);
            let nextPc = controlResult.nextPC;
            pc[0] <= nextPc;
 
            let mem_business = MemBusiness { isUnsigned : unpack(isUnsigned), size : size, offset : offset, mmio: mmio};

            e2w.enq(E2W{
                mem_business : mem_business,
                data : data,
                dInst: from_decode.dInst,
                k_id : from_decode.k_id // <- This is a unique identifier per instructions, for logging purposes
            });            
        end
        // Similarly, to register an execute event for an instruction:
    	//	executeKonata(lfh, k_id);
    	// where k_id is the unique konata identifier that has been passed around that came from the fetch stage


    	// Execute is also the place where we advise you to kill mispredicted instructions
    	// (instead of Decode + Execute like in the class)
    	// When you kill (or squash) an instruction, you should register an event for Konata:
    	
        // squashed.enq(current_inst.k_id);

        // This will allow Konata to display those instructions in grey
    endrule

    rule writeback if (!starting);
        let from_execute = e2w.first();
        e2w.deq();
        
        let fields = getInstFields(from_execute.dInst.inst);

        if (isMemoryInst(from_execute.dInst)) begin // (* // write_val *)
            let resp = ?;

		    if (from_execute.mem_business.mmio) begin 
                resp = fromMMIO.first();
		        fromMMIO.deq();
		    end else begin 
                resp = fromDmem.first();
		        fromDmem.deq();
		    end

            let mem_data = resp.data;
            mem_data = mem_data >> {from_execute.mem_business.offset ,3'b0};

            case ({pack(from_execute.mem_business.isUnsigned), from_execute.mem_business.size}) matches
	     	    3'b000 : from_execute.data = signExtend(mem_data[7:0]);
	     	    3'b001 : from_execute.data = signExtend(mem_data[15:0]);
	     	    3'b100 : from_execute.data = zeroExtend(mem_data[7:0]);
	     	    3'b101 : from_execute.data = zeroExtend(mem_data[15:0]);
	     	    3'b010 : from_execute.data = mem_data;
             endcase
		end

		if(debug) $display("[Writeback]", fshow(from_execute.dInst));
        
        if (!from_execute.dInst.legal) begin
			if (debug) $display("[Writeback] Illegal Inst, Drop and fault: ", fshow(from_execute.dInst));
			pc[1] <= 0;	// Fault
	    end

		if (from_execute.dInst.valid_rd) begin
            let rd_idx = fields.rd;
            if (rd_idx != 0) begin rf[rd_idx][0] <= from_execute.data; end
            scoreboard[rd_idx][0] <= 0; //update scoreboard
		end

        // Similarly, to register an execute event for an instruction:
	    writebackKonata(lfh,from_execute.k_id);


	   	// In writeback is also the moment where an instruction retires (there are no more stages)
	   	// Konata requires us to register the event as well using the following: 
		retired.enq(from_execute.k_id);
	endrule
		

	// ADMINISTRATION:

    rule administrative_konata_commit;
		    retired.deq();
		    let f = retired.first();
		    commitKonata(lfh, f, commit_id);
	endrule
		
	rule administrative_konata_flush;
		    squashed.deq();
		    let f = squashed.first();
		    squashKonata(lfh, f);
	endrule
		
    method ActionValue#(Mem) getIReq();
		toImem.deq();
		return toImem.first();
    endmethod
    method Action getIResp(Mem a);
    	fromImem.enq(a);
    endmethod
    method ActionValue#(Mem) getDReq();
		toDmem.deq();
		return toDmem.first();
    endmethod
    method Action getDResp(Mem a);
		fromDmem.enq(a);
    endmethod
    method ActionValue#(Mem) getMMIOReq();
		toMMIO.deq();
		return toMMIO.first();
    endmethod
    method Action getMMIOResp(Mem a);
		fromMMIO.enq(a);
    endmethod
endmodule