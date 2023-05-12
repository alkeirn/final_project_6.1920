import FIFO::*;
import SpecialFIFOs::*;
import RegFile::*;
import RVUtil::*;
import Vector::*;
import KonataHelper::*;
import Printf::*;
import Ehr::*;
import MemTypes::*;
import SupFifo::*;

//typedef struct { Bit#(4) byte_en; Bit#(32) addr; Bit#(32) data; } Mem deriving (Eq, FShow, Bits);

interface RVIfc;
    method ActionValue#(Mem) getIReq();
    method Action getIResp(CacheResp a);
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
                 Bit#(4) epoch; 
                 KonataId k_id; // <- This is a unique identifier per instructions, for logging purposes
             } F2D deriving (Eq, FShow, Bits);

typedef struct { 
    DecodedInst dInst;
    Bit#(32) pc;
    Bit#(32) ppc;
    Bit#(4) epoch;
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

typedef enum {
	Fetch, Decode, Execute, Writeback
} StateProc deriving (Eq, FShow, Bits);

(* synthesize *)
module mkpipelined(RVIfc);
    // Interface with memory and devices
    FIFO#(Mem) toImem <- mkBypassFIFO;
    SupFifo#(Bit#(32)) fromImem <- mkBypassSupFifo;
    FIFO#(Mem) toDmem <- mkBypassFIFO;
    FIFO#(Mem) fromDmem <- mkBypassFIFO;
    FIFO#(Mem) toMMIO <- mkBypassFIFO;
    FIFO#(Mem) fromMMIO <- mkBypassFIFO;

    SupFifo#(F2D) f2d <- mkSupFifo; //changed for superscalar to do 2 inst at once. 
    SupFifo#(D2E) d2e <- mkSupFifo;
    SupFifo#(E2W) e2w <- mkSupFifo;

    Ehr#(4, Bit#(32)) pc <- mkEhr(32'h0000000); 
    Vector#(32, Ehr#(4, Bit#(32))) rf <- replicateM(mkEhr(0)); 
    Vector#(32, Ehr#(6, Bit#(3))) scoreboard <- replicateM(mkEhr(0));
    Ehr#(3, Bit#(4)) epoch <- mkEhr(0);
    Ehr#(2, Bool) auth_decode <- mkEhr(False); //these are used to check that the first inst has been executed. 
    Ehr#(2, Bool) auth_execute <- mkEhr(False);
    Ehr#(2, Bool) auth_writeback <- mkEhr(False);

	// Code to support Konata visualization
    String dumpFile = "output.log" ;
    let lfh <- mkReg(InvalidFile);
	Reg#(KonataId) fresh_id <- mkReg(0);
	Reg#(KonataId) commit_id <- mkReg(0);

	SupFifo#(KonataId) retired <- mkSupFifo;
	SupFifo#(KonataId) squashed <- mkSupFifo;

    Ehr#(3, Bit#(2)) count <- mkEhr(0);

    Bool debug = True;  
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
        let current_pc = pc[1];
	    if(debug) $display("Fetch %x", current_pc);

        let req = Mem {byte_en : 0,
			   addr : current_pc,
			   data : 0};

        toImem.enq(req);

        //check for boundary conditions to enq 2 instructions *NEW*
        if (notLineBoundary(current_pc)) begin
           pc[1] <= current_pc + 8;
           let iid <- nfetchKonata(lfh, fresh_id, 0, 2);
           labelKonataLeft(lfh, iid, $format("PC : %x",current_pc));
           labelKonataLeft(lfh, iid + 1, $format("PC : %x",current_pc + 4));

            f2d.enq1(F2D{
                pc : current_pc, 
                ppc: current_pc + 4,
                epoch: epoch[2], 
                k_id: iid});

            f2d.enq2(F2D{
               pc: current_pc + 4, 
               ppc : current_pc + 8, 
               epoch: epoch[2],
               k_id: iid + 1});

            
        end else begin
            pc[1] <= current_pc + 4;
            let iid <- nfetchKonata(lfh, fresh_id, 0, 1);
            labelKonataLeft(lfh, iid, $format("PC : %x",current_pc));

            f2d.enq1(F2D{
                pc : current_pc, 
                ppc: current_pc + 4,
                epoch: epoch[2], 
                k_id: iid});
        end
    endrule

    rule decode1 if (!starting);
        let from_fetch = f2d.first1();
        let instr = fromImem.first1(); //changed to work with bit 32
       
        let decodedInst = decodeInst(instr);

        let rs1_idx = getInstFields(instr).rs1;
        let rs2_idx = getInstFields(instr).rs2;
        let rd_idx = getInstFields(instr).rd;

		let rs1 = (rs1_idx ==0 ? 0 : rf[rs1_idx][2]);
		let rs2 = (rs2_idx == 0 ? 0 : rf[rs2_idx][2]);

        if(scoreboard[rs1_idx][4] == 0 && scoreboard[rs2_idx][4] == 0 && scoreboard[rd_idx][4] == 0) begin
            if (debug) $display("[Decode] ", fshow(decodedInst));
        
            if(decodedInst.valid_rd) begin scoreboard[rd_idx][4] <= scoreboard[rd_idx][4] + 1; end //update scoreboard to include new destination register
            fromImem.deq1();
            f2d.deq1();
            auth_decode[0] <= True; //authorize second instruction

            d2e.enq1(D2E{ 
                dInst: decodedInst,
                pc : from_fetch.pc,
                ppc : from_fetch.ppc,
                epoch : from_fetch.epoch,
                rv1 : rs1, 
                rv2 : rs2, 
                k_id : from_fetch.k_id 
            });
            decodeKonata(lfh, from_fetch.k_id);
            labelKonataLeft(lfh,from_fetch.k_id, $format(" Instr bits: %x",decodedInst.inst));
            labelKonataLeft(lfh,from_fetch.k_id, $format(" Potential r1: %x, Potential r2: %x" , rs1, rs2));
        end
        else begin
            if (debug) $display("[Decode STALL 1] %x %x", scoreboard[rs1_idx][4], scoreboard[rs2_idx][4], fshow(decodedInst)); 
        end
    endrule

    rule decode2 if (!starting && auth_decode[1]);
        let from_fetch = f2d.first2();
        let instr = fromImem.first2(); //changed to work with bit 32
       
        let decodedInst = decodeInst(instr);

        let rs1_idx = getInstFields(instr).rs1;
        let rs2_idx = getInstFields(instr).rs2;
        let rd_idx = getInstFields(instr).rd;

		let rs1 = (rs1_idx ==0 ? 0 : rf[rs1_idx][3]);
		let rs2 = (rs2_idx == 0 ? 0 : rf[rs2_idx][3]);

        if(scoreboard[rs1_idx][5] == 0 && scoreboard[rs2_idx][5] == 0 && scoreboard[rd_idx][5] == 0) begin
            if (debug) $display("[Decode] ", fshow(decodedInst));
        
            if(decodedInst.valid_rd) begin scoreboard[rd_idx][5] <= scoreboard[rd_idx][5] + 1; end //update scoreboard to include new destination register
            fromImem.deq2();
            f2d.deq2();

            d2e.enq2(D2E{ 
                dInst: decodedInst,
                pc : from_fetch.pc,
                ppc : from_fetch.ppc,
                epoch : from_fetch.epoch,
                rv1 : rs1, 
                rv2 : rs2, 
                k_id : from_fetch.k_id 
            });
            decodeKonata(lfh, from_fetch.k_id);
            labelKonataLeft(lfh,from_fetch.k_id, $format(" Instr bits: %x",decodedInst.inst));
            labelKonataLeft(lfh,from_fetch.k_id, $format(" Potential r1: %x, Potential r2: %x" , rs1, rs2));
        end
        else begin
            if (debug) $display("[Decode STALL 2] %x %x", scoreboard[rs1_idx][5], scoreboard[rs2_idx][5], fshow(decodedInst)); 
        end
    endrule

    rule execute1 if (!starting);
        let from_decode = d2e.first1();
        d2e.deq1();
        if (debug) $display("[Execute 1] ", fshow(from_decode.dInst));

		executeKonata(lfh, from_decode.k_id);

		let imm = getImmediate(from_decode.dInst);
		Bool mmio = False;
		let data = execALU32(from_decode.dInst.inst, from_decode.rv1, from_decode.rv2, imm, from_decode.pc);
		let isUnsigned = 0;
		let funct3 = getInstFields(from_decode.dInst.inst).funct3;
		let size = funct3[1:0];
		let addr = from_decode.rv1 + imm;
		Bit#(2) offset = addr[1:0];

        let controlResult = execControl32(from_decode.dInst.inst, from_decode.rv1, from_decode.rv2, imm, from_decode.pc);
		let nextPc = controlResult.nextPC;
        let current_epoch = epoch[0];

        if(current_epoch == from_decode.epoch) begin
            auth_execute[0] <= True;

            if(nextPc != from_decode.ppc) begin //check if branch has occurred
                epoch[0] <= current_epoch + 1; //update the epoch
                if (debug) $display(" [Execute 1] Next pc != ppc %x %x", nextPc, from_decode.ppc);
                pc[0] <= nextPc;
            end

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
                    if (debug) $display("[Execute 1] MMIO", fshow(req));
                    toMMIO.enq(req);
                    labelKonataLeft(lfh,from_decode.k_id, $format(" MMIO ", fshow(req)));
                    mmio = True;
                end else begin 
                    if (debug) $display("[Execute 1] Emit Dmem ", fshow(req));
                    labelKonataLeft(lfh,from_decode.k_id, $format(" MEM ", fshow(req)));
                    toDmem.enq(req);
                end
            end
            else if (isControlInst(from_decode.dInst)) begin
                    labelKonataLeft(lfh,from_decode.k_id, $format(" Ctrl instr "));
                    data = from_decode.pc + 4;
                    if(debug) $display("control");
            end else begin 
                labelKonataLeft(lfh,from_decode.k_id, $format(" Standard instr "));
            end

            labelKonataLeft(lfh,from_decode.k_id, $format(" ALU output: %x" , data));

            e2w.enq1(E2W{
                mem_business : MemBusiness { isUnsigned : unpack(isUnsigned), size : size, offset : offset, mmio: mmio},
                data : data,
                dInst: from_decode.dInst,
                k_id : from_decode.k_id // <- This is a unique identifier per instructions, for logging purposes
            });

        end else begin
            squashed.enq1(from_decode.k_id); //need to modify for Konata
            labelKonataLeft(lfh,from_decode.k_id, $format(" epoch: %x, curepoch: %x ", from_decode.epoch, current_epoch));

            if(from_decode.dInst.valid_rd) begin //remove from scoreboard
                let fields = getInstFields(from_decode.dInst.inst); 
                scoreboard[fields.rd][0] <= scoreboard[fields.rd][0] - 1;    
            end
        end
    endrule

    rule printexecute if (!starting);
        let toWrite = e2w.first1();
        $display("[Execute PRINT] ", fshow(toWrite));
    endrule

    rule execute2 if (!starting && auth_execute[1]);
        let from_decode = d2e.first2();

        if (!isMemoryInst(from_decode.dInst)) begin
            d2e.deq2();
            if (debug) $display("[Execute 2] ", fshow(from_decode.dInst));

            executeKonata(lfh, from_decode.k_id);

            let imm = getImmediate(from_decode.dInst);
            Bool mmio = False;
            let data = execALU32(from_decode.dInst.inst, from_decode.rv1, from_decode.rv2, imm, from_decode.pc);
            let isUnsigned = 0;
            let funct3 = getInstFields(from_decode.dInst.inst).funct3;
            let size = funct3[1:0];
            let addr = from_decode.rv1 + imm;
            Bit#(2) offset = addr[1:0];

            let controlResult = execControl32(from_decode.dInst.inst, from_decode.rv1, from_decode.rv2, imm, from_decode.pc);
            let nextPc = controlResult.nextPC;
            let current_epoch = epoch[1]; 

            if(current_epoch == from_decode.epoch) begin
                if(nextPc != from_decode.ppc) begin //check if branch has occurred
                    epoch[1] <= current_epoch + 1; //update the epoch
                    if (debug) $display("Next pc != ppc %x %x", nextPc, from_decode.ppc);
                    pc[1] <= nextPc;
                end

                if (isControlInst(from_decode.dInst)) begin
                        labelKonataLeft(lfh,from_decode.k_id, $format(" Ctrl instr "));
                        data = from_decode.pc + 4;
                        if(debug) $display("control");
                end else begin 
                    labelKonataLeft(lfh,from_decode.k_id, $format(" Standard instr "));
                end

                labelKonataLeft(lfh,from_decode.k_id, $format(" ALU output: %x" , data));

                e2w.enq2(E2W{
                    mem_business : MemBusiness { isUnsigned : unpack(isUnsigned), size : size, offset : offset, mmio: mmio},
                    data : data,
                    dInst: from_decode.dInst,
                    k_id : from_decode.k_id // <- This is a unique identifier per instructions, for logging purposes
                });
                if (debug) $display("[Execute 2] enque ", fshow(from_decode));

            end else begin
                squashed.enq2(from_decode.k_id); 
                labelKonataLeft(lfh,from_decode.k_id, $format(" epoch: %x, curepoch: %x ", from_decode.epoch, current_epoch));

                if(from_decode.dInst.valid_rd) begin //remove from scoreboard
                    let fields = getInstFields(from_decode.dInst.inst); 
                    scoreboard[fields.rd][1] <= scoreboard[fields.rd][1] - 1;    
                end
            end
        end
    endrule
    

    rule writeback1 if (!starting);
        let from_execute = e2w.first1();
        e2w.deq1();
        auth_writeback[0] <= True;
        if (debug) $display("[Writeback 1] ", fshow(from_execute));

		writebackKonata(lfh,from_execute.k_id); 
        retired.enq1(from_execute.k_id);

        let fields = getInstFields(from_execute.dInst.inst);
        

        if (isMemoryInst(from_execute.dInst)) begin // (* // write_val *)
            let resp = ?;
		    if (from_execute.mem_business.mmio) begin 
                resp = fromMMIO.first();
		        fromMMIO.deq();
		    end else if (!(from_execute.dInst.inst[5] == 1)) begin
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

        if (!from_execute.dInst.legal) begin
			if (debug) $display("[Writeback 1] Illegal Inst, Drop and fault: ", fshow(from_execute.dInst));
			pc[2] <= 0;	// Fault
	    end
		if (from_execute.dInst.valid_rd) begin
            let rd_idx = fields.rd;
            scoreboard[rd_idx][2] <= scoreboard[rd_idx][2] - 1;

            if (rd_idx != 0) begin
                 rf[rd_idx][0] <= from_execute.data; 
            end
		end
	endrule
    
    rule writeback2 if (!starting && auth_writeback[1]);
        let from_execute = e2w.first2();
        if (!isMemoryInst(from_execute.dInst)) begin
            e2w.deq2();
            if (debug) $display("[Writeback 2] ", fshow(from_execute));

            writebackKonata(lfh,from_execute.k_id); 
            retired.enq2(from_execute.k_id);

            let fields = getInstFields(from_execute.dInst.inst);
            
            if (!from_execute.dInst.legal) begin
                if (debug) $display("[Writeback 2] Illegal Inst, Drop and fault: ", fshow(from_execute.dInst));
                pc[3] <= 0;	// Fault
            end
            if (from_execute.dInst.valid_rd) begin
                let rd_idx = fields.rd;
                scoreboard[rd_idx][3] <= scoreboard[rd_idx][3] - 1;

                if (rd_idx != 0) begin
                    rf[rd_idx][1] <= from_execute.data; 
                end
            end
        end
	endrule
	

    rule reset_authorization;
        auth_decode[1] <= False;
        auth_execute[1] <= False;
        auth_writeback[1] <= False;
    endrule

	// ADMINISTRATION:
    rule administrative_konata_commit1;
		    retired.deq1();
		    let f = retired.first1();
		    unsafeCommitKonata(lfh, f, commit_id);
            count[0] <= 1;
	endrule

    rule administrative_konata_commit2;
		    retired.deq2();
		    let f = retired.first2();
		    unsafeCommitKonata(lfh, f, commit_id);
            count[1] <= count[1] + 1;
	endrule

    rule final_commit;
        count[2] <= 0; // reset the count of commits per cycle
        commit_id <= commit_id + zeroExtend(count[2]);
    endrule
		
	rule administrative_konata_flush1;
		    squashed.deq1();
		    let f = squashed.first1();
		    squashKonata(lfh, f);
	endrule

    rule administrative_konata_flush2;
		    squashed.deq2();
		    let f = squashed.first2();
		    squashKonata(lfh, f);
	endrule
		
    method ActionValue#(Mem) getIReq();
		toImem.deq();
		return toImem.first();
    endmethod
    method Action getIResp(CacheResp a);
        //split up the first and second inst responses. 
    	fromImem.enq1(a.first); 
        if(a.second_valid) fromImem.enq2(a.second);
    endmethod
    method ActionValue#(Mem) getDReq();
        if (debug) $display("[Execute] Dmem method");
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

//fuction to check if the requested instruction is on a line boundary in the cache. 
// If it is, we will only get one instruction returned by the cache. 
function Bool notLineBoundary(Bit#(32) pc);
    Bit#(4) offset = pc[5:2];
    return (offset != 4'b1111);
endfunction