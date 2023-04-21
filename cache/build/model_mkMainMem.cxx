/*
 * Generated by Bluespec Compiler, version 2023.01-6-g034050db (build 034050db)
 * 
 * On Fri Mar 24 16:43:22 EDT 2023
 * 
 */
#include "bluesim_primitives.h"
#include "model_mkMainMem.h"

#include <cstdlib>
#include <time.h>
#include "bluesim_kernel_api.h"
#include "bs_vcd.h"
#include "bs_reset.h"


/* Constructor */
MODEL_mkMainMem::MODEL_mkMainMem()
{
  mkMainMem_instance = NULL;
}

/* Function for creating a new model */
void * new_MODEL_mkMainMem()
{
  MODEL_mkMainMem *model = new MODEL_mkMainMem();
  return (void *)(model);
}

/* Schedule functions */

static void schedule_posedge_CLK(tSimStateHdl simHdl, void *instance_ptr)
       {
	 MOD_mkMainMem &INST_top = *((MOD_mkMainMem *)(instance_ptr));
	 tUInt8 DEF_INST_top_DEF_bram_serverAdapter_s1_5_BIT_1___d40;
	 tUInt8 DEF_INST_top_DEF_bram_serverAdapter_outData_ff_i_notFull____d38;
	 tUInt8 DEF_INST_top_DEF_bram_serverAdapter_outData_beforeDeq_read____d45;
	 tUInt8 DEF_INST_top_DEF_bram_serverAdapter_outData_enqw_whas____d1;
	 tUInt8 DEF_INST_top_DEF_bram_serverAdapter_outData_dequeueing_whas____d2;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_outData_enqueue;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_outData_enqueue;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_outData_dequeue;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_outData_dequeue;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_cnt_finalAdd;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_cnt_finalAdd;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_s1__dreg_update;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_s1__dreg_update;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_stageReadResponseAlways;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_stageReadResponseAlways;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_moveToOutFIFO;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_moveToOutFIFO;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_overRun;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_overRun;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_1;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_1;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_2;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_2;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_3;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_3;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_4;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_4;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_5;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_5;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_6;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_6;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_7;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_7;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_8;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_8;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_9;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_9;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_10;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_10;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_11;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_11;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_12;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_12;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_13;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_13;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_14;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_14;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_15;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_15;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_16;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_16;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_17;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_17;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_18;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_18;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_19;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_19;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_20;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_20;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_21;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_21;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_22;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_22;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_23;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_23;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_24;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_24;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_25;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_25;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_26;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_26;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_27;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_27;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_28;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_28;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_29;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_29;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_30;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_30;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_31;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_31;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_32;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_32;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_33;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_33;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_34;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_34;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_35;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_35;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_36;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_36;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_37;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_37;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_38;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_38;
	 tUInt8 DEF_INST_top_DEF_CAN_FIRE_RL_deq;
	 tUInt8 DEF_INST_top_DEF_WILL_FIRE_RL_deq;
	 INST_top.DEF_bram_serverAdapter_s1___d35 = INST_top.INST_bram_serverAdapter_s1.METH_read();
	 DEF_INST_top_DEF_bram_serverAdapter_outData_ff_i_notFull____d38 = INST_top.INST_bram_serverAdapter_outData_ff.METH_i_notFull();
	 DEF_INST_top_DEF_bram_serverAdapter_s1_5_BIT_1___d40 = (tUInt8)((INST_top.DEF_bram_serverAdapter_s1___d35) >> 1u);
	 INST_top.DEF_bram_serverAdapter_s1_5_BIT_0___d36 = (tUInt8)((tUInt8)1u & (INST_top.DEF_bram_serverAdapter_s1___d35));
	 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_moveToOutFIFO = (!(INST_top.DEF_bram_serverAdapter_s1_5_BIT_0___d36) || DEF_INST_top_DEF_bram_serverAdapter_outData_ff_i_notFull____d38) && DEF_INST_top_DEF_bram_serverAdapter_s1_5_BIT_1___d40;
	 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_moveToOutFIFO = DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_moveToOutFIFO;
	 DEF_INST_top_DEF_bram_serverAdapter_outData_beforeDeq_read____d45 = INST_top.INST_bram_serverAdapter_outData_beforeDeq.METH_read();
	 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_overRun = DEF_INST_top_DEF_bram_serverAdapter_s1_5_BIT_1___d40 && ((!INST_top.INST_bram_serverAdapter_outData_beforeEnq.METH_read() || !DEF_INST_top_DEF_bram_serverAdapter_outData_beforeDeq_read____d45) || !DEF_INST_top_DEF_bram_serverAdapter_outData_ff_i_notFull____d38);
	 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_overRun = DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_overRun;
	 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_s1__dreg_update = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_s1__dreg_update = DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_s1__dreg_update;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_1 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_1 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_1;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_10 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_10 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_10;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_12 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_12 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_12;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_11 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_11 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_11;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_13 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_13 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_13;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_14 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_14 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_14;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_15 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_15 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_15;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_19 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_19 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_19;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_16 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_16 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_16;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_17 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_17 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_17;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_18 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_18 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_18;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_2 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_2 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_2;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_20 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_20 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_20;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_21 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_21 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_21;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_23 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_23 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_23;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_22 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_22 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_22;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_24 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_24 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_24;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_25 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_25 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_25;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_26 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_26 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_26;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_27 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_27 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_27;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_29 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_29 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_29;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_28 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_28 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_28;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_3 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_3 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_3;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_30 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_30 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_30;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_31 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_31 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_31;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_32 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_32 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_32;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_34 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_34 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_34;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_33 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_33 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_33;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_35 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_35 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_35;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_36 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_36 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_36;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_37 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_37 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_37;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_6 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_6 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_6;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_38 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_38 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_38;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_4 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_4 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_4;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_5 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_5 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_5;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_7 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_7 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_7;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_8 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_8 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_8;
	 DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_9 = (tUInt8)1u;
	 DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_9 = DEF_INST_top_DEF_CAN_FIRE_RL_dl_try_move_9;
	 INST_top.DEF_WILL_FIRE_put = INST_top.PORT_EN_put;
	 INST_top.METH_RDY_put();
	 INST_top.DEF_WILL_FIRE_get = INST_top.PORT_EN_get;
	 INST_top.METH_RDY_get();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_overRun)
	   INST_top.RL_bram_serverAdapter_overRun();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_moveToOutFIFO)
	   INST_top.RL_bram_serverAdapter_moveToOutFIFO();
	 if (INST_top.DEF_WILL_FIRE_get)
	   INST_top.METH_get();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move)
	   INST_top.RL_dl_try_move();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_1)
	   INST_top.RL_dl_try_move_1();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_2)
	   INST_top.RL_dl_try_move_2();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_3)
	   INST_top.RL_dl_try_move_3();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_4)
	   INST_top.RL_dl_try_move_4();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_5)
	   INST_top.RL_dl_try_move_5();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_6)
	   INST_top.RL_dl_try_move_6();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_7)
	   INST_top.RL_dl_try_move_7();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_8)
	   INST_top.RL_dl_try_move_8();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_9)
	   INST_top.RL_dl_try_move_9();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_10)
	   INST_top.RL_dl_try_move_10();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_11)
	   INST_top.RL_dl_try_move_11();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_12)
	   INST_top.RL_dl_try_move_12();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_13)
	   INST_top.RL_dl_try_move_13();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_14)
	   INST_top.RL_dl_try_move_14();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_15)
	   INST_top.RL_dl_try_move_15();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_16)
	   INST_top.RL_dl_try_move_16();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_17)
	   INST_top.RL_dl_try_move_17();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_18)
	   INST_top.RL_dl_try_move_18();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_19)
	   INST_top.RL_dl_try_move_19();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_20)
	   INST_top.RL_dl_try_move_20();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_21)
	   INST_top.RL_dl_try_move_21();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_22)
	   INST_top.RL_dl_try_move_22();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_23)
	   INST_top.RL_dl_try_move_23();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_24)
	   INST_top.RL_dl_try_move_24();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_25)
	   INST_top.RL_dl_try_move_25();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_26)
	   INST_top.RL_dl_try_move_26();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_27)
	   INST_top.RL_dl_try_move_27();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_28)
	   INST_top.RL_dl_try_move_28();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_29)
	   INST_top.RL_dl_try_move_29();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_30)
	   INST_top.RL_dl_try_move_30();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_31)
	   INST_top.RL_dl_try_move_31();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_32)
	   INST_top.RL_dl_try_move_32();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_33)
	   INST_top.RL_dl_try_move_33();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_34)
	   INST_top.RL_dl_try_move_34();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_35)
	   INST_top.RL_dl_try_move_35();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_36)
	   INST_top.RL_dl_try_move_36();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_37)
	   INST_top.RL_dl_try_move_37();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_dl_try_move_38)
	   INST_top.RL_dl_try_move_38();
	 INST_top.DEF_dl_d_0_rv_port1__read____d366 = INST_top.INST_dl_d_0_rv.METH_port1__read();
	 DEF_INST_top_DEF_bram_serverAdapter_outData_enqw_whas____d1 = INST_top.INST_bram_serverAdapter_outData_enqw.METH_whas();
	 INST_top.DEF_bram_serverAdapter_outData_ff_i_notEmpty____d4 = INST_top.INST_bram_serverAdapter_outData_ff.METH_i_notEmpty();
	 DEF_INST_top_DEF_CAN_FIRE_RL_deq = (DEF_INST_top_DEF_bram_serverAdapter_outData_beforeDeq_read____d45 && (INST_top.DEF_bram_serverAdapter_outData_ff_i_notEmpty____d4 || DEF_INST_top_DEF_bram_serverAdapter_outData_enqw_whas____d1)) && !INST_top.DEF_dl_d_0_rv_port1__read____d366.get_bits_in_word8(16u,
																																						 0u,
																																						 1u);
	 DEF_INST_top_DEF_WILL_FIRE_RL_deq = DEF_INST_top_DEF_CAN_FIRE_RL_deq;
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_deq)
	   INST_top.RL_deq();
	 DEF_INST_top_DEF_bram_serverAdapter_outData_dequeueing_whas____d2 = INST_top.INST_bram_serverAdapter_outData_dequeueing.METH_whas();
	 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_outData_dequeue = DEF_INST_top_DEF_bram_serverAdapter_outData_dequeueing_whas____d2 && INST_top.DEF_bram_serverAdapter_outData_ff_i_notEmpty____d4;
	 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_outData_dequeue = DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_outData_dequeue;
	 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_outData_enqueue = DEF_INST_top_DEF_bram_serverAdapter_outData_enqw_whas____d1 && (!DEF_INST_top_DEF_bram_serverAdapter_outData_dequeueing_whas____d2 || INST_top.DEF_bram_serverAdapter_outData_ff_i_notEmpty____d4);
	 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_outData_enqueue = DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_outData_enqueue;
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_outData_dequeue)
	   INST_top.RL_bram_serverAdapter_outData_dequeue();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_outData_enqueue)
	   INST_top.RL_bram_serverAdapter_outData_enqueue();
	 if (INST_top.DEF_WILL_FIRE_put)
	   INST_top.METH_put(INST_top.PORT_put_req);
	 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_stageReadResponseAlways = INST_top.INST_bram_serverAdapter_writeWithResp.METH_whas();
	 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_stageReadResponseAlways = DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_stageReadResponseAlways;
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_stageReadResponseAlways)
	   INST_top.RL_bram_serverAdapter_stageReadResponseAlways();
	 INST_top.DEF_bram_serverAdapter_cnt_3_whas____d13 = INST_top.INST_bram_serverAdapter_cnt_3.METH_whas();
	 INST_top.DEF_bram_serverAdapter_cnt_2_whas____d11 = INST_top.INST_bram_serverAdapter_cnt_2.METH_whas();
	 INST_top.DEF_bram_serverAdapter_cnt_1_whas____d10 = INST_top.INST_bram_serverAdapter_cnt_1.METH_whas();
	 DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_cnt_finalAdd = (INST_top.DEF_bram_serverAdapter_cnt_1_whas____d10 || INST_top.DEF_bram_serverAdapter_cnt_2_whas____d11) || INST_top.DEF_bram_serverAdapter_cnt_3_whas____d13;
	 DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_cnt_finalAdd = DEF_INST_top_DEF_CAN_FIRE_RL_bram_serverAdapter_cnt_finalAdd;
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_cnt_finalAdd)
	   INST_top.RL_bram_serverAdapter_cnt_finalAdd();
	 if (DEF_INST_top_DEF_WILL_FIRE_RL_bram_serverAdapter_s1__dreg_update)
	   INST_top.RL_bram_serverAdapter_s1__dreg_update();
	 INST_top.INST_dl_d_39_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_38_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_37_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_36_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_35_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_34_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_33_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_32_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_31_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_30_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_29_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_28_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_27_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_26_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_25_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_24_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_23_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_22_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_21_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_20_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_19_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_18_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_17_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_16_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_15_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_14_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_13_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_12_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_11_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_10_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_9_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_8_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_7_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_6_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_5_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_4_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_3_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_2_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_1_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_dl_d_0_rv.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_bram_serverAdapter_s1_1.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_bram_serverAdapter_writeWithResp.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_bram_serverAdapter_cnt_3.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_bram_serverAdapter_cnt_2.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_bram_serverAdapter_cnt_1.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_bram_serverAdapter_outData_dequeueing.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_bram_serverAdapter_outData_enqw.clk((tUInt8)1u, (tUInt8)1u);
	 INST_top.INST_bram_memory.clk((tUInt8)1u, (tUInt8)1u);
	 if (do_reset_ticks(simHdl))
	 {
	   INST_top.INST_bram_serverAdapter_outData_ff.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_bram_serverAdapter_cnt.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_bram_serverAdapter_s1.rst_tick__clk__1((tUInt8)1u);
	   INST_top.INST_dl_d_0_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_1_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_2_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_3_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_4_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_5_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_6_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_7_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_8_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_9_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_10_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_11_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_12_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_13_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_14_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_15_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_16_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_17_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_18_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_19_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_20_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_21_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_22_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_23_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_24_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_25_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_26_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_27_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_28_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_29_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_30_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_31_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_32_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_33_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_34_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_35_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_36_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_37_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_38_rv.rst_tick_clk((tUInt8)1u);
	   INST_top.INST_dl_d_39_rv.rst_tick_clk((tUInt8)1u);
	 }
       };

/* Model creation/destruction functions */

void MODEL_mkMainMem::create_model(tSimStateHdl simHdl, bool master)
{
  sim_hdl = simHdl;
  init_reset_request_counters(sim_hdl);
  mkMainMem_instance = new MOD_mkMainMem(sim_hdl, "top", NULL);
  bk_get_or_define_clock(sim_hdl, "CLK");
  if (master)
  {
    bk_alter_clock(sim_hdl, bk_get_clock_by_name(sim_hdl, "CLK"), CLK_LOW, false, 0llu, 5llu, 5llu);
    bk_use_default_reset(sim_hdl);
  }
  bk_set_clock_event_fn(sim_hdl,
			bk_get_clock_by_name(sim_hdl, "CLK"),
			schedule_posedge_CLK,
			NULL,
			(tEdgeDirection)(POSEDGE));
  (mkMainMem_instance->INST_bram_memory.set_clk_0)("CLK");
  (mkMainMem_instance->INST_bram_serverAdapter_outData_ff.set_clk_0)("CLK");
  (mkMainMem_instance->INST_bram_serverAdapter_outData_enqw.set_clk_0)("CLK");
  (mkMainMem_instance->INST_bram_serverAdapter_outData_dequeueing.set_clk_0)("CLK");
  (mkMainMem_instance->INST_bram_serverAdapter_cnt_1.set_clk_0)("CLK");
  (mkMainMem_instance->INST_bram_serverAdapter_cnt_2.set_clk_0)("CLK");
  (mkMainMem_instance->INST_bram_serverAdapter_cnt_3.set_clk_0)("CLK");
  (mkMainMem_instance->INST_bram_serverAdapter_writeWithResp.set_clk_0)("CLK");
  (mkMainMem_instance->INST_bram_serverAdapter_s1_1.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_0_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_1_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_2_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_3_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_4_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_5_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_6_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_7_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_8_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_9_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_10_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_11_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_12_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_13_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_14_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_15_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_16_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_17_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_18_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_19_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_20_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_21_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_22_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_23_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_24_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_25_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_26_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_27_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_28_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_29_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_30_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_31_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_32_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_33_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_34_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_35_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_36_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_37_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_38_rv.set_clk_0)("CLK");
  (mkMainMem_instance->INST_dl_d_39_rv.set_clk_0)("CLK");
  (mkMainMem_instance->set_clk_0)("CLK");
}
void MODEL_mkMainMem::destroy_model()
{
  delete mkMainMem_instance;
  mkMainMem_instance = NULL;
}
void MODEL_mkMainMem::reset_model(bool asserted)
{
  (mkMainMem_instance->reset_RST_N)(asserted ? (tUInt8)0u : (tUInt8)1u);
}
void * MODEL_mkMainMem::get_instance()
{
  return mkMainMem_instance;
}

/* Fill in version numbers */
void MODEL_mkMainMem::get_version(char const **name, char const **build)
{
  *name = "2023.01-6-g034050db";
  *build = "034050db";
}

/* Get the model creation time */
time_t MODEL_mkMainMem::get_creation_time()
{
  
  /* Fri Mar 24 20:43:22 UTC 2023 */
  return 1679690602llu;
}

/* State dumping function */
void MODEL_mkMainMem::dump_state()
{
  (mkMainMem_instance->dump_state)(0u);
}

/* VCD dumping functions */
MOD_mkMainMem & mkMainMem_backing(tSimStateHdl simHdl)
{
  static MOD_mkMainMem *instance = NULL;
  if (instance == NULL)
  {
    vcd_set_backing_instance(simHdl, true);
    instance = new MOD_mkMainMem(simHdl, "top", NULL);
    vcd_set_backing_instance(simHdl, false);
  }
  return *instance;
}
void MODEL_mkMainMem::dump_VCD_defs()
{
  (mkMainMem_instance->dump_VCD_defs)(vcd_depth(sim_hdl));
}
void MODEL_mkMainMem::dump_VCD(tVCDDumpType dt)
{
  (mkMainMem_instance->dump_VCD)(dt, vcd_depth(sim_hdl), mkMainMem_backing(sim_hdl));
}
