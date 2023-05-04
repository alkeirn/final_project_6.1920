/*
 * Generated by Bluespec Compiler, version 2023.01-6-g034050db (build 034050db)
 * 
 * On Wed May  3 23:41:50 EDT 2023
 * 
 */

/* Generation options: */
#ifndef __model_mktop_bsv_h__
#define __model_mktop_bsv_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"

#include "bs_model.h"
#include "mktop_bsv.h"

/* Class declaration for a model of mktop_bsv */
class MODEL_mktop_bsv : public Model {
 
 /* Top-level module instance */
 private:
  MOD_mktop_bsv *mktop_bsv_instance;
 
 /* Handle to the simulation kernel */
 private:
  tSimStateHdl sim_hdl;
 
 /* Constructor */
 public:
  MODEL_mktop_bsv();
 
 /* Functions required by the kernel */
 public:
  void create_model(tSimStateHdl simHdl, bool master);
  void destroy_model();
  void reset_model(bool asserted);
  void get_version(char const **name, char const **build);
  time_t get_creation_time();
  void * get_instance();
  void dump_state();
  void dump_VCD_defs();
  void dump_VCD(tVCDDumpType dt);
};

/* Function for creating a new model */
extern "C" {
  void * new_MODEL_mktop_bsv();
}

#endif /* ifndef __model_mktop_bsv_h__ */
