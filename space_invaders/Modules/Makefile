include ../make.sys
include ../make.rules

OBJS = constants.o fft_scalar.o kind.o parameters.o \
       dyn_op_read.o  dyn_op_write.o mp.o parallel_include.o \
       mp_global.o  io_module.o shmem_include.o input.o \
       startup.o  timing.o  version.o  parser.o  converters.o \
       sph_har.o  kpoints.o  ions.o  util.o  lattice.o \
       subspace.o  windows.o  overlap_module.o  wfc.o \
       ggrids.o  files.o  parser_base.o  localization.o \
       cleanup.o  want_init.o  summary.o  ions.o  wfc_manager.o \
       upf_to_internal.o  uspp.o  read_pseudo_base.o  pwcom.o \
       pseudo_types.o  functionals.o  atom.o  becmod.o \
       struct_fact_data.o  trial_center.o  trial_center_data.o

all: $(OBJS)

clean: 
	rm -rf *.o *.a *core  work.pc work.pcl *.mod *.d

# dependencies

include .dependencies
