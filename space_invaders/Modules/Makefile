include ../make.sys
include ../make.rules

OBJS = constants.o fft_scalar.o kind.o parameters.o \
       dyn_op_read.o  dyn_op_write.o mp.o parallel_include.o \
       mp_global.o  io_module.o shmem_include.o input_wan.o \
       startup.o  timing.o  version.o  parser.o  converters.o \
       sph_har.o  kpoints.o  ions.o  util.o  lattice.o \
       subspace.o  windows_module.o  overlap_module.o  wfc.o \
       ggrids.o  files.o  parser_base.o  localization.o 

all: $(OBJS)

clean: 
	rm -rf *.o *.a *core  work.pc work.pcl *.mod *.d

# dependencies

include .dependencies
