include ../make.sys
include ../make.rules

OBJS = constants.o fft_scalar.o kind.o parameters.o \
       dyn_op_read.o  dyn_op_write.o mp.o parallel_include.o \
       mp_global.o  io_global.o shmem_include.o input_wan.o \
       parser.o

all: $(OBJS)

clean: 
	rm -rf *.o *.a *core  work.pc work.pcl *.mod

# dependencies

include .dependencies
