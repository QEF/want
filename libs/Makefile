#
#===============================
# WanT package
#===============================
#
include ../make.sys

OBJS= \
constants.o \
crystal_io.o \
crystal_io_base.o \
date_and_tim.o \
divide_et_impera.o \
erf.o \
error.o \
etsf_error.o \
fft_scalar.o \
files.o \
flush_unit.o \
input_from_file.o \
io_global.o \
kind.o \
locate.o \
log_module.o \
mp.o \
mp_global.o \
parallel_include.o \
parameters.o \
parser_base.o \
parser.o \
qexml.o \
qexpt.o \
random_numbers.o \
shmem_include.o \
simpsn.o \
sort.o \
sph_bes.o \
sph_har.o \
timing.o \
util.o \
warning.o \
write_header.o \
ylmr2.o



#
# main targets
#
all : libwant.a


libwant.a : $(OBJS) 
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@

#
# cleaning
# 
clean: 
	- /bin/rm -f *.o *.a *core *.F90 *.mod work.pc work.pcl
	        

# dependencies
include make.depend

