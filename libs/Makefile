#
#===============================
# WanT package
#===============================
#
include ../make.sys

OBJS= \
constants.o \
date_and_tim.o \
erf.o \
error.o \
fft_scalar.o \
files.o \
indexx.o \
input_from_file.o \
invmat.o \
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
rranf.o \
shmem_include.o \
simpsn.o \
sort.o \
sph_bes.o \
sph_har.o \
timing.o \
util.o \
warning.o \
ylmr2.o



#
# main targets
#
all : libwant.a $(INTERNAL_LIBS)


libwant.a : $(OBJS) 
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@

blas.a : blas.o
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@

lapack_atlas.a : lapack_atlas.o dlamch.o
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@

lapack.a : lapack_atlas.o lapack_all.o dlamch.o
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@

dlamch.o : dlamch.f
	$(F77) $(FFLAGS_NOOPT) -c $<


#
# cleaning
# 
clean: 
	- /bin/rm -f *.o *.a *core *.F90 *.mod work.pc work.pcl
	        

# dependencies
include make.depend

