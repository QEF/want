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
invmat.o \
io_global.o \
kind.o \
locate.o \
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
ylmr2.o \
c_mkdir.o \
cptimer.o \
fft_stick.o



#
# main targets
#
all : libwant.a $(MYLIB)


libwant.a : $(OBJS) 
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@

#
# add special libs to solve portability problems
# with proprietary libs or to self-contain the code
#
blas_and_lapack : blas lapack
	touch $@

blas : blas.o dlamch.o
	$(AR) $(ARFLAGS) libwant.a $?
	$(RANLIB) libwant.a
	touch $@

lapack : lapack.o lapack2.o dlamch.o
	$(AR) $(ARFLAGS) libwant.a $?
	$(RANLIB) libwant.a
	touch $@

lapack_ibm : lapack_ibm.o dlamch.o
	$(AR) $(ARFLAGS) libwant.a $?
	$(RANLIB) libwant.a
	touch $@

lapack_t3e : lapack_t3e.o 
	$(AR) $(ARFLAGS) libwant.a $?
	touch $@

lapack_mkl : lapack_mkl.o 
	$(AR) $(ARFLAGS) libwant.a $?
	touch $@

dlamch.o : dlamch.f
	$(F77) $(F77FLAGS_NOOPT) -c $<


#
# cleaning
# 
clean: 
	- /bin/rm -f *.o *.a *core *.F90 *.mod work.pc work.pcl \
		blas_and_lapack blas lapack lapack_ibm lapack_t3e \
		lapack_mkl 
	        

# dependencies
include make.depend

