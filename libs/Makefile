#
#===============================
# WanT package
#===============================
#
include ../make.sys

OBJS= \
constants.f90 \
date_and_tim.f90 \
erf.f90 \
error.f90 \
fft_scalar.f90 \
files.f90 \
indexx.f90 \
invmat.f90 \
kind.f90 \
mp.f90 \
mp_global.f90 \
parallel_include.f90 \
parameters.f90 \
parser_base.f90 \
parser.f90 \
rranf.f90 \
shmem_include.f90 \
simpsn.f90 \
sort.f90 \
sph_bes.f90 \
sph_har.f90 \
startup.f90 \
timing.f90 \
util.f90 \
ylmr2.f90 \
cptimer.c \  \
fft_stick.c \ 
fftw.c


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

