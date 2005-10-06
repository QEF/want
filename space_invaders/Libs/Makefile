include ../make.sys


OBJS= domega.o domega_aux.o error.o \
      get_points.o indexx.o komegai.o \
      bshells.o omega.o hamiltonian_calc.o \
      phases.o projection.o projection_frozen.o \
      wigner_seitz.o zmatrix.o fft_stick.o \
      gcube2plt.o cptimer.o date_and_tim.o \
      ordering.o sort.o recips.o  \
      overlap.o overlap_setmap.o \
      overlap_extract.o overlap_update.o overlap_augment.o \
      get_monkpack.o subspace_init.o localization_init.o \
      readpp.o read_pseudo.o read_ncpp.o readnewvan.o readvan.o \
      bachel.o simpsn.o ylmr2.o invmat.o rranf.o erf.o \
      init_us_1.o init_us_2.o qvan2.o sph_bes.o setqf.o \
      allocate_nlpot.o struct_fact.o ccalbec.o xsf.o \
      s_psi.o random_orthovect.o atomic_wfc.o init_at_1.o 


all : libwant.a $(MYLIB)

libwant.a : $(OBJS) 
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@

#
# adds to solve portability problems
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

clean: 
	- rm -f *.o *.a *core *.F90 *.mod work.pc work.pcl \
		blas_and_lapack blas lapack lapack_ibm lapack_t3e \
		lapack_mkl 
	        

# dependencies
include make.depend

