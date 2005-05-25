include ../make.sys


OBJS= compar.o domega.o domega_aux.o error.o \
      get_points.o indexx.o komegai.o \
      bshells.o omega.o \
      phases.o projection.o projection_frozen.o \
      wigner_seitz.o zmatrix.o fft_stick.o \
      write_op.o check_sgm_kpt.o check_sgm_wan.o do_self_energy.o \
      gcube2plt.o cptimer.o date_and_tim.o \
      ordering.o sort.o recips.o  \
      overlap.o overlap_setmap.o \
      overlap_extract.o overlap_update.o overlap_augment.o \
      get_monkpack.o subspace_init.o localization_init.o \
      readpp.o read_pseudo.o read_ncpp.o readnewvan.o readvan.o \
      bachel.o simpsn.o ylmr2.o invmat.o rranf.o erf.o \
      init_us_1.o init_us_2.o qvan2.o sph_bes.o setqf.o \
      allocate_nlpot.o struct_fact.o ccalbec.o \
      s_psi.o random_orthovect.o atomic_wfc.o init_at_1.o


all : lib_invaders.a

lib_invaders.a : $(OBJS)
	$(AR) $(ARFLAGS) $@ $?
#	touch lib_invaders.timestamp

clean: 
	rm -rf *.o *.a *core *.F90 *.mod work.pc work.pcl

# dependencies
include make.depend
