include ../make.sys
include ../make.rules


OBJS= compar.o domega.o dot_bloch.o error.o \
      get_points.o indexx.o komegai.o \
      lambda_avg.o bshells.o omega.o overlap.o \
      phases.o projection.o projection_frozen.o ranf.o \
      wigner_seitz.o zmatrix.o fft_stick.o \
      write_op.o check_sgm_kpt.o check_sgm_wan.o do_self_energy.o \
      gcube2plt.o cptimer.o date_and_tim.o \
      ordering.o sort.o recips.o overlap_extract.o \
      get_monkpack.o \
      readin.o read_pseudo.o read_ncpp.o readnewvan.o readvan.o \
      bachel.o simpsn.o ylmr2.o invmat.o rranf.o

all : lib_invaders.a

lib_invaders.a : $(OBJS)
	$(AR) $(ARFLAGS) $@ $?
#	touch lib_invaders.timestamp

clean: 
	rm -rf *.o *.a *core *.F90 *.mod work.pc work.pcl

# dependencies
include .dependencies
