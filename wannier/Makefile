include ../make.sys
include ../make.rules


OBJS= compar.o domega.o dot_bloch.o error.o        \
      genbtr.o get_points.o indexx.o inv3.o komegai.o   \
      lambda_avg.o new_bshells.o omega.o overlap_new.o       \
      phases.o projection.o projection_frozen.o ranf.o      \
      spline.o splint.o wigner_seitz.o zmatrix.o fft_stick.o         \
      write_op.o check_sgm_kpt.o check_sgm_wan.o do_self_energy.o    \
      diag_gemat.o gcube2plt.o cptimer.o date_and_tim.o \
      ordering.o sort.o recips.o

all : lib_invaders.a

lib_invaders.a : $(OBJS)
	$(AR) $(ARFLAGS) $@ $?
#	touch lib_invaders.timestamp

clean: 
	rm -rf *.o *.a *core  *.mod work.pc work.pcl

# dependencies
include .dependencies
