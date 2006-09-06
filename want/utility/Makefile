#
#===============================
# WanT package
#===============================
#
include ../make.sys

OBJS=

WANNIER_OBJS= \
../wannier/allocate_nlpot.o \
../wannier/atom.o \
../wannier/atomic_wfc.o \
../wannier/bachel.o \
../wannier/bshells_init.o \
../wannier/cleanup.o \
../wannier/control.o \
../wannier/converters.o \
../wannier/functionals.o \
../wannier/get_monkpack.o \
../wannier/get_rgrid.o \
../wannier/ggrids.o \
../wannier/init_at_1.o \
../wannier/init_us_1.o \
../wannier/init_us_2.o \
../wannier/input_parameters.o \
../wannier/io_module.o \
../wannier/ions.o \
../wannier/kpoints.o \
../wannier/lattice.o \
../wannier/localization.o \
../wannier/monkpack.o \
../wannier/overlap_module.o \
../wannier/pseudo_types.o \
../wannier/pwcom.o \
../wannier/qvan2.o \
../wannier/read_ncpp.o \
../wannier/readnewvan.o \
../wannier/readpp.o \
../wannier/read_pseudo_base.o \
../wannier/read_pseudo.o \
../wannier/readvan.o \
../wannier/recips.o \
../wannier/setqf.o \
../wannier/shutdown.o \
../wannier/startup.o \
../wannier/struct_fact.o \
../wannier/struct_fact_data.o \
../wannier/subspace.o \
../wannier/summary.o \
../wannier/trial_center.o \
../wannier/trial_center_data.o \
../wannier/upf_to_internal.o \
../wannier/uspp.o \
../wannier/version.o \
../wannier/want_init.o \
../wannier/want_dftread.o \
../wannier/wfc_data.o \
../wannier/wfc_info.o \
../wannier/hamiltonian.o \
../wannier/windows.o 



EXTERNAL_LIBS= ../iotk/libiotk.a ../libs/libwant.a
BIN_DEPS=      $(OBJS) $(WANNIER_OBJS) $(EXTERNAL_LIBS)

#
# main targets
#
all: midpoint.x kgrid.x #gcube2plt.x


midpoint.x: midpoint.o $(BIN_DEPS)
	$(LD) $(LDFLAGS) -o $@ midpoint.o \
	       $(OBJS) $(WANNIER_OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../bin ; ln -sf ../utility/$@ . )

kgrid.x: kgrid.o 
	$(LD) $(LDFLAGS) -o $@ kgrid.o 
	- ( cd ../bin ; ln -sf ../utility/$@ . )

gcube2plt.x: 
	$(CC) $(CFLAGS) -o $@ gcube2plt.c
	- ( cd ../bin ; ln -sf ../utility/$@ . )

clean:
	rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x

# dependencies
include make.depend

