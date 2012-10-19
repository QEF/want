#
#===============================
# WanT package
#===============================
#
include ../../make.sys

OBJS=decay_module.o \
midpoint_module.o

WANNIER_OBJS= \
../wannier/allocate_nlpot.o \
../wannier/atom.o \
../wannier/atomic.o \
../wannier/atomic_wfc.o \
../wannier/bachel.o \
../wannier/bshells_init.o \
../wannier/cleanup.o \
../wannier/compute_kham.o \
../wannier/control.o \
../wannier/converters.o \
../wannier/correlation.o \
../wannier/crystal_tools.o \
../wannier/etsf_io_data.o \
../wannier/functionals.o \
../wannier/get_monkpack.o \
../wannier/get_rgrid.o \
../wannier/gglobal.o \
../wannier/ggrids.o \
../wannier/hamiltonian.o \
../wannier/init_at_1.o \
../wannier/init_us_1.o \
../wannier/init_us_2.o \
../wannier/input_parameters.o \
../wannier/io_module.o \
../wannier/internal_tools.o \
../wannier/ions.o \
../wannier/kpoints.o \
../wannier/lattice.o \
../wannier/localization.o \
../wannier/memusage.o \
../wannier/monkpack.o \
../wannier/overlap_module.o \
../wannier/operator.o \
../wannier/paratools.o \
../wannier/pseudo_types.o \
../wannier/pwcom.o \
../wannier/qvan2.o \
../wannier/radial_grids.o \
../wannier/read_ncpp.o \
../wannier/read_pseudo.o \
../wannier/read_upf_v1.o \
../wannier/read_upf_v2.o \
../wannier/read_uspp.o \
../wannier/recips.o \
../wannier/setqf.o \
../wannier/shutdown.o \
../wannier/sort_gvec.o \
../wannier/startup.o \
../wannier/struct_fact.o \
../wannier/struct_fact_data.o \
../wannier/subspace.o \
../wannier/summary.o \
../wannier/symmetrize_kgrid.o \
../wannier/symmetry.o \
../wannier/trial_center.o \
../wannier/trial_center_data.o \
../wannier/upf.o \
../wannier/upf_to_internal.o \
../wannier/uspp.o \
../wannier/version.o \
../wannier/wannier90_tools.o \
../wannier/want_init.o \
../wannier/want_dftread.o \
../wannier/wfc_data.o \
../wannier/wfc_info.o \
../wannier/windows.o \
../wannier/workspace_wan.o \
../wannier/workspace_dis.o \
../wannier/write_upf_v2.o 



EXTERNAL_LIBS=  ../clibs/libclibs.a ../wlibs/libwant.a
BIN_DEPS=      $(OBJS) $(WANNIER_OBJS) $(EXTERNAL_LIBS)

#
# main targets
#
all: midpoint.x wfk2etsf.x kgrid.x sumpdos sum_sgm.x \
     sax2qexml.x decay.x gcube2plt.x write_ham.x


midpoint.x: midpoint.o $(BIN_DEPS)
	$(LD) $(LDFLAGS) -o $@ midpoint.o \
	       $(OBJS) $(WANNIER_OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../../utility/$@ . )

wfk2etsf.x: wfk2etsf.o $(BIN_DEPS)
	$(LD) $(LDFLAGS) -o $@ wfk2etsf.o \
	       $(OBJS) $(WANNIER_OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../../utility/$@ . )

kgrid.x: kgrid.o 
	$(LD) $(LDFLAGS) -o $@ kgrid.o 
	- ( cd ../../bin ; ln -sf ../../utility/$@ . )

gcube2plt.x: gcube2plt.o
	$(CC) $(CFLAGS) -o $@ gcube2plt.c
	- ( cd ../../bin ; ln -sf ../../utility/$@ . )

sumpdos: sumpdos.o
	$(LD) $(LDFLAGS) -o $@ sumpdos.o
	- ( cd ../../bin ; ln -sf ../../utility/$@ . )

decay.x: decay.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ decay.o \
	       $(OBJS) $(WANNIER_OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../../utility/$@ . )

sum_sgm.x: sum_sgm.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ sum_sgm.o \
	       $(OBJS) $(WANNIER_OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../../utility/$@ . )

sax2qexml.x: sax2qexml.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ sax2qexml.o \
	       $(OBJS) $(WANNIER_OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../../utility/$@ . )
	- ( cd ../../bin ; ln -sf ../../utility/sax2qexml . )

write_ham.x: write_ham.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ write_ham.o \
	       $(OBJS) $(WANNIER_OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../../utility/$@ . )

clean:
	rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x sumpdos

# dependencies
include make.depend

