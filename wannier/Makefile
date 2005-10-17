#
#===============================
# WanT package
#===============================
#
include ../make.sys


OBJS= \
allocate_nlpot.f90 \
atom.f90 \
atomic.f90 \
atomic_wfc.f90 \
bachel.f90 \
becmod.f90 \
bshells.f90 \
ccalbec.f90 \
cleanup.f90 \
control.f90 \
converters.f90 \
dft_interface.f90 \
domega_aux.f90 \
domega.f90 \
functionals.f90 \
get_monkpack.f90 \
get_points.f90 \
ggrids.f90 \
hamiltonian_calc.f90 \
hamiltonian.f90 \
init_at_1.f90 \
init_us_1.f90 \
init_us_2.f90 \
input_base.f90 \
input.f90 \
input_parameters.f90 \
io_module.f90 \
ions.f90 \
kpoints.f90 \
lattice.f90 \
localization.f90 \
localization_init.f90 \
omega.f90 \
omegai.f90 \
ordering.f90 \
overlap_augment.f90 \
overlap_extract.f90 \
overlap.f90 \
overlap_module.f90 \
overlap_setmap.f90 \
overlap_update.f90 \
phases.f90 \
projection.f90 \
projection_frozen.f90 \
pseudo_types.f90 \
pwcom.f90 \
qvan2.f90 \
random_orthovect.f90 \
read_ncpp.f90 \
readnewvan.f90 \
readpp.f90 \
read_pseudo_base.f90 \
read_pseudo.f90 \
readvan.f90 \
recips.f90 \
setqf.f90 \
s_psi.f90 \
struct_fact_data.f90 \
struct_fact.f90 \
subspace.f90 \
subspace_init.f90 \
summary.f90 \
trial_center_data.f90 \
trial_center.f90 \
unitary_update.f90 \
upf_to_internal.f90 \
uspp.f90 \
version.f90 \
want_init.f90 \
wfc_data.f90 \
wfc_info.f90 \
wfc_manager.f90 \
wigner_seitz.f90 \
windows.f90 \
xsf.f90 \
zmatrix.f90

BIN= \
disentangle.x \
wannier.x \
bands.x \
plot.x \
blc2wan.x

BIN_DEPENDENCIES= ../iotk/libiotk.a ../libs/libwant.a $(OBJS)

#
# main targets
#
all : $(BIN)


disentangle.x: disentangle.o $(BIN_DEPENDENCIES)
	$(LD) -o $@ disentangle.o $(OBJS) $(LDFLAGS)
	- ( cd ../bin ; ln -sf ../wannier/$@ . )

wannier.x:     wannier.o $(BIN_DEPENDENCIES)
	$(LD) -o $@ wannier.o $(OBJS) $(LDFLAGS)
	- ( cd ../bin ; ln -sf ../wannier/$@ . )

plot.x:        plot.o $(BIN_DEPENDENCIES)
	$(LD) -o $@ plot.o $(OBJS) $(LDFLAGS)
	- ( cd ../bin ; ln -sf ../wannier/$@ . )

bands.x:       bands.o $(BIN_DEPENDENCIES)
	$(LD) -o $@ bands.o $(OBJS) $(LDFLAGS)
	- ( cd ../bin ; ln -sf ../wannier/$@ . )

blc2wan.x:     blc2wan.o $(BIN_DEPENDENCIES)
	$(LD) -o $@ blc2wan.o $(OBJS) $(LDFLAGS)
	- ( cd ../bin ; ln -sf ../wannier/$@ . )

#
# cleaning
#
clean:
	- /bin/rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x

# dependencies
include make.depend

