#
#===============================
# WanT package
#===============================
#
include ../make.sys


OBJS= \
allocate_nlpot.o \
atom.o \
atomic.o \
atomic_wfc.o \
bachel.o \
becmod.o \
bshells_init.o \
ccalbec.o \
cleanup.o \
collect_wf.o \
control.o \
converters.o \
dft_interface.o \
domega_aux.o \
domega.o \
functionals.o \
get_rgrid.o \
get_monkpack.o \
get_points.o \
ggrids.o \
hamiltonian_calc.o \
hamiltonian.o \
init_at_1.o \
init_us_1.o \
init_us_2.o \
input_base.o \
input.o \
input_parameters.o \
io_module.o \
ions.o \
kpoints.o \
lattice.o \
localization.o \
localization_init.o \
omega.o \
omegai.o \
ordering.o \
overlap_augment.o \
overlap_extract.o \
overlap.o \
overlap_module.o \
overlap_setmap.o \
overlap_update.o \
polarization.o \
projection.o \
projection_frozen.o \
pseudo_types.o \
pwcom.o \
qvan2.o \
random_orthovect.o \
read_ncpp.o \
readnewvan.o \
readpp.o \
read_pseudo_base.o \
read_pseudo.o \
readvan.o \
recips.o \
setqf.o \
s_psi.o \
struct_fact_data.o \
struct_fact.o \
subspace.o \
subspace_init.o \
summary.o \
startup.o \
trial_center_data.o \
trial_center.o \
unitary_update.o \
upf_to_internal.o \
uspp.o \
version.o \
want_init.o \
wfc_data.o \
wfc_info.o \
wfc_manager.o \
windows.o \
xsf.o \
zmatrix.o \
gcube2plt.o 

BIN= \
disentangle.x \
wannier.x \
bands.x \
plot.x \
blc2wan.x

EXTERNAL_LIBS= ../iotk/libiotk.a ../libs/libwant.a 
BIN_DEPS=      $(OBJS) $(EXTERNAL_LIBS)

#
# main targets
#
all : $(BIN)


disentangle.x: disentangle.o $(BIN_DEPS)
	$(LD) -o $@ disentangle.o $(OBJS) $(LDFLAGS)
	- ( cd ../bin ; ln -sf ../wannier/$@ . )

wannier.x:     wannier.o $(BIN_DEPS)
	$(LD) -o $@ wannier.o $(OBJS) $(LDFLAGS)
	- ( cd ../bin ; ln -sf ../wannier/$@ . )

plot.x:        plot.o $(BIN_DEPS)
	$(LD) -o $@ plot.o $(OBJS) $(LDFLAGS)
	- ( cd ../bin ; ln -sf ../wannier/$@ . )

bands.x:       bands.o $(BIN_DEPS)
	$(LD) -o $@ bands.o $(OBJS) $(LDFLAGS)
	- ( cd ../bin ; ln -sf ../wannier/$@ . )

blc2wan.x:     blc2wan.o $(BIN_DEPS)
	$(LD) -o $@ blc2wan.o $(OBJS) $(LDFLAGS)
	- ( cd ../bin ; ln -sf ../wannier/$@ . )

#
# cleaning
#
clean:
	- /bin/rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x

# dependencies
include make.depend

