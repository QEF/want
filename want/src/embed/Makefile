#
#===============================
# WanT package
#===============================
#
include ../../make.sys


OBJS= \
cleanup.o \
control.o \
correlation.o \
datafiles.o \
hamiltonian.o \
hamiltonian_init.o \
hamiltonian_setup.o \
input.o \
input_parameters.o \
memusage.o \
summary.o \
workspace.o


WANNIER_OBJS= \
../wannier/atmproj_tools.o \
../wannier/compute_kham.o \
../wannier/compute_rham.o \
../wannier/control.o \
../wannier/converters.o \
../wannier/cp2k_tools.o \
../wannier/crystal_tools.o \
../wannier/datafiles.o \
../wannier/etsf_io_data.o \
../wannier/get_monkpack.o \
../wannier/get_rgrid.o \
../wannier/io_module.o \
../wannier/internal_tools.o \
../wannier/monkpack.o \
../wannier/operator.o \
../wannier/startup.o \
../wannier/smearing_base.o \
../wannier/shutdown.o \
../wannier/version.o \
../wannier/wannier90_tools.o 

TRANSPORT_OBJS= \
../transport/egrid.o \
../transport/fourier_par.o \
../transport/gzero_maker.o \
../transport/kpoints.o \
../transport/operator_blc.o \
../transport/read_matrix.o \
../transport/smearing_T.o

BIN= \
embed.x 

EXTERNAL_LIBS=  ../clib/libclib.a ../baselib/libwant.a
BIN_DEPS=  $(OBJS) $(WANNIER_OBJS) $(TRANSPORT_OBJS) $(EXTERNAL_LIBS)
OBJS_ALL=  $(OBJS) $(WANNIER_OBJS) $(TRANSPORT_OBJS)

#
# main targets
#
all: $(BIN)


embed.x: $(BIN_DEPS) embed.o $(OBJS_ALL)
	$(LD) $(LDFLAGS) -o $@ embed.o \
	     $(OBJS_ALL) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../src/embed/$@ . )

clean:
	rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x

# dependencies
include make.depend

