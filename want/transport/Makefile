#
#===============================
# WanT package
#===============================
#
include ../make.sys


OBJS= \
green.o \
hamiltonian_setup.o \
read_matrix.o \
transfer.o \
transmittance.o


EXTERNAL_LIBS= ../iotk/libiotk.a ../libs/libwant.a
LOCAL_LIBS=    ../wannier/libwannier.a ./libtrans.a
BIN_DEPS=      $(LOCAL_LIBS) $(EXTERNAL_LIBS)

#
# main targets
#
all: conductor.x 


libtrans.a : $(OBJS)
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@

conductor.x: conductor.o $(BIN_DEPS)
	$(LD) -o $@ conductor.o $(LOCAL_LIBS) $(LDFLAGS)
	- ( cd ../bin ; ln -sf ../transport/$@ . )

clean:
	rm -rf *.o *.a *core  work.pc work.pcl *.mod *.x

# dependencies
include make.depend

