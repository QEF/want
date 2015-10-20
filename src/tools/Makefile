#
#===============================
# WanT package
#===============================
#
include ../../make.sys

OBJS=decay_module.o \
midpoint_module.o


LIBOBJS=  ../wannier/libwannier.a ../baselib/libbase.a ../clib/libclib.a
BIN_DEPS=      $(OBJS) $(LIBOBJS)

#
# main targets
#
all: midpoint.x wfk2etsf.x kgrid.x sumpdos sum_sgm.x \
     sax2qexml.x decay.x gcube2plt.x write_ham.x


midpoint.x: midpoint.o $(BIN_DEPS)
	$(LD) $(LDFLAGS) -o $@ midpoint.o \
	       $(OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../src/tools/$@ . )

wfk2etsf.x: wfk2etsf.o $(BIN_DEPS)
	$(LD) $(LDFLAGS) -o $@ wfk2etsf.o \
	       $(OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../src/tools/$@ . )

kgrid.x: kgrid.o 
	$(LD) $(LDFLAGS) -o $@ kgrid.o 
	- ( cd ../../bin ; ln -sf ../src/tools/$@ . )

gcube2plt.x: gcube2plt.o
	$(CC) $(CFLAGS) -o $@ gcube2plt.c
	- ( cd ../../bin ; ln -sf ../src/tools/$@ . )

sumpdos: sumpdos.o
	$(LD) $(LDFLAGS) -o $@ sumpdos.o
	- ( cd ../../bin ; ln -sf ../src/tools/$@ . )

decay.x: decay.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ decay.o \
	       $(OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../src/tools/$@ . )

sum_sgm.x: sum_sgm.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ sum_sgm.o \
	       $(OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../src/tools/$@ . )

sax2qexml.x: sax2qexml.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ sax2qexml.o \
	       $(OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../src/tools/$@ . )
	- ( cd ../../bin ; ln -sf ../src/tools/sax2qexml . )

write_ham.x: write_ham.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ write_ham.o \
	       $(OBJS) $(LIBOBJS) $(LIBS)
	- ( cd ../../bin ; ln -sf ../src/tools/$@ . )

clean:
	rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x sumpdos

# dependencies
include make.depend

