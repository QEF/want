#
# Makefile for transfer.f
#

#-- Defines for CRAY T90
FC=ifc
OPTS = -w -c -r8 -tpp6 -O3 -save
ATLAS=/usr/local/cicaia/intel/lib/ATLAS/OLD_NODE
LAPACK=/usr/local/cicaia/intel/lib
LIBS= -L$(LAPACK) -llapack -L$(ATLAS) -lf77blas -latlas /usr/lib/libpthread.a

#-----  End of configurable parameters -----

FFLAGS = $(OPTS) 
LDFLAGS = ifc

SRCS = Sconductor.f90 Sgreena.f90 Stransfera.f90 Sgreenb.f90 Stransferb.f90 \
SreadH.f90 setv0.f90

PRJS = $(SRCS:.f=.prj)
OBJS = $(SRCS:.f=.o)

Sconductor.x: $(OBJS) 
	$(FC) -o $@ $(OBJS) $(LIBS) 

clean:
	rm -f *.[ou] *.x *.dvi *.lst *.log core *.prj

cleanall:
	rm -f *.[ou] *.x *.dvi *.lst *.log core *.prj fort.* va.job.*

check: $(PRJS)
	ftnchek $(PRJS)
