#
#===============================
# WanT package
#===============================
#
include ../make.sys


#
# MAIN target
#
all: libnetcdf libetsf_io


#
# lib netcdf
#
libnetcdf: libnetcdf_$(NETCDF_LIBS_SWITCH)

libnetcdf_internal:
	 if test -d netcdf ;    then ( cd netcdf;    $(MAKE) ) ; fi

libnetcdf_external:
libnetcdf_:
	
#
# lib etsf_io
#
libetsf_io: libnetcdf libetsf_io_$(ETSF_IO_LIBS_SWITCH)

libetsf_io_internal:
	 if test -d etsf_io ;   then ( cd etsf_io;   $(MAKE) ) ; fi

libetsf_io_external:
libetsf_io_:

#
# cleaning
#
clean wash:
	 if test -d netcdf ;    then ( cd netcdf;    $(MAKE) $@ ) ; fi
	 if test -d etsf_io ;   then ( cd etsf_io;   $(MAKE) $@ ) ; fi

