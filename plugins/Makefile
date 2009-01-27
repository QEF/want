#
#===============================
# WanT package
#===============================
#
include ../make.sys


#
# MAIN target
#
all: $(PLUGINS_LIBS)


libnetcdf:
	 if test -d netcdf ;    then ( cd netcdf;    $(MAKE) ) ; fi

libetsf_io:
	 if test -d etsf_io ;   then ( cd etsf_io;   $(MAKE) ) ; fi

#
# cleaning
#
clean wash:
	 if test -d netcdf ;    then ( cd netcdf;    $(MAKE) $@ ) ; fi
	 if test -d etsf_io ;   then ( cd etsf_io;   $(MAKE) $@ ) ; fi

