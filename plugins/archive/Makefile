#
#===============================
# WanT package
#===============================
#

#
# Package List
#
include ./package.list

all: $(tarball_vdw_wf) 

$(tarball_vdw_wf):
	wget -O $(tarball_vdw_wf) $(url_vdw_wf) > /dev/null 2>&1
	@if test "`echo $$?`" -ne "0" ; then \
		curl -o $(tarball_vdw_wf) $(url_vdw_wf) > /dev/null 2>&1 ; \
	fi
	@if test ! -s $(tarball_vdw_wf) ; then \
		echo "*** Unable to download vdW-WF. Test whether curl or wget is installed and working," ; \
		echo "*** if you have direct access to the internet." ; \
		echo "*** If not, copy $(tarball_vdw_wf) into extlibs/archive/" ; \
	exit 1 ; fi

#
# cleaning
#
clean:

clean_all:
	if test -s $(tarball_vdw_wf)    && test "$(keep_vdw_wf)"    != "yes" ;  then rm $(tarball_vdw_wf)  ; fi

