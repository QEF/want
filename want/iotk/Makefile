
#==============================
# IOTK input-output tool kit 
#==============================

#system dipendent stuff
include ../make.sys

# specific redefinition
# FFLAGS=$(FFLAGS_NOOPT)

OBJS = \
iotk_01_base.o \
iotk_02_unit_list.o \
iotk_03_attr_interf.o \
iotk_04_dat_interf.o \
iotk_05_error_interf.o \
iotk_06_files_interf.o \
iotk_07_fmt_interf.o \
iotk_08_misc_interf.o \
iotk_09_str_interf.o \
iotk_10_scan_interf.o \
iotk_11_stream_interf.o \
iotk_12_tool_interf.o \
iotk_13_unit_interf.o \
iotk_14_write_interf.o \
iotk_15_xtox_interf.o \
iotk_16_module.o \
iotk_17_attr+CHARACTER.o \
iotk_18_attr+COMPLEX.o \
iotk_19_attr+REAL.o \
iotk_20_attr+INTEGER.o \
iotk_21_attr+LOGICAL.o \
iotk_22_dat+CHARACTER.o \
iotk_23_dat+COMPLEX.o \
iotk_24_dat+REAL.o \
iotk_25_dat+INTEGER.o \
iotk_26_dat+LOGICAL.o \
iotk_27_error.o \
iotk_28_files.o \
iotk_29_fmt.o \
iotk_30_misc.o \
iotk_31_str.o \
iotk_32_stream.o \
iotk_33_scan.o \
iotk_34_tool.o \
iotk_35_unit.o \
iotk_36_write.o \
iotk_37_xtox.o 

BIN=\
iotk.x


#----------------------------------------------------
    
all : libiotk.a $(BIN)
   
#
# Lib
#
libiotk.a : $(OBJS)
	$(AR) $(ARFLAGS) $@ $?

#
# Executable
#
iotk.x: iotk.o $(OBJS)
	$(LD) -o $@ iotk.o $(OBJS) $(LDFLAGS)
	- ( cd ../bin ; ln -sf ../iotk/iotk . ; \
	                ln -sf ../iotk/iotk.x . )
    
clean:
	rm -rf *.x *.o *.a *core *.F90 *.mod *.d work.pc work.pcl

# dependencies
include make.depend
