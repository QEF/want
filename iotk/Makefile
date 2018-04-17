
#==============================
# IOTK input-output tool kit 
#==============================

#system dipendent stuff
include ../make.sys

# specific redefinition
# FFLAGS=$(FFLAGS_NOOPT)

OBJS = \
    iotk_01_base.o  \
    iotk_02_attr_interf.o  \
    iotk_03_dat_interf.o  \
    iotk_04_error_interf.o  \
    iotk_05_files_interf.o  \
    iotk_06_fmt_interf.o  \
    iotk_07_misc_interf.o  \
    iotk_08_str_interf.o  \
    iotk_09_unit_interf.o  \
    iotk_10_scan_interf.o  \
    iotk_11_write_interf.o  \
    iotk_12_xtox_interf.o  \
    iotk_13_module.o  \
    iotk_14_attr+CHARACTER.o  \
    iotk_15_attr+COMPLEX.o  \
    iotk_16_attr+REAL.o  \
    iotk_17_attr+INTEGER.o  \
    iotk_18_attr+LOGICAL.o  \
    iotk_19_dat+CHARACTER.o  \
    iotk_20_dat+COMPLEX.o  \
    iotk_21_dat+REAL.o  \
    iotk_22_dat+INTEGER.o  \
    iotk_23_dat+LOGICAL.o  \
    iotk_24_error.o  \
    iotk_25_files.o  \
    iotk_26_fmt.o  \
    iotk_27_misc.o  \
    iotk_28_str.o  \
    iotk_29_unit.o  \
    iotk_30_scan.o  \
    iotk_31_write.o  \
    iotk_32_xtox.o  
    
#----------------------------------------------------
    
all : libiotk.a
   
libiotk.a : $(OBJS)
	$(AR) $(ARFLAGS) $@ $?
    
clean:
	rm -rf *.o *.a *core *.F90 *.mod *.d work.pc work.pcl

# dependencies
include make.depend
