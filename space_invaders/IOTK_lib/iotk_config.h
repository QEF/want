
!------------------------------------------------------------------------------!
! CONFIGURATION FILE FOR IOTK 0.2.10
!------------------------------------------------------------------------------!
! The following lines map some commonly defined system macro to the internal
! iotk macros.
! Iotk macros which are not defined take their default values.
! See the manual for a list of iotk macros.

#ifdef __AIX
#   define __IOTK_BINARY_FORMAT "IBM-SP/XLF"
#   define __IOTK_LOGICAL1 1
#   define __IOTK_LOGICAL2 2
#   define __IOTK_LOGICAL3 4
#   define __IOTK_LOGICAL4 8
#   define __IOTK_INTEGER1 4
#   define __IOTK_INTEGER2 2
#   define __IOTK_INTEGER3 1
#   define __IOTK_INTEGER4 8
#   define __IOTK_REAL1    8
#   define __IOTK_REAL2    4
#   define __IOTK_REAL3    16
#endif

#ifdef __LINUX
#   define __IOTK_BINARY_FORMAT "PC-LINUX/IFC"
#   define __IOTK_LOGICAL1 1
#   define __IOTK_LOGICAL2 2
#   define __IOTK_LOGICAL3 4
#   define __IOTK_LOGICAL4 8
#   define __IOTK_INTEGER1 4
#   define __IOTK_INTEGER2 2
#   define __IOTK_INTEGER3 1
#   define __IOTK_INTEGER4 8
#   define __IOTK_REAL1    8
#   define __IOTK_REAL2    4
#   define __IOTK_REAL3    16
#   define __IOTK_AVOID_EMPTY_FILES
#endif

#ifdef __SGI
#   define __IOTK_BINARY_FORMAT "SGI-ORIGIN"
#   define __IOTK_LOGICAL1 1
#   define __IOTK_LOGICAL2 2
#   define __IOTK_LOGICAL3 4
#   define __IOTK_LOGICAL4 8
#   define __IOTK_INTEGER1 4
#   define __IOTK_INTEGER2 2
#   define __IOTK_INTEGER3 1
#   define __IOTK_INTEGER4 8
#   define __IOTK_REAL1    8
#   define __IOTK_REAL2    4
#   define __IOTK_REAL3    16
#   define __IOTK_AVOID_EMPTY_FILES
#endif

#ifdef __PARA
#  define __IOTK_MPI_ABORT
#endif

