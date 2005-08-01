! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 

#ifdef __ARCHITECTURE 
#   undefine __ARCHITECTURE
#endif
#ifdef __COMPILER
#   undefine __COMPILER
#endif

!
! Architectures / Operating systems
!

#ifdef __LINUX
#    define __ARCHITECTURE "LINUX"
#endif

#ifdef __LINUX64
#    define __ARCHITECTURE "LINUX 64bit" 
#endif

#ifdef __AIX
#    define __ARCHITECTURE "IBM AIX" 
#endif

#ifdef __SGI
#    define __ARCHITECTURE "SGI" 
#endif

#ifdef __SGI64
#    define __ARCHITECTURE "SGI 64bit" 
#endif

#ifdef __ALPHA
#    define __ARCHITECTURE "ALPHA" 
#endif

#ifdef __SUN
#    define __ARCHITECTURE "SUN" 
#endif

#ifdef __X1
#    define __ARCHITECTURE "CRAY X1" 
#endif

#ifdef __MAC
#    define __ARCHITECTURE "MAC" 
#endif


!
! Compilers
!
#ifdef __INTEL
#    define __COMPILER "INTEL" 
#endif

#ifdef __PGI
#    define __COMPILER "PGI" 
#endif

#ifdef __G95
#    define __COMPILER "GNU G95" 
#endif

