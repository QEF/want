! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 

#ifdef __MYARCHITECTURE 
#   undefine __MYARCHITECTURE
#endif
#ifdef __MYCOMPILER
#   undefine __MYCOMPILER
#endif

!
! Architectures / Operating systems
!

#ifdef __LINUX
#    define __MYARCHITECTURE "LINUX"
#endif

#ifdef __LINUX64
#    define __MYARCHITECTURE "LINUX 64bit" 
#endif

#ifdef __AIX
#    define __MYARCHITECTURE "IBM AIX" 
#endif

#ifdef __SGI
#    define __MYARCHITECTURE "SGI" 
#endif

#ifdef __SGI64
#    define __MYARCHITECTURE "SGI 64bit" 
#endif

#ifdef __ALPHA
#    define __MYARCHITECTURE "ALPHA" 
#endif

#ifdef __SUN
#    define __MYARCHITECTURE "SUN" 
#endif

#ifdef __X1
#    define __MYARCHITECTURE "CRAY X1" 
#endif

#ifdef __MAC
#    define __MYARCHITECTURE "MAC" 
#endif


!
! Compilers
!
#ifdef __INTEL
#    define __MYCOMPILER "INTEL" 
#endif

#ifdef __PGI
#    define __MYCOMPILER "PGI" 
#endif

#ifdef __G95
#    define __MYCOMPILER "GNU G95" 
#endif

