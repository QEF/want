! 
! Copyright (C) 2004 WanT Group
! 
! This file is distributed under the terms of the 
! GNU General Public License. See the file `License' 
! in the root directory of the present distribution, 
! or http://www.gnu.org/copyleft/gpl.txt . 
! 

!
! Architectures / Operating systems
!

#ifdef __AIX
#    define __MYARCHITECTURE "IBM rs/6000" 
#endif

#ifdef __LINUX
#    define __MYARCHITECTURE "Intel/Amd 32-bit, Linux"
#endif

#ifdef __LINUX64
#    define __MYARCHITECTURE "Intel/Amd/Alpha 64-bit, Linux" 
#endif

#ifdef __MAC
#    define __MYARCHITECTURE "Mac powerPC, OS-X" 
#endif

#ifdef __HP
#    define __MYARCHITECTURE "HP-compaq PA-RISC" 
#endif

#ifdef __ALTIX
#    define __MYARCHITECTURE "SGI Altix 350/3000, Linux"
#endif

#ifdef __ORIGIN
#    define __MYARCHITECTURE "SGI Origin 2k/3k"
#endif

#ifdef __ALPHA
#    define __MYARCHITECTURE "HP-compaq alpha" 
#endif

#ifdef __FUJ64
#    define __MYARCHITECTURE "Fujitsu VPP5000 (vector mach)" 
#endif

#ifdef __SX6
#    define __MYARCHITECTURE "Nec sx-6 (vector mach)" 
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

