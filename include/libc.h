
/*
  Copyright (C) 2002-2004 PWSCF,FPMD,CPV groups
  This file is distributed under the terms of the
  GNU General Public License. See the file `License'
  in the root directory of the present distribution,
  or http://www.gnu.org/copyleft/gpl.txt .
*/

/*
  ====================================================
     C-LIBS machine specific precompiler directives
  ====================================================
*/


#if defined __T3E || defined __ABSOFT
#  define __FC_FUNC(name,NAME) NAME
#  define __FC_FUNC_(name,NAME) NAME
#endif

#if defined __SGI || defined __FUJITSU || defined __SX4 || defined __INTEL || defined __LAHEY || defined __SX6 || defined __SUN || defined __ALTIX
#  define __FC_FUNC(name,NAME) name ## _
#  define __FC_FUNC_(name,NAME) name ## _
#endif

#if defined __PGI
#  if defined __GNU_LINK
#    define __FC_FUNC(name,NAME) name ## _
#    define __FC_FUNC_(name,NAME) name ## __
#  else
#    define __FC_FUNC(name,NAME) name ## _
#    define __FC_FUNC_(name,NAME) name ## _
#  endif
#endif

#if defined __G95
#  define __FC_FUNC(name,NAME) name ## _
#  define __FC_FUNC_(name,NAME) name ## __
#endif

#if defined __AIX || defined __HP || defined __MAC
#  define __FC_FUNC(name,NAME) name
#  define __FC_FUNC_(name,NAME) name
#endif

#if defined __ALPHA && !defined __LINUX64
#  define __FC_FUNC(name,NAME) name ## _
#  define __FC_FUNC_(name,NAME) name ## _
#endif

#if defined __ALPHA && defined __LINUX64
#  define __FC_FUNC(name,NAME) name ## _
#  define __FC_FUNC_(name,NAME) name ## __
#endif


