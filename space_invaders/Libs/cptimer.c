/*
  Copyright (C) 2002 FPMD group
  This file is distributed under the terms of the
  GNU General Public License. See the file `License'
  in the root directory of the present distribution,
  or http://www.gnu.org/copyleft/gpl.txt .
*/

#include<stdio.h>
#include<time.h>
#include<ctype.h>
#include<sys/types.h>
#include<sys/time.h>


#if defined __T3E | defined __ABSOFT
#  define CCLOCK CCLOCK
#endif
#if defined __SGI | defined __FUJITSU | defined __SX4 | defined __INTEL | defined __LAHEY | defined __SX6 | defined SUN | defined __ALTIX
#  define CCLOCK cclock_
#endif
#if defined __PGI
#  if defined __GNU_LINK
#     define CCLOCK cclock__
#  else
#     define CCLOCK cclock_
#  endif
#endif
#if defined __AIX | defined __HP 
#  define CCLOCK cclock
#endif
#if defined __ALPHA 
#  if defined __LINUX
#    define CCLOCK cclock_
#  else
#    define CCLOCK cclock_
#  endif
#endif


double ELAPSED_SECONDS()
{
  static time_t tstart, tend;
  static int first = 1;
  double sec;
  time(&tend);
  if( first ) {
    tstart = tend;
    first = 0;
  }
  sec = difftime( tend, tstart );
  return sec;
}


double CCLOCK()
/* Restituisce i secondi trascorsi dalla chiamata al timer rest */
{

#if defined __T3E

/*  return (double)(rtclock() * 3.333e-6 / 2.); */
    return (double)( ( _rtc() / (double)CLK_TCK ) );

#else

    struct timeval tmp;
    double sec;
    gettimeofday( &tmp, (struct timezone *)0 );
    sec = tmp.tv_sec + ((double)tmp.tv_usec)/1000000.0;
    return sec;

#endif

}
