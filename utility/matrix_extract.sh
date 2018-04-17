#!/bin/bash

#
# utility to extrac minors from a given matrix 
#
# the input format of the matrix is consistent with the output
# of the HAMILTONIAN routine, i.e. matrix is written columwise
# separating each column by a blank line
#

MANUAL="\

 Usage:

   matrix_extract.sh   name   r1 r2   c1 c2

   name           file with matrix data
   r1  r2         the rows from r1 to r2 will be extracted
   c1  c2         the columns from c1 to c2 will be extracted

   The output is on stdout
"
#
# The possibility of extract a non-sequential series of rows
# or columns is not yes implemented
#

if [ "$#" != 5 ] ; then echo " Invalid number of input parameters " ; echo "$MANUAL" ; \
                   exit ; fi

#
#===================================================================
#

#
# reading parameters
name=" $1 "
r1=" $2 "
r2=" $3 "
c1=" $4 "
c2=" $5 "

if [ ! -e $name ] ; then echo " file $name does NOT exist "; exit 1 ; fi

#
# get the dimension of the matrix
data=`cat $name`
dimx=`echo "$data" | awk '{ if (NR == 1) print $1}'`

if [ $r1 -gt $dimx ] ; then echo " R1 = $r1 is larger than DIMX = $dimx " ; exit 1 ; fi
if [ $r2 -gt $dimx ] ; then echo " R2 = $r2 is larger than DIMX = $dimx " ; exit 1 ; fi
if [ $c1 -gt $dimx ] ; then echo " C1 = $c1 is larger than DIMX = $dimx " ; exit 1 ; fi
if [ $c2 -gt $dimx ] ; then echo " C2 = $c2 is larger than DIMX = $dimx " ; exit 1 ; fi

#
# eliminates first line of the file
len=`echo "$data" | wc -l`
data_tmp=`echo "$data" | awk '{ if (NR != 1) print }'`
data=`echo "$data_tmp"`


#
# main AWK loop
echo $(( $r2 - $r1 + 1)) "  " $(( $c2 - $c1 + 1))
echo "$data" | awk "{ icol = int( (NR-1)/($dimx+1) )+1 ; 
                  irow = (NR-1)%($dimx+1) ; 
                  if ( irow == 0 ) print ;
                  if ( irow >= $r1 && irow <= $r2 && icol >= $c1 && icol <= $c2 ) print }"


exit 0


