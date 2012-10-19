#!/bin/bash 

#
# gets a eig file and compute the DOS
#

file=$1

emin=-20.0
emax=10.0
ne=2000
delta=0.100
#conv=27.2116
conv=1.0
#fermi=-1.8920
fermi=-5.1874
#fermi=0.0
nkpts=8

if [ ! -e "$file" ] ; then echo "ERROR: file does not exist" ; exit 1 ; fi

#
# get eigenvalues

#eig_list=`cat $file | awk '{ 
#                   if (match($1,"#")) next
#                   if (NF > 1) {
#                      for( i=2; i<=NF; i++) printf "%15.9f ", $i
#                      printf "\n"
#                      exit
#                   }
#                 }'`

eig_list=`cat $file |  grep -v Fermi`


dos=` echo $eig_list | awk -v emin=$emin   -v emax=$emax \
                           -v ne=$ne       -v delta=$delta \
                           -v fermi=$fermi -v conv=$conv \
                           -v nkpts=$nkpts  \
               ' BEGIN{ PI=3.14159266 }
                 { 
                      #
                      # build grid
                      #
                      for( i=1; i<= ne ; i++ ) {
                         egrid[i]=emin + (emax-emin) / (ne-1.0) * (i-1.0)
                      }

                      #
                      # read eigs
                      #
                      for( ib=1; ib<= NF ; ib++ ) {
                         eig[ib]=conv*$(ib)-fermi
                      }
                      nbnd=NF
                      
                      #
                      # build dos
                      #
                      for ( i=1; i<= ne ; i++ ) {
                         #
                         dos[i]=0.0
                         #
                         for ( ib=1; ib<=nbnd; ib++) {
                             #
                             dd=(egrid[i]-eig[ib]) / delta
                             #
                             #
                             # lorentzian
                             cost = 1.0 / ( PI * delta )
                             rtmp = cost / ( 1.0 + dd^2 )
                             #
                             ## gaussian
                             #cost = 1.0 / ( sqrt( PI ) * delta )
                             #
                             #if ( dd^2 < 100.0 ) {
                             #    rtmp = cost * exp( - dd^2 )
                             #} else { 
                             #    rtmp = 0.0 
                             #}
                             #
                             dos[i]+= rtmp
                         }
                         #
                         printf "%15.9f   %15.9f \n", egrid[i], dos[i]/nkpts
                      }
                 }'`


echo "$dos"

exit 0

