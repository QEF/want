#!/bin/bash 
#
# Check to results of selected tests
#================================================================
#
MANUAL=" Usage
   check.sh [<file1>...<fileN>]

 Report a simplified \'diff\' of the selected <file>'s
 and their reference counterparts in the dir ./Reference .
 The script should therefore be run in a specific Test dir.

"
#
#================================================================
#

LIST=$*

if [ ! -d "./Reference" ] ; then 
   echo "error: Reference directory not found"
   exit 1
fi


for file in $LIST
do
    
   echo 
   echo "######################### file: $file "
   if [ ! -e $file ] ; then 
      echo "   $file not found: skipped"  
   else
      diff $file Reference/$file | 
           grep -v "Date " |
           grep -v CPU |
           grep -v secs
   fi
done
exit 0






