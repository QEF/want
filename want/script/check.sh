#! /bin/bash 
#
# Check to results of selected tests
#================================================================
#
MANUAL=" Usage
   check.sh [-h] [-v] [<file1>...<fileN>]

 Report a simplified 'diff' of the selected <file>'s
 and their reference counterparts in the dir ./Reference .
 The script should therefore be run in a specific Test dir.

"
#
#================================================================
#

VERBOSITY=low

while getopts :hv OPT
do
  case $OPT in
  (v) VERBOSITY="high" ; shift 1;;
  (h) echo "$MANUAL" ; exit 0 ;;
  (:) echo "error: $OPTARG requires an argument" ; exit 1 ;;
  (?) echo "error: unkwown option $OPTARG" ; exit 1 ;;
  esac
done

#
# account for environment variables
test -e ../environment.conf && . ../environment.conf
if [ "$VERBOSITY_LEVEL" = "high" ] ; then
   VERBOSITY=$VERBOSITY_LEVEL
fi
echo "check VERBOSITY: $VERBOSITY"


#
# set file list
LIST=$*
if [ -z "$LIST" ] ; then echo "$MANUAL" ; exit 0 ; fi


#
# other checks
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
      OUT="$( diff $file Reference/$file | 
           grep -v "Date " |
           grep -v CPU |
           grep -v secs  )"

      if [ "$VERBOSITY" = "high" ] ; then
         echo "$OUT"
      else
         echo "$OUT"                     | 
              grep "^[-<>]"              |
              grep -v  "Iteration ="     |
              grep -vi "Center"          |
              grep -v  "Omega variation" |
              grep -v  "k point"         |
              grep -v  "Fermi energy"    |
              grep -v  "^[<>] *! "       |
              uniq
      fi
   fi
done
exit 0






