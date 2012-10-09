#!/bin/sh
file=$1
if [ "$file" = "" ]
then
   file="../lib/user/crash.txt"
   if [ ! -f $file ]
   then
      echo "No crashdump mentioned, ../lib/user/crash.txt doesn't exist."
      exit 1
   fi
fi
exe=$2
if [ "$exe" = "" ]
then
   echo "no exe mentioned?"
   exit 1
fi
for i in `grep address $file | cut -d"s" -f4 | grep -v 00000000`
do
   echo $i `i586-cygwin32-addr2line --functions --exe=$exe $i`
done
