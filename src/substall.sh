#!/bin/sh
val1=$1
val2=$2
echo subst $val1 with $val2
for i in `grep -l "$val1" *.c *.h`
do
   sed -e"s/$val1/$val2/g" $i > $i.new
   mv $i.new $i
   echo $i
done
