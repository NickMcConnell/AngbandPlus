#!/bin/sh
#
# this adds a delete.me file to every dir in lib/
#
for name in `find lib/ -name "*" -type d`
do
   echo "this is just a place holder so the directory isn't empty and gets lost during zipping!" > $name/delete.me;
done
