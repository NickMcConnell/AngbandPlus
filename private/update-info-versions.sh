#!/bin/bash

# Updates lib/edit/*.txt files versions

if [ "$1" == "" ] ; then
	echo "Usage: `basename $0` <version>"
	exit 1
fi

if ! test -f lib/edit/limits.txt ; then
	echo "File lib/edit/limits.txt not found."
	echo "Run this script from the PosBand sources root directory!"
	exit 1
fi

OLDVERS=`grep V: lib/edit/limits.txt | grep -o '[0-9]*\.[0-9]*\.[0-9]*' | sed -e 's/\./\\\./g'`
NEWVERS=`echo $1 | sed -e 's/\./\\\./g'`

for file in lib/edit/*.txt ; do
	sed -e "s/$OLDVERS/$NEWVERS/" < $file > $file.new
	mv $file.new $file
done
