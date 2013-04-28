#!/bin/bash

# Converts the whole source tree to UNIX line endings
# to-dos.sh does the opposite

find . -type f > /tmp/from-dos-list

for file in `cat /tmp/from-dos-list` ; do
	if file $file | cut -d: -f2 | grep text >/dev/null 2>&1 ; then
		fromdos < $file > $file.new
		chmod --reference=$file $file.new
		mv $file.new $file
	fi
done

rm /tmp/from-dos-list
