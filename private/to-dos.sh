#!/bin/bash

# Converts the whole source tree to DOS line endings
# from-dos.sh does the opposite

find . -type f > /tmp/to-dos-list

for file in `cat /tmp/to-dos-list` ; do
	if file $file | cut -d: -f2 | grep text >/dev/null 2>&1 ; then
		todos < $file > $file.new
		chmod --reference=$file $file.new
		mv $file.new $file
	fi
done

rm /tmp/to-dos-list
