#! /usr/bin/env bash

# This awk incantation exists with code 42 if there are duplicates, and 0 otherwise
grep error_ src/error.ml | awk 'a[$7]++{print $7; exit 42}'

ret=$?

if [ $ret == 42 ]; then
	echo "there are duplicate errors!"
elif [ $ret == 0 ]; then
	echo "there are no duplicate error codes!"
else
	echo "something else failed"
fi

exit $ret
