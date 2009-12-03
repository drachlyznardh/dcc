#!/bin/bash
for i in input/*; do
	echo -en "Testing $i: "
	./interpreter < $i > /dev/null
	p=$?
	if [ "$p" -eq 0 ]; then echo -e "\x1b[32;1mSUCCESS\x1b[m"; else echo -e "\x1b[31;1mFAILURE\x1b[m"; fi;
done

