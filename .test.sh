#!/bin/bash
for i in $(find progs -name *.cre); do
	echo -en "Testing $i: "
	output=$(./interpreter < $i 2>&1 > /dev/null)
	p=$?
	if [ "$p" -eq 0 ]; \
		then echo -e "\x1b[32;1mSUCCESS\x1b[m"; \
		else echo -e "\x1b[31;1mFAILURE\x1b[m" $output; fi;
done

