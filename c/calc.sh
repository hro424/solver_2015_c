#!/bin/bash

RESULT=./result

SCORES=`grep score $RESULT/result.* | cut -f 3 -d ':'`
TOTAL=0

for s in $SCORES; do
	TOTAL=`expr $s + $TOTAL`
done

echo $TOTAL
