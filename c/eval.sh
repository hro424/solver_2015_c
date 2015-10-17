#!/bin/bash

total=0
count=1000
SOLVER=./solver

for i in `seq 0 $count`; do
  score=`python2 ./collatzris.py $i $SOLVER moves.txt 0 | grep score | cut -f2 -d':'`
  total=`expr $total + $score`
  echo "[$i] score $score total $total"
done
