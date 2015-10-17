#!/bin/bash

total=0
count=1000
GAME="python2 ./game.py"
SOLVER=./solver

for i in `seq 0 $count`; do
  score=`$GAME $i $SOLVER moves.txt 0 | grep score | cut -f2 -d':'`
  total=`expr $total + $score`
  echo "[$i]\tscore $score\ttotal $total"
done
