#!/bin/bash

total=0
count=1000
GAME="python2 ./game.py"
SOLVER=./solver

if [ -e $1 ]; then
  rm $1
fi

for i in `seq 0 $count`; do
  $GAME $i $SOLVER moves.txt >> $1
  echo >> $1
#  score=`$GAME $i $SOLVER moves0.txt | grep score | cut -f 2 -d ':'`
#  total=`expr $score + $total`
#  echo "$i $score	$total"
done
echo $total

# vim: set et sw=2 ts=2:
