#!/bin/bash

total=0
COUNT=1000
GAME="python2 ./game.py"
SOLVER=./solver
RESULT=./result

mkdir -p $RESULT

for i in `seq 0 $COUNT`; do
  $GAME $i $SOLVER $RESULT/moves.$i > $RESULT/result.$i
done

# vim: set et sw=2 ts=2:
