#!/bin/bash

BEF_DIR=~/opt/Befunge-93

make

for test in $(ls ../testsuites/*.bf) # $BEF_DIR/eg/*.bf 
do
    echo $(basename $test)
    $BEF_DIR/bin/bef -s bef.stack $test > bef.out 2> bef.err
    ../bin/befunge93 $test > befunge93.out 2> befunge93.err 3> befunge93.stack
    ../bin/befunge93+ $test > befunge93+.out 2> befunge93+.err 3> befunge93+.stack
    sed -i 1d bef.out
    diff -q bef.out befunge93.out
    diff -q bef.out befunge93+.out
    diff -q bef.err befunge93.err
    diff -q bef.err befunge93+.err
    diff -q bef.stack befunge93.stack
    diff -q bef.stack befunge93+.stack
done

rm -f *out *err *stack
make clean
