#!/bin/bash

BEF_DIR=~/opt/Befunge-93

make
for test in $(ls $BEF_DIR/eg/*.bf test/*.bf)
do
    echo $test
    $BEF_DIR/bin/bef -s bef.stack $test  2> bef.err > bef.out
    ./befunge93 $test 2> befunge93.err 3> befunge93.stack > befunge93.out
    sed -i 1d bef.out
    diff bef.out befunge93.out
    diff bef.err befunge93.err
    diff bef.stack befunge93.stack
    sleep 5
done
rm -f *out *err *stack
make clean
