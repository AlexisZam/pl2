#!/bin/bash

BEF=~/opt/Befunge-93/bin/bef

make
for test in $(ls test/*bf)
do
    echo $test | cut -d / -f 2 | cut -d . -f 1
    ./befunge93 $test > befunge93.out 2> befunge93.err 3> befunge93.stack
    $BEF -s bef.stack $test > bef.out 2> bef.err
    sed -i 1d bef.out
    diff befunge93.out bef.out
    diff befunge93.err bef.err
    diff befunge93.stack bef.stack
    sleep 1
done
rm -f *out *err *stack
make clean