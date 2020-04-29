BEF_DIR=${HOME}/opt/Befunge-93
PL2_DIR=${HOME}/Workspace/pl2

cd ${PL2_DIR}/lab1/src
make
for test in $(ls ../tests/quine-2.bf); do
    echo $(basename ${test})
    ${BEF_DIR}/bin/bef -s bef.stack ${test} >bef.out 2>bef.err
    ./vm ${test} >vm.out 2>vm.err 3>vm.stack
    sed -i 1d bef.out
    diff -q bef.out vm.out
    diff -q bef.err vm.err
    diff -q bef.stack vm.stack
done
rm -f *out *err *stack
make clean
