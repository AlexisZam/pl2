BEF_DIR=${HOME}/opt/Befunge-93
PL2_DIR=${HOME}/Workspace/pl2

cd ${PL2_DIR}/lab1/src
make >/dev/null
for test in $(ls ../*.bf); do
    echo $(basename ${test})
    echo "  bef"
    # ${BEF_DIR}/bin/bef -s bef.stack ${test} >bef.out 2>bef.err
    echo "  vm"
    ./vm ${test} 2>vm.err
    sed -i 1d bef.out
    diff -q bef.out vm.out
    diff -q bef.err vm.err
done
rm -f *out *err *stack
make clean >/dev/null
