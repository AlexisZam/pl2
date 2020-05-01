BEF_DIR=${HOME}/opt/Befunge-93
PL2_DIR=${HOME}/Workspace/pl2

cd ${PL2_DIR}/lab1/src
make >/dev/null
for test in $(ls ../eg/*.bf); do
    input=${test%.*}.in
    [ -f ${input} ] || input=/dev/stdin
    echo $(basename ${test})
    echo "  bef"
    ${BEF_DIR}/bin/bef -s bef.stack ${test} <${input} >bef.out 2>bef.err
    echo "  vm"
    ./vm ${test} <${input} >vm.out 2>vm.err 3>vm.stack
    sed -i 1d bef.out
    diff -q *.out
    [ $? -eq 0 ] || exit 1
    diff -q *.err
    [ -f "bef.stack" ] && diff -q *.stack
done
rm -f *out *err *stack
make clean >/dev/null
