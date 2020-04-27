PL2_DIR=${HOME}/Workspace/pl2

cd ${PL2_DIR}/lab6
ghc-8.8.3 src/parhaskell.hs -feager-blackholing -threaded
for package in monad-par parallel; do
    echo "Package=${package}"
    export PACKAGE=${package}
    mkdir -p outputs/${package}
    for T in 1 2 4 8 16 32 64 128 256 512 1024; do
        echo "  T=${T}"
        for N in 1 2 4 8; do
            echo "    N=${N}"
            scripts/input.py ${T} | src/parhaskell +RTS -N${N} -s 1>/dev/null 2>outputs/${package}/T-${T}_N-${N}.out
        done
    done
done
rm -f src/parhaskell src/parhaskell.hi src/parhaskell.o
