from subprocess import run
from os.path import dirname, join


def choose_mod(N, K, P):
    args = join(dirname(__file__), "parhaskell")
    return run(
        args, input=f"1\n {N} {K} {P}\n", capture_output=True, text=True,
    ).stdout.strip()
