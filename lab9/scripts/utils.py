from subprocess import run


def choose_mod(N, K, P):
    return run(
        "../lab6/parhaskell",
        input=f"1\n {N} {K} {P}\n",
        capture_output=True,
        text=True,
    ).stdout.strip()
