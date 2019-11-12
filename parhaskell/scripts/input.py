#!/usr/bin/env python3.8

from argparse import ArgumentParser
from random import randint, randrange, seed

from sympy import randprime

parser = ArgumentParser(add_help=False)
parser.add_argument("T", type=int)
args = parser.parse_args()

seed(a=42)

print(args.T)
for _ in range(args.T):
    P = randprime(2, 10 ** 9 + 1)
    N = randrange(0, P)
    K = randint(0, N)
    print(N, K, P)
