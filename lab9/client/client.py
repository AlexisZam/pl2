#!/usr/bin/env python3.8

from bs4 import BeautifulSoup
from requests import Session
from subprocess import check_output
from sys import argv


def choose_mod(N, K, P):
    return check_output(
        "../../lab6/parhaskell", input=f"1\n {N} {K} {P}\n", text=True
    ).strip()


n_iter = 10

try:
    url = argv[1]
except:
    print(f"Usage: {argv[0]} url")
    exit(1)


s = Session()

for i in range(n_iter):
    r = s.get(url)
    soup = BeautifulSoup(markup=r.text)
    N = int(soup.find(attrs={"id": "N"}).text)
    K = int(soup.find(attrs={"id": "K"}).text)
    P = int(soup.find(attrs={"id": "P"}).text)
    print(f"Round {i + 1}, C({N}, {K}) modulo {P}")

    answer = choose_mod(N, K, P)
    print(f"Answer: {answer}")

    r = s.post(url, data={"answer": answer})
    soup = BeautifulSoup(markup=r.text)
    right = soup.find(attrs={"class": "right"}).text
    print(right)
