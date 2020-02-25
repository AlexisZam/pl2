#!/usr/bin/env python3

from bs4 import BeautifulSoup
from requests import Session
from scipy.special import comb
from sys import argv

try:
    url = argv[1]
except:
    print(f"Usage: {argv[0]} url")
    exit(1)

s = Session()

for i in range(10):
    r = s.get(url)
    soup = BeautifulSoup(markup=r.text, features="lxml")
    N = int(soup.find(attrs={"id": "N"}).text)
    K = int(soup.find(attrs={"id": "K"}).text)
    P = int(soup.find(attrs={"id": "P"}).text)
    print(f"Round {i + 1}, C({N}, {K}) modulo {P}")
    answer = comb(N, K, exact=True) % P  # TODO: call parhaskell.hs
    print(f"Answer: {answer}")
    r = s.post(url, data={"answer": answer})
    soup = BeautifulSoup(markup=r.text, features="lxml")
    right = soup.find(attrs={"class": "right"}).text
    print(right)
