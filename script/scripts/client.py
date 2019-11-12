#!/usr/bin/env python3.8

from argparse import ArgumentParser

from bs4 import BeautifulSoup
from requests import Session

from utils import choose_mod

parser = ArgumentParser(add_help=False)
parser.add_argument(
    "url",
    nargs="?",
    default="https://courses.softlab.ntua.gr/pl2/2019b/exercises/combmod.php",
)
args = parser.parse_args()

session = Session()

i = 1
while True:
    response = session.get(args.url)
    soup = BeautifulSoup(markup=response.text, features="lxml")
    N = int(soup.find(id="N").text)
    K = int(soup.find(id="K").text)
    P = int(soup.find(id="P").text)
    print(f"Round {i}, C({N}, {K}) modulo {P}")

    answer = choose_mod(N, K, P)
    print(f"Answer: {answer}")

    response = session.post(args.url, data={"answer": answer})
    soup = BeautifulSoup(markup=response.text, features="lxml")
    right = soup.find(attrs={"class": "right"})
    if right:
        print(right.text)
    else:
        wrong = soup.find(attrs={"class": "wrong"}).text
        print(wrong)
        session.post(args.url)
        continue

    congratulations = soup.find(attrs={"class": "congratulations"})
    if congratulations:
        print(congratulations.text)
        break

    i += 1
