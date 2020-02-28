#!/usr/bin/env python3

from flask import Flask, render_template, request, session
from gmpy2 import is_prime
from os import urandom
from random import randint
from subprocess import check_output
from time import time


def choose_mod(N, K, P):
    return check_output(
        "../../lab6/parhaskell", input=f"1\n {N} {K} {P}\n", universal_newlines=True
    ).strip()


def foo():
    while True:
        P = randint(1, int(base ** session["i"]))
        if is_prime(P):
            break
    N = randint(0, P - 1)
    K = randint(0, N)
    answer = choose_mod(N, K, P)
    print(answer)
    return {"N": N, "K": K, "P": P, "answer": answer}


def init():
    return {
        "i": 1,
        "mistakes": 0,
        "right": False,
        "time": time(),
    }


base = 10 ** (9 / 10)
n_iter = 10

app = Flask(__name__)
app.secret_key = urandom(16)


@app.route("/", methods=["GET", "POST"])
def _():
    answer = True
    congratulations = False

    if not session:
        session.update(init())
        session.update(foo())

    if session["right"]:
        session["right"] = False
        session["i"] += 1
        if session["i"] == n_iter + 1:
            session.update(init())
        session.update(foo())

    if "answer" in request.form:
        answer = False
        session["right"] = str(session["answer"]) == request.form["answer"]
        if session["right"]:
            if session["i"] == n_iter:
                congratulations = True
                session["time"] = round(time() - session["time"], 3)
        else:
            session["mistakes"] += 1

    return render_template(
        "combmod.html", answer=answer, congratulations=congratulations
    )


app.run(debug=True)
