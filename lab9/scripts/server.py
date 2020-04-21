#!/usr/bin/env python3.8

from os import urandom
from random import randint, randrange
from time import time

from flask import Flask, render_template, request, session
from sympy import randprime

from utils import choose_mod


def foo():
    P = randprime(2, 10 ** max(session["i"] - 1, 2) + 1)
    N = randrange(0, P)
    K = randint(0, N)
    answer = choose_mod(N, K, P)
    return {"N": N, "K": K, "P": P, "answer": answer}


def init():
    return {
        "i": 1,
        "n_mistakes": 0,
        "right": False,
        "time": time(),
    }


n_iter = 10

app = Flask(__name__)
app.secret_key = urandom(16)


@app.route("/", methods=["GET", "POST"])
def route():
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

    if request.method == "POST":
        if "answer" in request.form:
            answer = False
            session["right"] = session["answer"] == request.form["answer"]
            if session["right"]:
                if session["i"] == n_iter:
                    congratulations = True
                    session["time"] = round(time() - session["time"], 3)
                    if session["n_mistakes"] == 0:
                        session["n_mistakes"] = "no"
            else:
                session["n_mistakes"] += 1

    return render_template(
        "combmod.html", answer=answer, congratulations=congratulations
    )


app.run()
