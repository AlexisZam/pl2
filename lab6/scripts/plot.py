#!/usr/bin/env python3.8

from os import walk
from os.path import basename, dirname, join, splitext
import pandas as pd
import seaborn as sns

sns.set()


data = []
for dirpath, dirnames, filenames in walk(join(dirname(__file__), "../outputs")):
    for filename in filenames:
        d = {}
        tokens = splitext(filename)[0].split(sep="_")
        for token in tokens:
            k, v = token.split(sep="-")
            d[k] = int(v)
            d["Package"] = basename(dirpath)
        with open(join(dirpath, filename)) as f:
            for line in f:
                if line.startswith("  Total   time"):
                    d["Time"] = float(line.split("(")[1].split("s")[0])
        data.append(d)
data = pd.DataFrame(data=data).sort_values(by=["T", "Package", "N"])

data["Speedup"] = (
    data.groupby(["T", "Package"], as_index=False)
    .apply(lambda df: df[df["N"] == 1]["Time"].values / df["Time"])
    .values
)

for y in ["Time", "Speedup"]:
    fg = sns.relplot(
        x="N",
        y=y,
        hue="T",
        data=data,
        col="Package",
        legend="full",
        kind="line",
        marker="o",
    )
    # for ax in fg.axes:
    #     ax.set_xscale("log", basex=2)
    #     ax.set_yscale("log", basey=2)
    fg.savefig(
        join(dirname(__file__), f"../plots/{y.lower()}.pdf"), bbox_inches="tight"
    )
