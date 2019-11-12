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
        d["Package"], d["Parallelism"] = basename(dirpath).split(sep="_")
        tokens = splitext(filename)[0].split(sep="_")
        for token in tokens:
            k, v = token.split(sep="-")
            d[k] = int(v)
        with open(join(dirpath, filename)) as f:
            for line in f:
                if line.startswith("  Total   time"):
                    d["Time"] = float(line.split(sep="(")[1].split(sep="s")[0])
        data.append(d)
data = pd.DataFrame(data=data).sort_values(by=["T", "Package", "Parallelism", "N"])

data["Speedup"] = (
    data.groupby(by=["T", "Package", "Parallelism"], as_index=False)
    .apply(lambda df: df[df["N"] == 1]["Time"].values / df["Time"])
    .values
)

data.to_csv(join(dirname(__file__), f"../tables/table.csv"), index=False)

for y in ["Time", "Speedup"]:
    fg = sns.relplot(
        x="N",
        y=y,
        hue="T",
        data=data,
        row="Package",
        col="Parallelism",
        legend="full",
        kind="line",
        marker="o",
    )
    for ax1 in fg.axes:
        for ax2 in ax1:
            ax2.set_xscale("log", basex=2)
            ax2.set_yscale("log", basey=2)
    fg.savefig(
        join(dirname(__file__), f"../plots/{y.lower()}.pdf"), bbox_inches="tight"
    )
