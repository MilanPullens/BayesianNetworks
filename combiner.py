import pandas as pd
import sys
import csv

topMilan = pd.read_csv('top200Milan.csv')
topPip = pd.read_csv('top200Pip.csv')
topManon = pd.read_csv('top200Manon.csv')

topMilan["listener"] = 0
topPip["listener"] = 1
topManon["listener"] = 2

for df in [topMilan, topPip, topManon]:
    df["release_date"] = df["release_date"].astype(str).apply(lambda x: x[:4]).astype(int)
    df["explicit"] = df["explicit"].astype(int)


merged_df = pd.concat([topMilan, topPip, topManon], ignore_index=True)

merged_df.to_csv("topCombi.csv", index=False, quotechar="\"")