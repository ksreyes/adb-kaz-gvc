import numpy as np
import pandas as pd
import duckdb
import sys

sys.path.append('../../../MRIO Processing/codes')
from functions import subset, zeroout, diagvec

select, years = 'Kazakhstan', [2021, 2022]

input, output = 'adb-mrio.parquet', 'fva-origins.parquet'
countries = pd.read_excel('../../../MRIO Processing/dicts/countries.xlsx')
s = countries.loc[countries['name'] == select, 'mrio'].iloc[0].astype(int)
G, N, f = 73, 35, 5

fva_origins = pd.DataFrame()

for year in years:
    
    mrio = duckdb.sql(f"SELECT * EXCLUDE(t, si) FROM '../../../MRIO Processing/data/mrio/{input}' WHERE t={year}").df()
    mrio = mrio.values

    x = mrio[-1][:(G*N)]
    Z = mrio[:(G*N)][:, :(G*N)]
    va = np.sum(mrio[-7:-1][:, :(G*N)], axis=0)
    Y_big = mrio[:(G*N)][:, (G*N):-1]
    Y = Y_big @ np.kron(np.eye(G), np.ones((f, 1)))
    v = np.where(x != 0, va/x, 0)
    A = Z @ np.diag(np.where(x != 0, 1/x, 0))
    E = zeroout(Z @ np.kron(np.eye(G), np.ones((N, 1))) + Y)
    Bnots = np.linalg.inv(np.eye(G*N) - zeroout(A, s, -s))
    fva = np.transpose(diagvec(subset(v, -s))) @ subset(Bnots, -s, s) @ np.sum(subset(E, s, -s), axis=1)

    fva_origins = pd.concat([
        fva_origins,
        pd.DataFrame({
            't': int(year), 
            'u': np.setdiff1d(np.arange(1, G+1), s),
            's': s, 
            'fva': fva
    })])

fva_origins.to_parquet(f'../data/interim/{output}', index=False)