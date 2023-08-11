import numpy as np
import pandas as pd
import duckdb

region = 'eaeu'
input = 'adb-mrio.parquet'
year = 2022

outputpath = 'data/final/5.2_skyline.csv'

# CONSTRUCT REGIONAL INPUT-OUTPUT TABLE

G, N, f = 73, 35, 5

regions = pd.read_excel('../../MRIO Processing/dicts/countries.xlsx')
regions.dropna(subset=['mrio'], inplace=True)
regions.sort_values(by='mrio', inplace=True)
regions = regions[['mrio', f'rta_{region}']]
regions.fillna(0, inplace=True)
regions[f'rta_{region}'] = regions[f'rta_{region}'].astype(int)

mrio = duckdb.sql(f"SELECT * EXCLUDE(t, si) FROM '../../MRIO Processing/data/mrio/{input}' WHERE t={year}").df()
mrio = mrio.values

x = mrio[-1][:(G*N)]
Z = pd.DataFrame(mrio[:(G*N)][:, :(G*N)])
Y_big = mrio[:(G*N)][:, (G*N):-1]
Y = Y_big @ np.kron(np.eye(G), np.ones((f, 1)))
Y = pd.DataFrame(Y)

# CALCULATE REGIONAL IOT VALUES

# Output

df_x = pd.DataFrame({
    'region': regions[f'rta_{region}'].repeat(N),
    'i': np.tile(np.arange(1, N+1), G),
    'x': x
})
df_x = df_x.groupby(['region', 'i']).sum().reset_index()
x = df_x['x']

# Intermediates

df_Z = pd.DataFrame({
    'region': regions[f'rta_{region}'].repeat(N),
    'i': np.tile(np.arange(1, N+1), G)
}).reset_index(drop=True)
df_Z = pd.concat([df_Z, Z], axis=1)
df_Z = df_Z.groupby(['region', 'i']).sum().reset_index()

tZ = np.transpose(df_Z.iloc[:, 2:])
df_tZ = pd.DataFrame({
    'region': regions[f'rta_{region}'].repeat(N),
    'i': np.tile(np.arange(1, N+1), G)
}).reset_index(drop=True)
df_tZ = pd.concat([df_tZ, tZ], axis=1)
df_tZ = df_tZ.groupby(['region', 'i']).sum().reset_index()

Z = np.transpose(df_tZ.iloc[:, 2:])

# Final sales

df_Y = pd.DataFrame({
    'region': regions[f'rta_{region}'].repeat(N),
    'i': np.tile(np.arange(1, N+1), G)
}).reset_index(drop=True)
df_Y = pd.concat([df_Y, Y], axis=1)
df_Y = df_Y.groupby(['region', 'i']).sum().reset_index()

tY = np.transpose(df_Y.iloc[:, 2:])
df_tY = pd.DataFrame({'region': regions[f'rta_{region}']}).reset_index(drop=True)
df_tY = pd.concat([df_tY, tY], axis=1)
df_tY = df_tY.groupby(['region']).sum().reset_index()

Y = np.transpose(df_tY.iloc[:, 1:])

# Consolidate into table

Z_d = Z.iloc[:, N:N+N+1]
Y_d = Y[1]

riot = pd.DataFrame({'i': np.tile(np.arange(1, N+1), 2)})
riot = pd.concat([riot, pd.DataFrame(Z_d)], axis=1)
riot = pd.concat([riot, pd.DataFrame(Y_d)], axis=1)
riot = riot.groupby(['i']).sum().reset_index()

colnames = ['i'] + [f'Z_d_{i}' for i in range(1, 36)] + ['Y_d']
riot.columns = colnames

riot['Z_exports'] = np.sum(Z.iloc[N:N+N+1, 0:N], axis=1).reset_index(drop=True)
riot['Y_exports'] = Y.iloc[N:N+N+1, 0].reset_index(drop=True)
riot['imports'] = np.sum(riot.iloc[:, 1:], axis=1).reset_index(drop=True) - x[N:].reset_index(drop=True)
riot['x'] = x[N:].reset_index(drop=True)

# SKYLINE CHART

Z = riot.iloc[:, 1:N+1]
x = riot['x']
A = Z @ np.diag(np.where(x != 0, 1/x, 0))
B = np.linalg.inv(np.eye(N) - A)

sf = B @ riot['Y_d']
se = B @ (riot['Z_exports'] + riot['Y_exports'])
sm = B @ riot['imports']

df = pd.DataFrame({
    't': year,
    'i': np.arange(1, N+1),
    'x': x,
    'sf': sf, 
    'se': se, 
    'sm': sm, 
    'self_sufficiency': x/sf,
    'total': (sf+se)/sf,
    'imports': sm/sf
})

sectors = pd.read_excel('../../MRIO Processing/dicts/sectors.xlsx')
sectors = sectors.drop_duplicates(subset='ind', ignore_index=True)
sectors = sectors[['ind', 'abv', 'name']]
sectors.rename(columns={'ind':'i'}, inplace=True)

df = pd.merge(df, sectors)
df.insert(2, 'abv', df.pop('abv'))
df.insert(3, 'name', df.pop('name'))

df.to_csv(outputpath, index=False)