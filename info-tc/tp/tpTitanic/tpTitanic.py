import pandas as pd

df = pd.read_csv('titanic.csv')

def moyenne(df, c):
    sum = 0
    for i in df.index:
        sum += df[i, c]
    return sum / len(df)

def ecart_type(df, c):
    avg = moyenne(df, c) 
    sum = 0
    for i in df.index:
        sum += (df[i, c] - avg) ** 2
    return ( 1 / len(df) * sum) ** (1 / 2)

def survivants(col, val):
    n_survivants = 0
    n = 0
    for i in df.index:
        if df.loc[i, col] == val:
            n_survivants += df.loc[i, "Survived"]
            n += 1
    return n_survivants/n

for c, v in [("Sex", "male"), ("Sex", "female"), ("Pclass", 1), ("Pclass", 3)]:
    print("Taux de survie pour", c, "=", v, ":", survivants(c, v))

def standardiser(df, c):
    avg = moyenne(df, c) 
    std = ecart_type(df, c) 
    for i in df.index:
        df.loc[i, c] = (df.loc[i, c] - avg) / std
    return None

def distance(p1, p2):
    sum = 0
    for attribute in p1:
        if attribute != "Survived":
            sum += (p1[attribute] - p2[attribute]) ** 2
    return sum ** (1 / 2)


