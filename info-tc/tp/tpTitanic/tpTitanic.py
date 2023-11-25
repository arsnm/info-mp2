import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv("titanic.csv")


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
    return (1 / len(df) * sum) ** (1 / 2)


def survivants(col, val):
    n_survivants = 0
    n = 0
    for i in df.index:
        if df.loc[i, col] == val:
            n_survivants += df.loc[i, "Survived"]
            n += 1
    return n_survivants / n


# for c, v in [("Sex", "male"), ("Sex", "female"), ("Pclass", 1), ("Pclass", 3)]:
#      print("Taux de survie pour", c, "=", v, ":", survivants(c, v))

# remplace male par 0 et female par 1
df["Sex"] = df["Sex"].map({"male": 0, "female": 1})


def standardiser(df, c):
    avg = moyenne(df, c)
    std = ecart_type(df, c)
    for i in df.index:
        df.loc[i, c] = (df.loc[i, c] - avg) / std
    return None


def distance(p1, p2):
    sum = 0
    for attribute in p1.index:
        if attribute != "Survived":
            sum += (p1[attribute] - p2[attribute]) ** 2
    return sum ** (1 / 2)


train = df.sample(frac=0.9, random_state=0)
test = df.drop(train.index)
# print("nombre de données dans train :", len(train))
# print("nombre de données dans test :", len(test))


def voisins(x, k):
    indices = sorted(train.index, key=lambda i: distance(x, train.loc[i]))
    return indices[k:]


def plus_frequent(list_elements):
    count = {}
    for element in list_elements:
        if element in count:
            count[element] += 1
        else:
            count[element] = 1
    return max(count, key=lambda element: count[element])


def knn(x, k):
    return plus_frequent(train.loc[i, "Survived"] for i in voisins(x, k))


def precision(k):
    success = 0
    for i in test.index:
        knn_result = knn(test.loc[i], k)
        print(knn_result)
        if knn_result == test.loc[i, "Survived"]:
            success += 1
    return success / len(test)


def plot_precision(kmax):
    x = range(1, kmax + 1)
    y = []
    for k in x:
        p = precision(k)
        y.append(p)
    plt.plot(x, y)
    plt.show()


print(precision(46))
plot_precision(11)
