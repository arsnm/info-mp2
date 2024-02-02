import numpy as np
from matplotlib import pyplot as plt
from sklearn.datasets import load_digits

digits = load_digits()
digits_matrix = digits.images[
    1000:
]  # utilisation d'une partie seulement pour accélérer les calculs
digits_matrix.shape

plt.imshow(digits_matrix[0], cmap=plt.cm.gray_r)
plt.show()  # ça ressemble à un 1


def to_vector(m):
    vector = []
    for l in m:
        vector += list(l)
    return vector


assert (
    len(to_vector(digits_matrix[0])) == 64
)  # les images 8*8 sont transformées en vecteur 64


def to_matrix(v):
    matrix = []
    n = int(len(v) ** 0.5)
    for i in range(n):
        matrix.append(v[n * i : n * i + n])
    return matrix


assert (to_matrix(to_vector(digits_matrix[0])) == digits_matrix[0]).all()

X = [to_vector(x) for x in digits_matrix]


def d(x, y):
    # assert x == y, "vector must be of the same dimension"
    dist = 0
    for i in range(len(x)):
        dist += (x[i] - y[i]) ** 2
    return dist**0.5


import random as rd

rd.seed(0)


def centres_aleatoires(set, number: int):
    return [X[i] for i in rd.sample(range(len(X)), number)]


centres = centres_aleatoires(X, 10)
assert len(centres) == 10 and len(centres[0]) == 64
