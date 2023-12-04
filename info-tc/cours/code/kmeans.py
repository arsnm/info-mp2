# k-means algorithm


def distance(x, y):
    assert len(x) == len(y)
    d = 0
    for i in range(len(x)):
        d += (x[i] - y[i]) ** 2
    return d**0.5


def centre(x: list):
    avg = [0] * len(x[0])
    for j in range(len(x[0])):
        for i in range(len(x)):
            avg[j] += x[i][j] / len(x)
    return avg


# print(centre([[5, 6, 3], [5, 34, 424], [5, 3, 4], [5, 2, 4]]))


def calculer_centres(classes):
    centres = []
    for classe in classes:
        centres.append(centre(classe))
    return centres


def plus_proche(x, centres):
    index_closest = min(range(len(centres)), key=lambda i: distance(x, centres[i]))
    return index_closest


def calculer_classes(x, centres):
    classes = [[] for _ in range(len(centres))]
    for vect in x:
        c = plus_proche(vect, centres)
        classes[c].append(vect)
    return classes


def kmeans(x, centres):
    centres_old = None
    classes = None
    while centres_old != centres:
        classes = calculer_classes(x, centres)
        centres_old = centres
        centres = calculer_centres(classes)
    return classes
