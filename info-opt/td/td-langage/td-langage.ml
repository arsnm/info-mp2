type 'a regexp =
    |Vide |Epsilon | L of 'a
    |Union of 'a regexp * 'a regexp
    |Concat of 'a regexp * 'a regexp
    |Etoile of 'a regexp


let rec voisinnage = function 
    |Vide -> Vide
    |Epsilon -> Epsilon
    |L(a) -> L(1 - a)
    |Union(e1, e2) -> Union(voisinnage e1, voisinnage e2)
    |Concat(e1, e2) -> Union(Concat(voisinnage e1, e2), Concat(e1, voisinnage e2))
    |Etoile(e) -> Concat(Etoile(e), Concat(voisinnage e, Etoile(e)))

let rec hauteur = function
    |Vide |Epsilon |L(_) -> 0
    |Union(e1, e2) |Concat(e1, e2) -> max (hauteur e1) (hauteur e2)
    |Etoile(e) -> 1 + hauteur e
