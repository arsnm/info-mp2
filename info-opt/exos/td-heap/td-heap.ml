(* Code du td Heap *)

(* Exercice I *)

(* Exercice II *)

type 'a tree = F of 'a | N of 'a tree * 'a tree
type 'a priority_queue = { a : 'a array; mutable n: int } ;;

let make_pq p e = { a = Array.make p e; n = 0 } ;; 

let pq_create () = make_pq 1000 (0,F("a")) ;; 

let pred i = (i - 1)/2 ;;

let g i = 2*i + 1 ;;

let d i = 2*i + 2 ;;

let swap pq i j =
    let tmp = pq.a.(i) in
        pq.a.(i) <- pq.a.(j);
        pq.a.(j) <- tmp
;;

let rec up pq i =
  let p = pred i in
  if i <> 0 && fst pq.a.(p) > fst pq.a.(i) then (
    swap pq i p;
    up pq p
) ;;

let rec down pq i =
    let get j = (if j < pq.n then fst pq.a.(j) else max_int), j in
    let m, j = min (get (2*i + 1)) (get (2*i + 2)) in
    if fst pq.a.(i) > m then (
    swap pq i j;
    down pq j 
) ;;

let pq_add pq e =
    pq.a.(pq.n) <- e;
    up pq pq.n;
    pq.n <- pq.n + 1
;;

let pq_extract_min pq =
    swap pq 0 (pq.n - 1);
    pq.n <- pq.n - 1;
    down pq 0;
    pq.a.(pq.n)
;;

let pq_is_empty pq =
    pq.n = 0
;;

let len_over_2 pq =
    pq.n >= 2
;;

let rec read tree list =
    match tree with 
    |F(char) -> char, list 
    |N(g, d) -> match list with
                |0::t -> read g t
                |1::t -> read d t
                |_ -> failwith "Given list not in the correct format"
;;

let rec decode tree list =
    if list = [] then [] else 
    let char, remainder = read tree list in
        char::decode tree remainder 
;;

let to_huffman freq =
    let fp = pq_create () in
    let rec fill freq=
      match freq with
      |[] -> ()
      |(f, chr)::t -> pq_add fp (f, F(chr)) ; fill t in
    fill freq;
    while len_over_2 fp do
      let (f1, a1), (f2, a2) = pq_extract_min fp, pq_extract_min fp in
        pq_add fp (f1 + f2, N(a1, a2))
    done;
    snd (pq_extract_min fp)
;;

let to_dict arb = 
  let dict = Hashtbl.create 100 in
  let rec aux arb dict code =
    match arb with
    |F(chr) -> Hashtbl.add dict chr (List.rev code)
    |N(g, d) -> aux g dict (0::code) ; aux d dict (1::code) 
  in
  aux arb dict [] ; dict
;;

let rec code lst dict =
    match lst with
    |[] -> []
    |h::t -> (Hashtbl.find dict h) @ code t dict
;;

(* Tests *)
let huffman_tree = to_huffman [(20, "a"); (15, "b"); (7, "c"); (14, "d"); (44, "e")]
let dict = to_dict huffman_tree
let message = ["b"; "a"; "c"; "a"; "d"; "e"; "b"; "a"; "c"; "a"; "d"; "e"] ;;
let encoded = code message dict ;;
let encoded_err = encoded @ [0] ;;
let decoded = decode huffman_tree encoded ;;

(* Exercice III *)
type 'a arb = V | N of 'a * 'a arb * 'a arb ;;

let rotd treap = match treap with
    |N(r, N(gr, gg, gd), d) -> N(gr, gg, N(r, gd, d))
    |_ -> treap
;;

let rotg treap = match treap with
  |N(gr, gg, N(r, gd, d)) -> N(r, N(gr, gg, gd), d)
  |_ -> treap
;;

let prio tree = match tree with
|V -> max_int
|N((_, p), _, _) -> p
;;

let rec add treap e = 
      let elem, _ = e in
      match treap with
      |V -> N(e, V, V)
      |N((x, p), g, d) -> if elem >= x then 
                              let d_upt = add d e in
                                  if (prio d_upt) < p then
                                    rotg (N((x,p), g, d_upt))
                                  else N((x,p), g, d_upt)
                          else 
                              let g_upt = add g e in
                                if (prio g_upt) < p then
                                  rotd (N((x,p), g_upt, d))
                                else N((x,p), g_upt, d)
                        ;;

let treap1 = N((4,1), 
          N((2,4), 
            N((1,6), V, V), V),
          N((7,6),
            N((5,8), V, V),
            N((9,9), V, V)))
          ;;        

let treap2 = add treap1 (6, 0) ;;


let rec del treap e = match treap with
    |V -> V
    |N((x,p), g, d) -> if e > x then
                        N((x,p), g, (del d e))
                      else if e < x then
                        N((x,p), (del g e), d)
                      else match g,d with
                        |V, V -> V
                        |V, f |f, V -> f
                        |_ -> if prio g > prio d then
                              let treap_rot = rotg(treap) in
                                del treap_rot e
                              else let treap_rot = rotd(treap) in
                                del treap_rot e
                            ;;

let treap3 = del treap2 6
