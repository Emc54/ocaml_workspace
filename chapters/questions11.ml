(*Trees*)

type 'a tree =
Br of 'a * 'a tree * 'a tree
| Lf

(*
     2   
   /   \ 
   1   4
  / \ / \
*)

let tree214 = Br (2, Br (1, Lf, Lf), Br (4, Lf, Lf)) 
let empty_tree = Lf

(*Number of tree elements*)
let rec size tr =
  match tr with
  Br(_,l,r) -> 1 + size l + size r
  |Lf -> 0

(*Max depth*)
let max x y = 
  if x > y then x else y

  let rec maxdepth tr =
    match tr with
    Br(_,l,r) -> 1 + max (maxdepth l) (maxdepth r)
    |Lf -> 0

(*Flatten a tree*)

let rec list_of_tree tr =
  match tr with
  Br(x,l,r) -> list_of_tree l @ [x] @ list_of_tree r
  |Lf -> []

(*Map over trees*)
let rec tree_map f tr =
  match tr with
  Br(x,l,r) -> Br (f x, tree_map f l, tree_map f r)
  |Lf -> Lf

(*If we arrange the tree such that, at each branch, everything to the left has a key less than the key at the
branch, and everything to the right has a key greater than that at the branch, we have a binary search tree.*)

let bin_tree = Br ((3, "three"), Br ((1, "one"), Lf, Br ((2, "two"), Lf, Lf)), Br ((4, "four"), Lf, Lf))

let rec lookup tr k =
  match tr with
  Lf -> raise Not_found
  | Br((key, value), l, r) ->
    if k = key then value
    else if k < key then lookup l k
    else lookup r k

let rec insert tr k v =
  match tr with
  Lf -> Br((k,v),Lf,Lf)
  |Br((key, value),l,r) ->
    if k = key then Br((k,v),l,r)
    else if k < key then Br((key,value), insert l k v, r)
    else Br((key,value),l,insert r k v)

(*Question 1*)
let rec intree x tr =
  match tr with
  Lf -> false
  |Br(y,l,r) ->
    x=y || intree x l || intree x r

(*Question 2*)
let rec mirror tr = 
  match tr with
  Lf -> Lf
  |Br(x,l,r) -> Br(x, mirror r, mirror l)

(*Question 3 book solution*)
let rec equal_shape tr1 tr2 =
  match tr1, tr2 with
  Lf, Lf -> true
  | Br(_,l1,r1), Br(_,l2,r2) ->
    equal_shape l1 l2 && equal_shape r1 r2
  | _, _ -> false
  