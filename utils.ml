let rec length_inner l n =
  match l with
  [] -> n
  | h::t -> length_inner t (n + 1)

let length l = length_inner l 0

let rec append a b =
  match a with
  [] -> b
  | h::t -> h :: append t b

let rec take n l =
  match l with
  [] ->
  if n = 0
    then []
    else raise (Invalid_argument "take")
  | h::t ->
    if n < 0 then raise (Invalid_argument "take") else
    if n = 0 then [] else h :: take (n - 1) t

let rec drop n l =
  match l with
  [] ->
  if n = 0
    then []
    else raise (Invalid_argument "drop")
  | h::t ->
    if n < 0 then raise (Invalid_argument "drop") else
    if n = 0 then l else drop (n - 1) t

let rec insert x l =
  match l with
   [] -> [x]
  | h::t ->
    if x <= h
      then x :: h :: t
      else h :: insert x t

let rec sort l = 
 match l with
  [] -> []
  | h::t -> insert h (sort t)

let rec merge x y =
  match x, y with
  [], l -> l
  | l, [] -> l
  | hx::tx, hy::ty ->
  if hx < hy
  then hx :: merge tx (hy :: ty)
  else hy :: merge (hx :: tx) ty

let rec msort l =
  match l with
  [] -> []
  | [x] -> [x]
  | _ ->
  let half_len = (length l / 2) in
  let left = take half_len l in
  let right = drop half_len l in
  merge (msort left) (msort right)

  let rec map f l =
    match l with
     [] -> []
  | h::t -> f h:: map f t 

let first (x,_) = x
let second (_,y) = y

(*Dictionary lookup*)
let rec lookup key l = 
  match l with
  [] -> raise Not_found
  | (k,v)::t ->
    if k = key then v 
    else lookup key t

(*Custom insert and sort*)

let rec cinsert f x l =
  match l with
  [] -> [x]
  | h::t ->
    if f x h
      then x::h::t
  else h:: cinsert f x t

let rec csort f l =
  match l with
  [] -> []
  | h::t -> cinsert f h (csort f t)