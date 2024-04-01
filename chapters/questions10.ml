#use "..\\utils.ml";;

(*Question 1*)

type rect =
LW of int * int
| SQ of int

let testlist = [SQ 6; LW (4, 3); LW (5, 6); SQ 2];;
(*Question 2*)

let rectArea rect = 
  match rect with
  LW (x,y) -> x*y
  | SQ x -> x*x

let rectCirc rect =
  match rect with
  LW (x,y) -> 2*(x+y)
  | SQ x -> 4*x

(*Question 3*)
(*Make rectangles stand tall*)

let stand rect =
  match rect with
  LW(x,y) -> 
    if x>y then LW(y,x)
    else rect
  |SQ _ -> rect

(*Question 4*)
let rect_sort reclist = sort (map stand reclist)
 (*This solution doesn't check the squares*)  

(*Q4 Book Solution*)

let width_of_rect r =
  match r with
  SQ s -> s
  | LW (w,_) -> w

let rect_comp a b =
  width_of_rect a < width_of_rect b
let pack rects =
 csort rect_comp (map stand rects)
 

 (*Question 5*)

 type 'a sequence = Nil | Cons of 'a * 'a sequence

 let rec seq_length s = 
  match s with
    Nil -> 0
  | Cons (_, t) -> 1 + seq_length t

let rec seq_append a b =
  match a with
  Nil -> b
  | Cons (h, t) -> Cons(h, seq_append t b)

let rec seq_take n seq =
  match seq with
  Nil ->
    if n = 0 then Nil
    else raise (Invalid_argument "seq_take")
  | Cons (h,t) ->
    if n < 0 then raise (Invalid_argument "seq_take") else
    if n = 0 then Nil else Cons(h, (seq_take (n-1) t)) 


let test_seq = Cons(4, Cons(34, Cons(2343,Nil)))


let req seq_drop n seq =
  if n = 0 then seq else
    match seq with
    Nil -> raise(Invalid_argument "seq_drop")
    | Cons(_, seq) -> seq_drop (n-1) seq

let rec seq_map f seq =
  match seq with
  Nil -> Nil
  | Cons (h,t) -> Cons(f h, seq_map f t)