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
 