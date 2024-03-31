(*Question 1*)

type rect =
LW of int * int
| SQ of int

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
