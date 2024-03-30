(* Question 1 *)
let rec calm l = 
  match l with
    [] -> []
| h::t ->
  if h != '!' 
    then h :: calm t
else '.' :: calm t

let calm_char cin =
  match cin with
  '!' -> '.'
  |_ -> cin

let calm2 l =
  map calm_char l

(* Question 2 *)
let clip x =
  if (x < 1) then 1
  else if (x > 10) then 10
  else x

let cliplist l =
  map clip l
  
(* Question 3 *)
let cliplist2 l =
  map (fun x ->
    if (x<1) then 1
    else if (x>10) then 10
    else x) l

(* Question 4 *)
let rec apply f num x =
 if num = 0 then x
 else f (apply f (num -1) x)

 (* Question 6 *)
let rec filter f l =
  match l with
  [] -> []
  | h::t -> 
  if f h then 
      h :: filter f t
  else filter f t 