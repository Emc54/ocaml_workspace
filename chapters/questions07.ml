(*Question 1*)


(* let rec smallest l =
  match l with
  [] -> []
  | [x] -> 
    if (x > 0) then [x]
    else raise Not_found
  | h::j::t ->
    if (h < j) then smallest h::t
    else smallest j::t *)

(*Wrong because it doesn't handle negative integers inside the list*)
(*Question 1 Method 2*)

let rec smallest_inner current found l =
  match l with
    [] ->
      if found then current else raise Not_found
    | h::t ->
      if h > 0 && h < current
        then smallest_inner h true t
      else smallest_inner current found t

let smallest l =
smallest_inner max_int false l



(*Question 2*)
let smallest_or_zero l =
  try smallest l with Not_found -> 0 