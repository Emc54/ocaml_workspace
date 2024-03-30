(*Helper function*)
let rec member e l = 
  match l with
    [] -> false
  | h::t -> h=e || member e t

(*Note that we are using the property that the || operator only evaluates its right hand side if the left
hand side is false to limit the recursion – it really does stop as soon as it finds the element *)

(*Question 1*)

let g = fun a -> fun b -> fun c -> a+b+c

(*Question 2*)

let member_all x ls =
  let bools = map (member x) ls in
    not (member false bools)

(*Question 3*)
let safediv x y =
  if x<=0 then raise (Invalid_argument "Bad dividend")
  else y/x

(*let t = map (safediv 2) [10;20;30] *)
(*Should give [5;10;15]*)

(*Question 4*)

let mapll f = map (map (map f))

