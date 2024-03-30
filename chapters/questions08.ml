let test_dict = [(1,4);(2,5);(4,2);(6,3);(7,7)]

(*Question 1*)
let dict_len dict = 
  length dict (*Length function in utils*)

(*Question 2*)
let rec replace k v d =
  match d with
    [] -> raise Not_found
  | (key, value)::t ->
    if k = key 
      then (k,v)::t
      else (key, value)::replace k v t

let rec add k v d =
  match d with
    [] -> [(k,v)]
    | (key, value)::t ->
      if k=key
        then (k,v)::t
      else (key, value) :: add k v t

(*Question 3*)
let rec dict_zip_inner keys values dict =
  match keys, values with
    [],[] -> dict
    | k::t1,v::t2 -> dict_zip_inner t1 t2 (add k v dict)
    |[], _ -> raise (Invalid_argument "different length keys and values")
    | _ ,[] -> raise (Invalid_argument "different length keys and values")
  

let dict_zip keys values = 
  dict_zip_inner keys values []

(*Question 4*)
let rec dict_unzip_inner keys values dict = 
  match dict with
  [] -> keys, values
  | (k,v)::t -> dict_unzip_inner (keys@[k]) (values@[v]) t

let dict_unzip dict =
  dict_unzip_inner [] [] dict
  

(*Q4 Expected Answer*)
let rec mklists l = 
  match l with
    [] -> ([],[])
  | (k,v)::more ->
    match mklists more with
    (ks,vs) -> (k::ks, v::vs)

(*Question 6*)

let rec union a b = 
  match a with
    [] -> b
  | (k,v)::t -> add k v (union t b) 