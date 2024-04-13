(*Question 1 - ceil*)
(*Question 2*)

let midpoint (x0,y0) (x1,y1) = 
  let x = (x0 +. x1) /. 2. in
    let y = (y0 +. y1) /. 2. in
      (x,y)

    
(*Question 3*)

let split x = 
  let a = floor x in
  let rest = x -. a in
    (a,rest)

(*Question 3 Accounting for negative numbers*)

let rec split2 x = 
  if x < 0. then
    let a,b = split2 (-. x) in
      (-. a, b)
  else
    (floor x, x -. floor x)


(*Question 4*)

let star x =
  let i = int_of_float (floor (x *. 50.)) in
    let j = 
      if i=50 then 49 
      else i 
    in
    for x = 1 to j-1 do
      print_char ' ' done;
      print_char '*';
      print_newline ()


(*Question 5*)

let plot f a b dy =
  let pos = ref a in
  while !pos <= b do
    star(f !pos);
    pos := !pos +. dy
  done