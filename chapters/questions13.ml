let channel_statistics in_channel =
  let lines = ref 0 in
    try
      while true do
        let line = input_line in_channel in
          lines := !lines + 1
      done
  with
  End_of_file ->
    print_string "There were ";
    print_int !lines;
    print_string " lines.";
    print_newline ()

let file_statistics name =
  let channel = open_in name in
    try
      channel_statistics channel;
      close_in channel
    with
      _ -> close_in channel

(*Question 5*)

let sum_array arr =
  let sum =  ref 0 in
    for x = 0 to Array.length arr - 1 do 
      sum := !sum + arr.(x)
    done;
    !sum

let ceil_div a b =
  if a mod 2 = 1 then
    (a+1)/b
  else
    a/b
(*Question 6*)
let rev_array arr =
  let temp = ref 0 in
    let len = Array.length arr - 1 in
      for x=0 to (ceil_div (Array.length arr) 2) - 1 do
        temp := arr.(x);
        arr.(x) <- arr.(len-x);
        arr.(len-x) <- !temp
      done