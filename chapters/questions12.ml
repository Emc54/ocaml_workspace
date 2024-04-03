(*Useful functions*)
(***Printing***)

let rec print_dict_entry (k,v) =
  print_int k;
  print_newline ();
  print_string v;
  print_newline ()

(*Equivalent to:*)
(* print_int k; print_newline (); print_string v; print_newline ()*)

let rec iter f l =
  match l with
  [] -> ()
  | h::t -> f h; iter f t

(*Shorthand ommitting dictionary argument*)
let print_dict = iter print_dict_entry

(***Reading from Keyboard***)
let rec read_dict () =
  try
    print_string "Give me an Int"; print_newline ();
    let i = read_int () in
    if i = 0 then [] 
    else
      let name = read_line () in
      (i,name) :: read_dict ()
    with
    Failure _ ->
      print_string "This is not a valid integer. Try again.";
      print_newline ();
      read_dict ()


(***File Manipulation***)
let entry_to_channel ch (k, v) =
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n'

let dictionary_to_channel ch d =
  iter (entry_to_channel ch) d

let dictionary_to_file filename dict =
  let ch = open_out filename in
    dictionary_to_channel ch dict;
    close_out ch  

let entry_of_channel ch =
  let number = input_line ch in
    let name = input_line ch in
      (int_of_string number, name)

let rec dictionary_of_channel ch =
  try
    let e = entry_of_channel ch in
      e :: dictionary_of_channel ch
  with
    End_of_file -> []

let dictionary_of_file filename = 
  let ch = open_in filename in
    let dict = dictionary_of_channel ch in
      close_in ch;
      dict

let test_dict = [(1,"4");(2,"5");(4,"2");(6,"3");(7,"7")]

(*Try question 6*)
let rec copy_channel ch_in ch_out =
  try
    output_string ch_out (input_line ch_in);
    output_char ch_out '\n';
    copy_channel ch_in ch_out
  with
    End_of_file -> ()

exception CopyFailed
let copy_file file_in file_out =
  try
  let ch_in = open_in file_in in
    let ch_out = open_out file_out in
        copy_channel ch_in ch_out;
        close_in ch_in;
        close_out ch_out
  with
  _ -> raise CopyFailed
  