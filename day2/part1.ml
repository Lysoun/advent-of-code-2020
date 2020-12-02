open Printf
open String
open List

let inputFilename = "input.txt"

let rec password_is_valid min max counter character password = 
    if (String.length password) == 0 then counter >= min else 
    let h = (get password 0) in
    let t = (sub password 1 ((String.length password) - 1)) in
    if h == character && counter == max then false else 
    if h == character then (password_is_valid min max (counter + 1) character t) else
    (password_is_valid min max counter character t)
 ;;

let rec input_lines file valid =
   match try (input_line file) with End_of_file -> "" with
      | "" -> valid
      | line -> 
      let splitOnHyphen = (split_on_char '-' line) in
      let splitOnSpaces = (split_on_char ' ' (nth splitOnHyphen 1)) in
      if (password_is_valid (int_of_string (nth splitOnHyphen 0)) (int_of_string (nth splitOnSpaces 0)) 0 (get (nth splitOnSpaces 1) 0) (nth splitOnSpaces 2)) 
      then (input_lines file (valid + 1)) else (input_lines file valid)
    ;;

print_int (input_lines (open_in inputFilename) 0);;

