open Printf
open String
open List

let inputFilename = "input.txt"

let xor bool1 bool2 =
    (bool1 &&  (not bool2)) || ((not bool1) && bool2);;

let password_is_valid pos1 pos2 character password = 
    (xor ((get password (pos1 - 1)) == character) ((get password (pos2 - 1)) == character) )
 ;;

let rec input_lines file valid =
   match try (input_line file) with End_of_file -> "" with
      | "" -> valid
      | line -> 
      let splitOnHyphen = (split_on_char '-' line) in
      let splitOnSpaces = (split_on_char ' ' (nth splitOnHyphen 1)) in
      if (password_is_valid (int_of_string (nth splitOnHyphen 0)) (int_of_string (nth splitOnSpaces 0)) (get (nth splitOnSpaces 1) 0) (nth splitOnSpaces 2)) 
      then (input_lines file (valid + 1)) else (input_lines file valid)
    ;;

print_int (input_lines (open_in inputFilename) 0);;

