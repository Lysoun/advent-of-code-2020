open Printf
open String
  
let input = "input.txt"

let rec input_lines file treeNumber column =
   match try (input_line file) with End_of_file -> "" with
      | "" -> treeNumber
      | line when ((get line (column mod (length line))) == '#') -> (input_lines file (treeNumber + 1) (column + 3))
      | _ -> (input_lines file treeNumber (column + 3))
    ;;

print_int (input_lines (open_in input) 0 0);;