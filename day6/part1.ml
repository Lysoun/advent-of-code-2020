open Printf
open String
open Set
open Char
  
let input = "input.txt"

module CharSet = Set.Make(Char) ;;  

let init_group_answers () = (CharSet.empty);;

let rec input_lines file currentGroupAnswers sum =
   match try (input_line file) with End_of_file -> " " with
      | " " -> sum
      | "" -> (input_lines file (init_group_answers ()) (sum + (CharSet.cardinal currentGroupAnswers)))
      | line -> (input_lines file (CharSet.add_seq (to_seq line) currentGroupAnswers) sum)
    ;;

print_int (input_lines (open_in input) (init_group_answers ()) 0);;