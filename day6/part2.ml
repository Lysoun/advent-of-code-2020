open Printf
open String
open Set
open Char
  
let input = "input.txt"

module CharSet = Set.Make(Char) ;;  

let init_group_answers () = (CharSet.empty);;

let rec input_lines file currentGroupAnswers initialized sum =
   match try (input_line file) with End_of_file -> " " with
      | " " -> sum
      | "" -> (input_lines file (init_group_answers ()) false (sum + (CharSet.cardinal currentGroupAnswers)))
      | line when initialized -> (input_lines file (CharSet.inter (CharSet.add_seq (to_seq line) (init_group_answers ())) currentGroupAnswers) initialized sum)
      | line -> (input_lines file (CharSet.add_seq (to_seq line) (init_group_answers ())) true sum)
    ;;

print_int (input_lines (open_in input) (init_group_answers ()) false 0);;