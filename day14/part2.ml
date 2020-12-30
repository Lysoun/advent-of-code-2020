open Printf
open String
open Str
open Array
open Hashtbl
open Seq

let input = "input.txt";;

let maskExtractionRegexp = Str.regexp "^mask = \\(.+\\)";;
let writingMemoryExtractionRegexp = Str.regexp "^mem\\[\\([0-9]+\\)\\] = \\([0-9]+\\)";;

let rec int_to_binary_aux binary index number = match number with 
    | 0 -> binary
    | _ -> 
    (Array.set binary index (number mod 2));
    (int_to_binary_aux binary (index - 1) (number / 2))
;;

let int_to_binary number = (int_to_binary_aux (Array.make 36 0) 35 number);;

let rec binary_to_int_aux binary index currentTwoPower currentResult = match index with
        | -1 -> currentResult
        | _ -> (binary_to_int_aux binary (index - 1) (currentTwoPower * 2) (currentResult + currentTwoPower * (Array.get binary index)))
    ;;

let binary_to_int binary = 
    (binary_to_int_aux binary 35 1 0)
;; 

let apply_mask mask str = 
    let binary = (int_to_binary (int_of_string str)) in
    let rec aux mask binaries index = match mask with
    | [] -> binaries
    | h::t when h = 'X' -> (aux t (List.concat (List.map (function bit -> (List.map (function b -> bit::b) binaries)) [0; 1])) (index + 1))
    | h::t when h = '0' -> (aux t (List.map (function b -> (Array.get binary index)::b) binaries) (index + 1))
    | h::t -> (aux t (List.map (function b -> 1::b) binaries) (index + 1))
    in 
    (List.map (binary_to_int) (List.map (Array.of_list) (List.map (List.rev) ((aux mask [[]] 0)))))
;;

let rec process_input file currentMask memory =
   match try (input_line file) with End_of_file -> "" with
      | "" -> memory
      | line when (Str.string_match maskExtractionRegexp line 0) -> (process_input file (List.of_seq (String.to_seq (Str.matched_group 1 line))) memory)
      | line ->  
        (Str.string_match writingMemoryExtractionRegexp line 0); 
        let keys = (apply_mask currentMask (Str.matched_group 1 line)) in
        let value = (int_of_string (Str.matched_group 2 line)) in
        (List.iter (function key -> (Hashtbl.replace memory key value)) keys);
        (process_input file currentMask memory)
    ;;

print_int (Seq.fold_left (+) 0 (Hashtbl.to_seq_values (process_input (open_in input) (['X']) (Hashtbl.create 550))));;