open Printf
open Int64
open String
open Str
open Array
open Hashtbl
open Seq

let input = "input.txt";;

let maskExtractionRegexp = Str.regexp "^mask = \\(.+\\)";;
let writingMemoryExtractionRegexp = Str.regexp "^mem\\[\\([0-9]+\\)\\] = \\([0-9]+\\)";;

let string_to_int str = (Int64.to_int (Int64.of_string str));;

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

let apply_bit_mask index bit binary = match bit with 
    | '1' -> (Array.set binary index 1)
    | '0' -> (Array.set binary index 0)
    | _ -> ()
;;

let apply_mask mask str = 
    let binary = (int_to_binary (string_to_int str)) in
    let iter_function index bit = (apply_bit_mask index bit binary) in
    (Array.iteri iter_function mask);
    (binary_to_int binary)
;;

let rec process_input file currentMask memory =
   match try (input_line file) with End_of_file -> "" with
      | "" -> memory
      | line when (Str.string_match maskExtractionRegexp line 0) -> (process_input file (Array.of_seq (String.to_seq (Str.matched_group 1 line))) memory)
      | line ->  
        (Str.string_match writingMemoryExtractionRegexp line 0); 
        let key = (string_to_int (Str.matched_group 1 line)) in
        let value = (apply_mask currentMask (Str.matched_group 2 line)) in
        (Hashtbl.replace memory key value);
        (process_input file currentMask memory)
    ;;

let add_ints i1 i2 = i1 + i2;;

print_int (Seq.fold_left (add_ints) 0 (Hashtbl.to_seq_values (process_input (open_in input) (Array.make 0 'X') (Hashtbl.create 550))));;