open Printf
open Int64
open Int
open Set
open List
  
let input = "input.txt"

let sample_size = 25;;

module IntSet = Set.Make(Int);;

let string_to_int str = (Int64.to_int (Int64.of_string str));;

let rec read_sample file sample lineNumber  =
   match try (input_line file) with End_of_file -> "" with
      | "" -> sample
      | line -> 
      let newSample = sample@[(string_to_int line)]  in
      if lineNumber = (sample_size - 1) 
      then newSample
      else (read_sample file newSample (lineNumber + 1))
    ;;

let file = (open_in input);;
let sample = (read_sample file [] 0);;

let rec is_valid_aux target element list =
    match list with 
        | [] -> false
        | h::t when h = (target-element) -> true
        | h::t -> (is_valid_aux target element t)
    ;;

let rec is_valid number sampleList = match sampleList with 
    | [] -> false 
    | h::t when (is_valid_aux number h t) -> true
    | h::t -> (is_valid number t)
;;

let rec find_invalid_number file sample  =
   match try (input_line file) with End_of_file -> "" with
      | "" -> 0
      | line -> 
        let number = (string_to_int line) in
(*        (print_int number);
        (print_string "\n");
        (print_string "sample: ");
        (List.iter (function n -> (print_int n); (print_string " ")) sample);
        (print_string "\n");*)
        if not (is_valid number sample) 
        then number
        else (find_invalid_number file ((List.tl sample)@[number]))
    ;;

print_int (find_invalid_number file sample);;