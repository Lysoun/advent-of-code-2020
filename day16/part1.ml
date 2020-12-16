open Printf
open Str
open Int64
open List
open String
open Bool

let input = "input.txt";;

(* A constraint is seen as a list of pairs, each pair being bornes for a valid interval of values *)
let or_bool bool1 bool2 = bool1 || bool2;;

let is_invalid value c = 
    (List.fold_left (&&) true (List.map (function (min, max) -> (value < min) || (value > max)) c))
;;

let string_to_int str = (Int64.to_int (Int64.of_string str));;

let constraintExtractionRegexp = Str.regexp ".*: \\([0-9]+\\)-\\([0-9]+\\) or \\([0-9]+\\)-\\([0-9]+\\)";;
let parse_constraint line = 
    (Str.string_match constraintExtractionRegexp line 0);
    [
        ((string_to_int (Str.matched_group 1 line)), (string_to_int (Str.matched_group 2 line))); 
        ((string_to_int (Str.matched_group 3 line)),(string_to_int (Str.matched_group 4 line)))
    ]
;;


let rec read_constraints file constraints =
   match try (input_line file) with End_of_file -> "" with
      | "" -> constraints
      | line -> (read_constraints file ((parse_constraint line)::constraints))
    ;;

let file = (open_in input);;
let constraints = (read_constraints file []);;

(* Ignore my ticket and nearby tickets title *)
(input_line file);;
(input_line file);;
(input_line file);;
(input_line file);;

let apply_constraints value constraints = 
    if (List.fold_left (&&) true (List.map (function c -> (is_invalid value c)) constraints))
    then value 
    else 0
;;

let rec count_invalid_fields_among_nearby_tickets file constraints sum = 
    match try (input_line file) with End_of_file -> "" with
        | "" -> sum
        | ticket -> 
            let ticketFields = (List.map (string_to_int) (String.split_on_char ',' ticket)) in
            let ticketScanningErrorRate = (List.fold_left (+) 0 (List.map (function value -> (apply_constraints value constraints)) ticketFields)) in
            (count_invalid_fields_among_nearby_tickets file constraints (ticketScanningErrorRate + sum))
    ;;

print_int (count_invalid_fields_among_nearby_tickets file constraints 0);;