open Printf
open Str
open List
open String
open Bool
open Set
open Array

let input = "input.txt";;

(* Read & parse constraints *)

type field = {name: string; constraints: (int*int) list};;

let constraintExtractionRegexp = Str.regexp "\\(.*\\): \\([0-9]+\\)-\\([0-9]+\\) or \\([0-9]+\\)-\\([0-9]+\\)";;
let parse_constraint line = 
    (Str.string_match constraintExtractionRegexp line 0);
    {
        name = (Str.matched_group 1 line); 
        constraints = [
            ((int_of_string (Str.matched_group 2 line)), (int_of_string (Str.matched_group 3 line))); 
            ((int_of_string (Str.matched_group 4 line)), (int_of_string (Str.matched_group 5 line)))
        ]
    }
;;


let rec read_constraints file constraints =
   match try (input_line file) with End_of_file -> "" with
      | "" -> constraints
      | line -> (read_constraints file ((parse_constraint line)::constraints))
    ;;

let file = (open_in input);;
let constraints = (read_constraints file []);;

(* Read my ticket *)

(input_line file);;
let myTicketFields = (Array.of_list (List.map (int_of_string) (String.split_on_char ',' (input_line file))));;
(input_line file);;
(input_line file);;

(* Find out all names possibilies for each field depending on the valid tickets *)

module StringSet = Set.Make(String);;

let constraint_applies value c = 
    (List.fold_left (||) false (List.map (function (min, max) -> (value >= min) && (value <= max)) c))
;;

let rec find file constraints fieldsNamesPossibilities = 
    match try (input_line file) with End_of_file -> "" with
        | "" -> fieldsNamesPossibilities
        | ticket -> 
            let ticketFields = (List.map (int_of_string) (String.split_on_char ',' ticket)) in
            let possibleConstraints = 
                (Array.of_list (List.map 
                    (function field -> (List.filter (function possibleField -> (constraint_applies field possibleField.constraints)) constraints)) 
                    ticketFields
                )) in
            (* If ticket contains fields that do not correspond to any constraints, ignore it *)
            if (Array.fold_left (||) false (Array.map (function fieldNamesPossibilities -> (List.length fieldNamesPossibilities) = 0) possibleConstraints))
            then (find file constraints fieldsNamesPossibilities)
            else (find 
                    file 
                    constraints 
                    (
                        Array.mapi 
                        (function index -> 
                            (function namesPossibilities -> 
                                (StringSet.inter namesPossibilities (StringSet.of_list (List.map (function f -> f.name) (Array.get possibleConstraints index)))))) 
                        fieldsNamesPossibilities
                    )
                )
    ;;

let fieldsNamesPossibilities = (find file constraints (Array.make (List.length constraints) (StringSet.of_list (List.map (function c -> c.name) constraints))));;

(* Display all names possibilities for each index to understand what is happening *)
(print_string "All names possibilities for each field: \n");
(Array.iteri (function index -> (function names -> (print_int index); (print_string ": "); (StringSet.iter (function name -> (print_string "\""); (print_string name); (print_string "\""); (print_string " ")) names); (print_string "\n"))) fieldsNamesPossibilities);;

(* When a name in the only possibility for a specific field, that field has that name and thus other fields cannot have it *)
let rec remove_unique_name_possibilities_from_other_fields_possibilities index maxIndex fieldsNamesPossibilities = match index with 
    | _ when index >= maxIndex -> fieldsNamesPossibilities
    | _ when (StringSet.cardinal (Array.get fieldsNamesPossibilities index)) = 1 -> 
        let fieldName = (StringSet.choose (Array.get fieldsNamesPossibilities index)) in
        (remove_unique_name_possibilities_from_other_fields_possibilities 
            (index + 1) 
            maxIndex 
            (Array.mapi (function i -> function namesSet -> if i = index then namesSet else (StringSet.remove fieldName namesSet)) fieldsNamesPossibilities)
        )
    | _ -> (remove_unique_name_possibilities_from_other_fields_possibilities (index + 1) maxIndex fieldsNamesPossibilities)
;;    

let rec reduce_to_one_name_possibility fieldsNamesPossibilities =
    (* Reduction is done when there is exactly one possibility for each field *)
    if Array.fold_left (&&) true (Array.map (function namesPossibilities -> (StringSet.cardinal namesPossibilities) = 1) fieldsNamesPossibilities)
    then (Array.map (function namesPossibilities -> (StringSet.choose namesPossibilities )) fieldsNamesPossibilities)
    else (reduce_to_one_name_possibility (remove_unique_name_possibilities_from_other_fields_possibilities 0 (Array.length fieldsNamesPossibilities) fieldsNamesPossibilities))
;;

(print_string "\nFields names: \n");;
let fieldsNames = (reduce_to_one_name_possibility fieldsNamesPossibilities);;
(Array.iteri (function index -> (function name -> (print_int index); (print_string ": "); (print_string "\""); (print_string name); (print_string "\""); (print_string "\n"))) fieldsNames);;

(* Find the values for fields containing "departure" in my ticket and multiply them to find the result! *)
let contains str subStr = (Str.string_match (Str.regexp (String.concat "" [".*" ; subStr ; ".*"])) str 0);;

(print_string "\nThe result: ");;
let times int1 int2 = int1 * int2;;
(print_int (Array.fold_left (times) 1 (Array.mapi (function index -> function name -> if (contains name "departure") then (Array.get myTicketFields index) else 1) fieldsNames)));;
