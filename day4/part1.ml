open Printf
open Hashtbl
open List
open String
  
let input = "input.txt"

let mandatory_fields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"];;

let and_bool bool1 bool2 = (bool1 && bool2);;

let is_valid password =  (List.fold_left and_bool true (List.map (function field -> (Hashtbl.mem password field)) mandatory_fields));;

let init_password () = Hashtbl.create ((List.length mandatory_fields) + 1);;

let print_pair a b = (print_string a); (print_string ":"); (print_string b);;

let print_password password = (Hashtbl.iter print_pair password);;

let rec fill_password password pairs = match pairs with
    | [] -> password
    | h::t -> 
        let splitPair = (String.split_on_char ':' h) in
        (add password (nth splitPair 0) (nth splitPair 1));
        (fill_password password t) 
    ;;
  
let rec input_lines file currentPassword passwordValids =
   match try (input_line file) with End_of_file -> " " with
      | " " -> passwordValids
      | "" when (is_valid currentPassword) -> 
        (input_lines file (init_password ()) (passwordValids + 1))
      | "" -> 
        (input_lines file (init_password ()) (passwordValids))
      | line -> (input_lines file (fill_password currentPassword (String.split_on_char ' ' line)) passwordValids)
    ;;

print_int (input_lines (open_in input) (init_password ()) 0);;