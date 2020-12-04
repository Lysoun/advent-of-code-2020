open Printf
open Hashtbl
open List
open String
open Int64
open Str
open Option
open Bool
  
let input = "input.txt"

let is_valid_int str = (string_match (regexp "^[0-9]+$") str 0);;

let is_string_between_values str min max = 
    if (not (is_valid_int str))
        then false
        else let i = (Int64.to_int (Int64.of_string str)) in (i >= min && i <= max);;

let is_birth_year_valid birthYear = (is_string_between_values birthYear 1920 2002);;

let is_issue_year_valid issueYear = (is_string_between_values issueYear 2010 2020);;

let is_expiration_year_valid expirationYear = (is_string_between_values expirationYear 2020 2030);;

let is_eye_color_valid eyeColor = (List.mem eyeColor ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]);;

let is_height_valid height = 
    let l = (String.length height) in 
    if (l < 3) then false else
    let unit = (String.sub height (l - 2) 2) in
    let h = (String.sub height 0 (l - 2)) in

    if ((String.compare unit "cm") == 0) then 
        (is_string_between_values h 150 193) 
    else if ((String.compare unit "in") == 0) then
        (is_string_between_values h 59 76) else false;;

let is_hair_color_valid hairColor = ((string_match (regexp "^#\\| [a-f] [0-9]+$") hairColor 0) && ((String.length hairColor) == 7));;

let is_password_id_valid passportId = ((string_match (regexp "^[0-9]+$") passportId 0) && ((String.length passportId) == 9));;

let mandatory_fields = [
    ("byr", is_birth_year_valid); 
    ("iyr", is_issue_year_valid); 
    ("eyr", is_expiration_year_valid); 
    ("hgt", is_height_valid); 
    ("hcl", is_hair_color_valid); 
    ("ecl", is_eye_color_valid); 
    ("pid", is_password_id_valid)
];;

let and_bool bool1 bool2 = (bool1 && bool2);;

let print_pair a b = (print_string a); (print_string ":"); (print_string b);;

let print_password password = (Hashtbl.iter print_pair password);;

let is_valid password =  
    (List.fold_left 
        and_bool 
        true 
        (List.map 
            (function (field, validationFunction) -> 
                let value = (Hashtbl.find_opt password field) in 
                ((Option.is_some value) && (validationFunction (Option.get value))))
            mandatory_fields
        )
    );;

let init_password () = Hashtbl.create ((List.length mandatory_fields) + 1);;

let rec fill_password password pairs = match pairs with
    | [] -> password
    | h::t -> 
        let splitPair = (String.split_on_char ':' h) in
        (Hashtbl.add password (nth splitPair 0) (nth splitPair 1));
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