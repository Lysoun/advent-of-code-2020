open Printf
open Int64
  
let input = "input1.txt"

let rec find_solution element list =
    match list with 
        | [] -> 0
        | t::q when t==(2020-element) -> element*t
        | t::q -> (find_solution element q)
    ;;
  
let rec input_lines file values =
   match try (input_line file) with End_of_file -> "" with
      | "" -> 0
      | line -> 
      let value = (to_int (of_string line)) in
      let result = (find_solution value values) in
      if result == 0 then (input_lines file (value::values)) else result
    ;;

print_int (input_lines (open_in input) []);;