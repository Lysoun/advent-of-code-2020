open Printf
open Int64
  
let input = "input2.txt"

let rec find_solution_part2 element1 element2 list =
    match list with 
        | [] -> 0
        | t::q when (element1+element2+t == 2020) -> element1*element2*t
        | t::q -> (find_solution_part2 element1 element2 q)
    ;;

let rec find_solution element list =
    match list with 
        | [] -> 0
        | t::q -> let result = (find_solution_part2 element t q) in
        if result == 0 then (find_solution element q) else result
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