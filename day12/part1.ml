open Printf
open Array
open Int
  
let input = "input.txt";;

let directions = (Array.of_list (['E'; 'S'; 'W'; 'N']));;

let getDirectionIndex direction = match direction with 
    | 'E' -> 0
    | 'S' -> 1
    | 'W' -> 2
    | _ -> 3
;;

let turn currentlyFacing rotationDirection rotationAngle = match rotationDirection with
    | 'L' -> (Array.get directions ((((getDirectionIndex currentlyFacing) - (rotationAngle / 90)) + 4) mod 4))
    | _ -> (Array.get directions ((((getDirectionIndex currentlyFacing) + (rotationAngle / 90)) + 4) mod 4))
;;

let move direction number (x, y) = match direction with
    | 'E' -> (x + number, y)
    | 'S' -> (x, y - number)
    | 'W' -> (x - number, y)
    | _ -> (x, y + number)
;;
  
let rec input_lines file (x, y) currentlyFacing  =
   match try (input_line file) with End_of_file -> "" with
      | "" -> (Int.abs x) + (Int.abs y)
      | line -> 
      let value = (int_of_string (String.sub line 1 ((String.length line) - 1))) in
      let instruction = (String.get line 0) in
      if (instruction = 'L') || (instruction = 'R') 
      then (input_lines file (x,y) (turn currentlyFacing instruction value))
      else if (instruction = 'F') then (input_lines file (move currentlyFacing value (x,y)) currentlyFacing)
      else (input_lines file (move instruction value (x,y)) currentlyFacing)
    ;;

print_int (input_lines (open_in input) (0, 0) 'E');;