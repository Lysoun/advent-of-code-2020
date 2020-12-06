open Printf
open Int64
open String
  
let input = "input.txt"

let rec find_solution element list =
    match list with 
        | [] -> 0
        | t::q when t==(2020-element) -> element*t
        | t::q -> (find_solution element q)
    ;;

let rec binarySearch entry lowLetter minValue maxValue = 
    let l = (length entry) in
    if (l == 0) 
    then minValue 
    else 
    let tail = (sub entry 1 ((length entry) - 1)) in
    let mid = minValue + ((maxValue - minValue) / 2) in
    if ((get entry 0) == lowLetter) then (binarySearch tail lowLetter minValue mid) else (binarySearch tail lowLetter (mid + 1) maxValue);;
;;

let rec toLine entry = (binarySearch entry 'F' 0 127);;

let rec toColumn entry = (binarySearch entry 'L' 0 7);; 

let rec input_lines file maxSeatId =
   match try (input_line file) with End_of_file -> "" with
      | "" -> maxSeatId
      | line ->       
      let seatId = ((toLine (sub line 0 7)) * 8) + (toColumn (sub line 7 3)) in
      if(seatId > maxSeatId) then (input_lines file seatId) else (input_lines file maxSeatId)
    ;;

print_int (input_lines (open_in input) 0);;