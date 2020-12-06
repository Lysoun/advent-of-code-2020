open Printf
open Int64
open String
open Array
open Bool
  
let input = "input.txt";;

let rec find_solution element list =
    match list with 
        | [] -> 0
        | t::q when t == (2020 - element) -> element * t
        | t::q -> (find_solution element q)
    ;;

let rec binarySearch entry lowLetter minValue maxValue = 
    let l = (String.length entry) in
    if (l == 0) 
    then minValue 
    else 
    let tail = (String.sub entry 1 ((String.length entry) - 1)) in
    let mid = minValue + ((maxValue - minValue) / 2) in
    if ((String.get entry 0) == lowLetter) then (binarySearch tail lowLetter minValue mid) else (binarySearch tail lowLetter (mid + 1) maxValue);;
;;

let rec toLine entry = (binarySearch entry 'F' 0 127);;

let rec toColumn entry = (binarySearch entry 'L' 0 7);; 

let rec input_lines file plane =
   match try (input_line file) with End_of_file -> "" with
      | "" -> plane
      | line ->       
      let lineNumber = (toLine (String.sub line 0 7)) in 
      let columnNumber = (toColumn (String.sub line 7 3)) in
      (set (Array.get plane lineNumber) columnNumber false);
      (input_lines file plane)
    ;;

let rec search_empty_seat plane currentLine currentColumn = 
    if (Array.get (Array.get plane currentLine) currentColumn) then ((currentLine * 8) + currentColumn) else
    if (currentColumn == 7) then (search_empty_seat plane (currentLine + 1) 0) else (search_empty_seat plane (currentLine) (currentColumn + 1));;

let plane = (Array.map (function column -> (make 8 true)) (make 128 (make 0 true)));;
let fullPlane = (input_lines (open_in input) plane);;
(print_int (search_empty_seat fullPlane 1 0));;