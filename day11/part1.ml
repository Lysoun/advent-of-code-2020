open Printf
open String
open Array
open Bool

let input = "input.txt"
  
let rec read_input file ferry lineNumber =
   match try (input_line file) with End_of_file -> "" with
      | "" -> ferry
      | line ->       
      (Array.set ferry lineNumber (Array.of_seq (String.to_seq line)));
      (read_input file ferry (lineNumber + 1))
    ;;

let ferry = (read_input (open_in input) (Array.make 90 (Array.make 0 '.')) 0);;

let is_seat_occupied (lineNumber, columnNumber) linesNumber columnsNumber ferry =
    if (lineNumber < 0 || lineNumber >= linesNumber) || (columnNumber < 0 || columnNumber >= columnsNumber) 
    then false
    else (Array.get (Array.get ferry lineNumber) columnNumber) = '#';;

let get_nearby_occupied_seats_number (lineNumber, columnNumber) linesNumber columnsNumber ferry = 
    (List.fold_left 
        (+) 
        0 
        (List.map 
            (function coordinates -> if is_seat_occupied coordinates linesNumber columnsNumber ferry then 1 else 0)
            (List.map (function (l, c) -> (lineNumber + l, columnNumber + c)) [(-1, -1);(-1, 1);(-1, 0); (0, -1); (0, 1); (1, -1); (1, 1); (1, 0)])
        )
    );;

let next_step_for_seat character coordinates linesNumber columnsNumber ferry = 
    let nearbyOccupiedSeatsNumber = (get_nearby_occupied_seats_number coordinates linesNumber columnsNumber ferry) in
    if (character = 'L') && (nearbyOccupiedSeatsNumber = 0) 
    then '#'
    else if (character = '#') && (nearbyOccupiedSeatsNumber >= 4) 
        then 'L'
        else character
;;

let rec next_step ferry next_ferry lineNumber columnNumber linesNumber columnsNumber changed = 
    if lineNumber = linesNumber 
    then (next_ferry, changed)
    else 
        if columnNumber = columnsNumber
        then (next_step ferry next_ferry (lineNumber + 1) 0 linesNumber columnsNumber changed)
        else (
            let character = (Array.get (Array.get ferry lineNumber) columnNumber) in
            let next_character = (next_step_for_seat character (lineNumber, columnNumber) linesNumber columnsNumber ferry) in
            (Array.set (Array.get next_ferry lineNumber) columnNumber next_character);
            (next_step ferry next_ferry lineNumber (columnNumber + 1) linesNumber columnsNumber (changed || (character != next_character))))
    ;;

let rec simulate_seating ferry linesNumber columnsNumber = 
    let (next_ferry, changed) = (next_step ferry (Array.map (function column -> Array.copy column) ferry) 0 0 linesNumber columnsNumber false) in
    if changed
    then simulate_seating next_ferry linesNumber columnsNumber
    else next_ferry;;

let stableFerry = (simulate_seating ferry (Array.length ferry) (Array.length (Array.get ferry 0)));;

print_int (Array.fold_left (+) 0 (Array.map (function line -> (Array.fold_left (+) 0 (Array.map (function character -> if character = '#' then 1 else 0) line))) stableFerry));;