open Printf
open Hashtbl
open List
open Seq

let inputFilename = "input.txt";;
let inputFile = (open_in inputFilename);;

let rec read_input x cubes = 
    match try (input_line inputFile) with End_of_file -> "" with
      | "" -> cubes
      | line -> 
        (String.iteri (function y -> function character -> if character = '#' then (Hashtbl.add cubes [x; y; 0; 0] true)) line);
        (read_input (x + 1) cubes)
;;

let conwayCubes = (read_input 0 (Hashtbl.create 64));;

let print_coordinates coordinates = (print_string "("); List.iter (function c -> (print_int c); (print_string ",")) coordinates; (print_string ")") ;;

let rec get_neighbours_coordinates coordinates = match coordinates with
    | [] -> [[]]
    | h::t -> let truc = (List.map (function c -> c + h) [-1; 0; 1]) in
        let b = (List.map (function coordinatesList -> (List.map (function c -> c::coordinatesList) truc)) (get_neighbours_coordinates t)) in
        let neighboursCoordinates = (List.concat b) in
        neighboursCoordinates
;;

let find_or_default key hasthbl default = 
    if Hashtbl.mem hasthbl key then Hashtbl.find hasthbl key else default
;;

let rec make_coordinate_evolve coordinates cubes nextCycleCubes = 
    let isActive = (find_or_default coordinates cubes false) in
    let neighboursCoordinates = (get_neighbours_coordinates coordinates) in

    let activeNeighbours = 
        List.fold_left 
            (+) 
            0 
            (List.map (function neighbourCoordinates -> if (find_or_default neighbourCoordinates cubes false) then 1 else 0) neighboursCoordinates) in
    
    (if (isActive && ((activeNeighbours = 3) || (activeNeighbours = 4))) || ((not isActive) && (activeNeighbours = 3))
    then (Hashtbl.add nextCycleCubes coordinates true)
    else (Hashtbl.add nextCycleCubes coordinates false));

    if isActive 
    then List.iter 
        (function neighbourCoordinates -> 
            if (not (Hashtbl.mem cubes neighbourCoordinates)) && (not (Hashtbl.mem nextCycleCubes neighbourCoordinates)) 
            then (make_coordinate_evolve neighbourCoordinates cubes nextCycleCubes)
        ) 
        neighboursCoordinates
;;

let print_cubes cubes = 
    (Hashtbl.iter 
        (function coordinates -> 
            function isActive -> 
                if isActive then
                ((print_coordinates coordinates); 
                (print_string ": "); 
                (print_string "active");
                (print_string "\n"))
        ) 
        cubes
    );;

let rec run_cycles max i cubes = 
    if i = max
    then cubes
    else (
        (print_string "Cycle ");
        (print_int (i + 1));
        (print_string "\n");
        let nextCycleCubes = (Hashtbl.create 200) in
        (Hashtbl.iter (function c -> function isActive -> (make_coordinate_evolve c cubes nextCycleCubes)) cubes);
        (print_cubes nextCycleCubes);
        (print_string "\n");
        (run_cycles max (i + 1) nextCycleCubes)
    )
;;

(print_string "Before cycles\n");;
(print_cubes conwayCubes);;
(print_string "\n");;

let result = (run_cycles 6 0 conwayCubes);;
(print_string "Number of cubes active: ");;
(print_int (Seq.fold_left (+) 0 (Seq.map (function isActive -> if isActive then 1 else 0) (Hashtbl.to_seq_values result))));;