open List
open Printf
open String
open Hashtbl

let move instruction (x, y) = match instruction with
    | "e" -> (x + 1, y)
    | "w" -> (x - 1, y)
    | "ne" -> (x + 1, y + 1)
    | "se" -> (x, y - 1)
    | "nw" -> (x, y + 1)
    | _ -> (x - 1, y - 1)
;;

let string_of_char character = String.make 1 character;; 

let line_to_instructions_list line = 
    let rec aux characters = match characters with
        | [] -> []
        | h1::h2::t when (h1 = 'n') || (h1 = 's') -> let instruction = String.concat "" [(string_of_char h1); (string_of_char h2)] in 
        instruction::(aux t)
        | h::t -> (string_of_char h)::(aux t)
    in
    (aux (List.of_seq (String.to_seq line)))
;;

let rec compute_position coordinates instructions = match instructions with
    | [] -> coordinates
    | h::t -> (compute_position (move h coordinates) t)
;;

let inputFilename = "input.txt";;
let inputFile = (open_in inputFilename);;

let print_flip (x, y) = 
    (print_string "position: (");
    (print_int x);
    (print_string ",");
    (print_int y);
    (print_string ") was flipped!\n");
;;

let print_instructions instructions = (List.iter (function i -> (print_string i); (print_string " ")) instructions); (print_string "\n");;

let black = 1;;
let white = 0;;

let rec flip_tiles tiles =
    match try (input_line inputFile) with End_of_file -> "" with
      | "" -> tiles
      | line -> 
        let instructions = (line_to_instructions_list line) in
        let tilePosition = (compute_position (0, 0) instructions) in
        
        (if Hashtbl.mem tiles tilePosition 
        then Hashtbl.replace tiles tilePosition (1 - (Hashtbl.find tiles tilePosition))
        else Hashtbl.add tiles tilePosition black);

        (flip_tiles tiles)
;;

let init_tiles () = (Hashtbl.create 10000);;

let tiles = (flip_tiles (Hashtbl.create 478));;

let daysNumber = 100;;

let get_adjacent_tiles_coordinates coordinates = (List.map (function instruction -> (move instruction coordinates)) ["ne"; "nw"; "w"; "e"; "se"; "sw"]);;

let count_adjacent_black_tiles tiles coordinates = 
    (List.fold_left 
        (+) 
        0 
        (
            List.map 
            (function adjacentTile -> 
                if (Hashtbl.mem tiles adjacentTile) && ((Hashtbl.find tiles adjacentTile) = black)
                then 1
                else 0
            )
            (get_adjacent_tiles_coordinates coordinates)
        )
    );;

let get_tile_color tiles coordinates = 
    if Hashtbl.mem tiles coordinates then Hashtbl.find tiles coordinates else white;;

let flip_tile_to_color nextDayTiles coordinates color = (Hashtbl.add nextDayTiles coordinates color);;

let rec flip_tile_depending_on_adjacent_black_tiles tiles nextDayTiles coordinates = 
    let tileColor = (get_tile_color tiles coordinates) in
    let adjacentBlackTiles = (count_adjacent_black_tiles tiles coordinates) in

    (if tileColor = black && ((adjacentBlackTiles = 0) || (adjacentBlackTiles > 2))
    then (flip_tile_to_color nextDayTiles coordinates white)
    else 
        if tileColor = white && (adjacentBlackTiles = 2)
        then (flip_tile_to_color nextDayTiles coordinates black)
        else (flip_tile_to_color nextDayTiles coordinates tileColor)
    );

    if tileColor = black then
        (List.iter 
            (function c -> (if (not (Hashtbl.mem tiles c) && (not (Hashtbl.mem nextDayTiles c))) then flip_tile_depending_on_adjacent_black_tiles tiles nextDayTiles c)) 
            (get_adjacent_tiles_coordinates coordinates)
        )
;;

let print_black_tiles tiles = (print_int (List.fold_left (+) 0 (List.of_seq (Hashtbl.to_seq_values tiles))));;

let day_flip_tiles tiles = 
    let nextDayTiles = (init_tiles ()) in
    (List.iter (function coordinates -> (flip_tile_depending_on_adjacent_black_tiles tiles nextDayTiles coordinates)) (List.of_seq (Hashtbl.to_seq_keys tiles))); 
    nextDayTiles
;;

let rec flip_tiles_for_days max i tiles = 
    if i <= max
    then (
        (print_string "Day ");
        (print_int i);
        (print_string ": ");
        let nextDayTiles = (day_flip_tiles tiles) in
        (print_black_tiles nextDayTiles);
        (print_string "\n");
        (flip_tiles_for_days max (i + 1) nextDayTiles)
    )
;;

(flip_tiles_for_days daysNumber 1 tiles);;
