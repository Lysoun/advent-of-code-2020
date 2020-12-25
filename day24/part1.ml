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

let rec flip_tiles tiles =
    match try (input_line inputFile) with End_of_file -> "" with
      | "" -> tiles
      | line -> 
        let instructions = (line_to_instructions_list line) in
        let tilePosition = (compute_position (0, 0) instructions) in
        (print_instructions instructions);
        (print_flip tilePosition);
        
        (if Hashtbl.mem tiles tilePosition 
        then Hashtbl.replace tiles tilePosition (1 - (Hashtbl.find tiles tilePosition))
        else Hashtbl.add tiles tilePosition 1);

        (flip_tiles tiles)
;;

let tiles = (flip_tiles (Hashtbl.create 478));;
(print_int (List.fold_left (+) 0 (List.of_seq (Hashtbl.to_seq_values tiles))));;
