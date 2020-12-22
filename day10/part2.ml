open Printf
open Set
open Int
open Hashtbl
open List
  
let input = "input.txt"

module IntSet = Set.Make(Int);;
  
let rec read_adapters file adaptersSet =
   match try (input_line file) with End_of_file -> "" with
      | "" -> adaptersSet
      | line -> (read_adapters file (IntSet.add (int_of_string line) adaptersSet))
    ;;

let adaptersSet = (read_adapters (open_in input) IntSet.empty);;

let count_adapter_possibilities adapter possibilitiesPerAdapter = 
    (List.fold_left 
        (+) 
        0 
        (List.map 
            (function differency -> 
                let lowerAdapter = (adapter - differency) in
                if Hashtbl.mem possibilitiesPerAdapter lowerAdapter
                then Hashtbl.find possibilitiesPerAdapter lowerAdapter
                else 0
            )
            [1;2;3]
        )
    );;

let rec count_possibilities adapters possibilitiesPerAdapter = match adapters with
    | [] -> 0
    | h::t -> let adapterPossibilities = (count_adapter_possibilities h possibilitiesPerAdapter) in
    if (List.length t) = 0
    then adapterPossibilities
    else 
        ((Hashtbl.add possibilitiesPerAdapter h adapterPossibilities);
        (count_possibilities t possibilitiesPerAdapter))
;;

let possibilitiesPerAdapter = (Hashtbl.create ((IntSet.cardinal adaptersSet) + 1));;
(Hashtbl.add possibilitiesPerAdapter 0 1);;
(print_int (count_possibilities (IntSet.elements adaptersSet) possibilitiesPerAdapter));