open Printf
open Int64
open Set
open Int
  
let input = "input.txt"

module IntSet = Set.Make(Int);;

let string_to_int str = (Int64.to_int (Int64.of_string str));;
  
let rec read_adapters file adaptersSet =
   match try (input_line file) with End_of_file -> "" with
      | "" -> adaptersSet
      | line -> (read_adapters file (IntSet.add (string_to_int line) adaptersSet))
    ;;

let adaptersSet = (read_adapters (open_in input) IntSet.empty);;

let rec jolt_differencies adapters jolt1Differencies jolt3Differences = match adapters with
    | [] -> jolt1Differencies * (1 + jolt3Differences)
    | h::[] -> jolt1Differencies * (1 + jolt3Differences)
    | h1::h2::t when (h2 - h1) = 1 -> (jolt_differencies (h2::t) (jolt1Differencies + 1) jolt3Differences)
    | h1::h2::t when (h2 - h1) = 3 -> (jolt_differencies (h2::t) jolt1Differencies (jolt3Differences + 1))
    | h::t -> (jolt_differencies t jolt1Differencies jolt3Differences)
;;

(IntSet.add adaptersSet 0);;
(print_int (jolt_differencies (IntSet.elements adaptersSet) 0 0));;