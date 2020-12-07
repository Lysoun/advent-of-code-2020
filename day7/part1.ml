open Printf
open Hashtbl
open String
open Str
open Int64
open Seq
open List
open Bool
  
let input = "input.txt";;

type bag = {color:string; children: (string, int) Hashtbl.t};;

let init_bag color = {color = color; children = Hashtbl.create 10};;

let rec parseChildrenBags childrenBags children = match childrenBags with
        | [] -> children
        | h::t -> 
        (string_match (regexp " ?\\([0-9]+\\) \\([a-z]+ [a-z]+\\) bags?.?") h 0);
        (Hashtbl.add children (matched_group 2 h) (Int64.to_int (Int64.of_string (matched_group 1 h))));
        (parseChildrenBags t children)
    ;;

let parseLine line = 
    let splitLine = (Str.split (regexp " bags contain ") line) in
    let bag =  (init_bag (nth splitLine 0)) in
    let childrenBags = (nth splitLine 1) in
    if ((String.compare "no other bags." childrenBags) == 0) then bag else 
        {color = bag.color; children = (parseChildrenBags (split_on_char ',' childrenBags) bag.children)} 
    ;;

let rec input_lines file bags =
   match try (input_line file) with End_of_file -> "" with
      | "" -> bags
      | line -> 
      let bag = (parseLine line) in
      (Hashtbl.add bags bag.color bag);
      (input_lines file bags)
    ;;

let bags = (input_lines (open_in input) (Hashtbl.create 100));;

let print_pair name number = (print_string name); (print_string "->"); (print_int number); (print_string " ");;

let print_pair_bag bagName bag = (print_string bagName); (print_string ";; children: "); (Hashtbl.iter (print_pair) bag.children); (print_string "\n");;

let print_bag bag = (print_pair_bag bag.color bag);;

let color = "shiny gold";;

let or_bool bool1 bool2 = bool1 || bool2;;

let rec containsColor bag = 
    if (Hashtbl.length bag.children) = 0 then (*((print_pair_bag bag.color bag); (print_string "!!! false"); (print_string "\n");*) false
    else if (Hashtbl.mem bag.children color) then (*((print_pair_bag bag.color bag); (print_string "!!! true"); (print_string "\n");*) true
    else (Seq.fold_left (or_bool) false (Seq.map containsColor (Seq.map (function child -> (Hashtbl.find bags child)) (Hashtbl.to_seq_keys bag.children))))
;;

let add_ints int1 int2 = int1 + int2;;

print_int (Seq.fold_left (add_ints) 0 (Seq.map (Bool.to_int) (Seq.map (function bag -> (containsColor bag)) (to_seq_values bags))));;