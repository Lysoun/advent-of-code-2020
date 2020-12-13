open Printf
open Int64
open String
open List
  
let input = "input.txt";;

let string_to_int str = (Int64.to_int (Int64.of_string str));;

let file = (open_in input);;

(* Ignore first line because it is no longer relevant *)
(input_line file);;

let rec readBuses busesIds currentIndex result product = match busesIds with
    | [] -> (result, product)
    | h::t when (String.compare h "x") = 0 -> (readBuses t (currentIndex + 1) result product)
    | h::t -> 
    let busId = (string_to_int h) in
    (readBuses t (currentIndex + 1) (((busId - currentIndex), busId)::result) (product * busId))
    ;;

let (buses, busIdsProduct) = (readBuses (String.split_on_char ',' (input_line file)) 0 [] 1);;

(*(print_string "product: ");;
(print_int busIdsProduct);;
(print_string "\n");;*)

let compute_coef (busIndex,busId) product = 
    let n = product / busId in
    let rec aux current = (*(print_string " "); (print_int current);*) if ((current * n) mod busId) = 1 then current else (aux (current + 1)) in
(*    (print_string "busIndex: ");
    (print_int busIndex);
    (print_string " busId: ");
    (print_int busId);
        (print_string " n: ");
    (print_int n);*)
        let res = (n * (aux 0)) in
(*    (print_string " e: ");
    (print_int res);
    (print_string "\n");*)
    res
;;

let add_ints i1 i2 = i1 + i2;;

(print_int ((List.fold_left add_ints 0 (map (function (busIndex, busId) -> (busIndex * (compute_coef (busIndex, busId) busIdsProduct))) buses) mod busIdsProduct)));;