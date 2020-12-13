open Printf
open Int64
open String
  
let input = "input.txt";;

let string_to_int str = (Int64.to_int (Int64.of_string str));;

let file = (open_in input);;
let earliestTimestamp = (string_to_int (input_line file));;
let buses = (String.split_on_char ',' (input_line file));;

let rec find_earliest_bus earliestTimestamp buses earliestBus minimumWait = match buses with
    | [] -> (earliestBus * minimumWait)
    | h::t when (compare h "x") = 0 -> (find_earliest_bus earliestTimestamp t earliestBus minimumWait)
    | h::t -> let busId = (string_to_int h) in 
    let wait = (busId - (earliestTimestamp mod busId)) in
    if wait < minimumWait
    then (find_earliest_bus earliestTimestamp t busId wait)
    else (find_earliest_bus earliestTimestamp t earliestBus minimumWait) 
;;

(print_int (find_earliest_bus earliestTimestamp buses 0 earliestTimestamp));;

