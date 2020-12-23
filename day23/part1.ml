open String
open List
open Set
open Int

module IntSet = Set.Make(Int);;

let range n length = 
    let rec aux i = 
        if i = length
        then []
        else (n + i)::(aux (i + 1))
    in
    (aux 0)
;;

let get_array_cyclical array index arrayLength = (Array.get array (index mod arrayLength));;

let array_cyclical_sub array startingIndex subLength   = 
    let arrayLength = (Array.length array) in
    List.map (function a -> (get_array_cyclical array a arrayLength)) (range startingIndex subLength)
;;

let find_current_cup_index cupsArray currentCup =  
    let rec aux i = 
        if (Array.get cupsArray i) = currentCup
        then i
        else (aux (i + 1))
    in
    (aux 0)
;;

let pick_cups cupsArray currentCup =
    let cupsNumber = (Array.length cupsArray) in
    let currentCupIndex = (find_current_cup_index cupsArray currentCup) in
    ((array_cyclical_sub cupsArray (currentCupIndex + 1) 3), (array_cyclical_sub cupsArray (currentCupIndex + 4) (cupsNumber - 3)))
;;

let select_destination_cup cups currentCup = 
    let cupsSet = (IntSet.of_list cups) in
    let rec aux cup = 
        if cup < 1
        then (IntSet.max_elt cupsSet)
        else 
            if (IntSet.mem cup cupsSet)
            then cup
            else (aux (cup - 1))
    in
    (aux (currentCup - 1))
;;

let place_cups destinationCup pickedUpCups remainingCups = 
    let rec aux cups = match cups with 
        | [] -> []
        | h::t when h = destinationCup -> h::pickedUpCups@t
        | h::t -> h::(aux t) 
    in
    (aux remainingCups)
;;

let select_new_current_cup movedCups currentCup = 
    let movedCupsArray = (Array.of_list movedCups) in
    get_array_cyclical movedCupsArray ((find_current_cup_index movedCupsArray currentCup) + 1) (Array.length movedCupsArray)
;;

let execute_move cups currentCup = 
    let cupsArray = (Array.of_list cups) in
    let (pickedUpCups, remainingCups) = (pick_cups cupsArray currentCup) in
    let destinationCup = (select_destination_cup remainingCups currentCup) in
    let movedCups = (place_cups destinationCup pickedUpCups remainingCups) in
    let newCurrentCup = (select_new_current_cup movedCups currentCup) in
    (movedCups, newCurrentCup)
;;

let move_cups cups = 
    let rec move_cups_100_times i (cups, currentCup) = 
        if i >= 100 
            then cups
        else
            (move_cups_100_times (i + 1) (execute_move cups currentCup))
    in

    move_cups_100_times 0 (cups, (List.hd cups))
;;

let input = "589174263";;
let cups = List.map (int_of_string) (List.map (function character -> String.make 1 character) (List.of_seq (String.to_seq input)));;
let movedCups = Array.of_list (move_cups cups);;
List.iter (function cup -> print_int cup) (array_cyclical_sub movedCups ((find_current_cup_index movedCups 1) + 1) (Array.length movedCups - 1));;