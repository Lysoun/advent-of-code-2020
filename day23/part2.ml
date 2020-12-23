open Hashtbl
open String
open List
open Array

module IntSet = Set.Make(Int);;

let input = "589174263";;

let cupsArray = Array.of_list (List.map (int_of_string) (List.map (function character -> String.make 1 character) (List.of_seq (String.to_seq input))));;
let cupsNumber = 1000000;;

let print_cups cups = 
    let rec aux printedNumber cup =
        if printedNumber < 2
        then ((print_int cup); (print_string ";"); (aux (printedNumber + 1) (Hashtbl.find cups cup)))
    in
    (aux 0 (Hashtbl.find cups 1));
    (print_string "\n")
;;

let pick_cups cups currentCup = 
    let pickedUpCups = (Array.make 3 0) in
    
    let rec pick_cups_aux cup index = 
        if index <= 2
        then (
            let nextCup = (Hashtbl.find cups cup) in
            (Array.set pickedUpCups index nextCup);
            (pick_cups_aux (nextCup) (index + 1))
        )
    in

    (pick_cups_aux currentCup 0);
    (Hashtbl.replace cups currentCup (Hashtbl.find cups (Array.get pickedUpCups 2)));
    pickedUpCups
;;

let select_destination_cup cups pickedUpCups currentCup = 
    let pickedUpCupsSet = (IntSet.of_seq (Array.to_seq pickedUpCups)) in
    let rec aux cup = 
        if cup < 1
        then (aux cupsNumber)
        else 
            if (IntSet.mem cup pickedUpCupsSet)
            then (aux (cup - 1))
            else cup
    in
    (aux (currentCup - 1))
;;

let place_cups destinationCup pickedUpCups cups = 
    let cupAfterPickedUpCups = (Hashtbl.find cups destinationCup) in
    (Hashtbl.replace cups destinationCup (Array.get pickedUpCups 0));
    (Hashtbl.replace cups (Array.get pickedUpCups 2) cupAfterPickedUpCups);
;;

let select_new_current_cup cups currentCup = 
    Hashtbl.find cups currentCup
;;

let execute_move cups currentCup = 
    let pickedUpCups = (pick_cups cups currentCup) in
    let destinationCup = (select_destination_cup cups pickedUpCups currentCup) in
    (place_cups destinationCup pickedUpCups cups);
    let newCurrentCup = (select_new_current_cup cups currentCup) in
    (cups, newCurrentCup)
;;

let move_cups cups = 
    let rec move_cups_aux i movesNumber (cups, currentCup) = 
        if i >= movesNumber 
            then cups
        else
            (move_cups_aux (i + 1) movesNumber (execute_move cups currentCup))
    in

    move_cups_aux 0 10000000 (cups, 5)
;;

let cups = Hashtbl.create cupsNumber;;
(Array.iteri (function index -> function cup -> (Hashtbl.add cups cup (Array.get cupsArray ((index + 1) mod 9)))) cupsArray);;

let print_all_cups cups = 
    let rec aux printedNumber cup =
        if printedNumber < (cupsNumber - 1)
        then ((print_int cup); (aux (printedNumber + 1) (Hashtbl.find cups cup)))
    in
    (aux 0 (Hashtbl.find cups 1));
    (print_string "\n")
;;

(Hashtbl.add cups (Array.get cupsArray 8) 10);;

let fillCups = 
    for i = 10 to 999999 do
        (Hashtbl.add cups i (i + 1))
    done
;;

(fillCups);;

(Hashtbl.add cups 1000000 (Array.get cupsArray 0));;

(move_cups cups);;
(print_cups cups);;