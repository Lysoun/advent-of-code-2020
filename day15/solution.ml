open Hashtbl
open List

let startingNumbers = [14;1;17;0;3;20];;

let spokenNumbers = Hashtbl.create 2020;;
let iteri_function index number = (Hashtbl.add spokenNumbers number (index + 1));;
(List.iteri (iteri_function) startingNumbers);;

let rec speak_numbers maxIndex index spokenNumbers lastSpokenNumber =
    if index >= maxIndex 
    then lastSpokenNumber
    else
        let newIndex = (index + 1) in
        if Hashtbl.mem spokenNumbers lastSpokenNumber
        then (let age = (index - Hashtbl.find spokenNumbers lastSpokenNumber) in
            (Hashtbl.replace spokenNumbers lastSpokenNumber index); (speak_numbers maxIndex newIndex spokenNumbers age))
        else ((Hashtbl.add spokenNumbers lastSpokenNumber index); (speak_numbers maxIndex newIndex spokenNumbers 0))
;;

let startingNumbersLength = (List.length startingNumbers);;
print_int (speak_numbers 30000000 (startingNumbersLength + 1) spokenNumbers 0);;