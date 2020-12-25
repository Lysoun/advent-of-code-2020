open Printf

let inputFile = (open_in "input.txt");;
let cardPublicKey = int_of_string (input_line inputFile);;
let doorPublicKey = int_of_string (input_line inputFile);;

let compute_next_value subjectNumber currentValue = (currentValue * subjectNumber) mod 20201227;;

let rec compute_loop_size publicKey subjectNumber currentValue loopSize = 
    let nextValue = (compute_next_value subjectNumber currentValue) in
    if nextValue = publicKey
    then loopSize
    else (compute_loop_size publicKey subjectNumber nextValue (loopSize + 1))
;;

let subjectNumber = 7;;
let cardLoopSize = (compute_loop_size cardPublicKey subjectNumber 1 1);;
let doorLoopSize = (compute_loop_size doorPublicKey subjectNumber 1 1);;

let rec transform i subjectNumber currentValue loopSize = 
    if i = loopSize
    then currentValue
    else (transform (i + 1) subjectNumber (compute_next_value subjectNumber currentValue) loopSize)
;;

(print_int (transform 0 cardPublicKey 1 doorLoopSize));;
