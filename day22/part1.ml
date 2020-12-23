open Printf
open List

let inputFile = (open_in "input.txt");;

let rec read_deck deck =
    match try (input_line inputFile) with End_of_file -> "" with
      | "" -> (List.rev deck)
      | line -> (read_deck ((int_of_string line)::deck))
;;

let print_deck deck = (print_string "deck: "); (List.iter (function card -> (print_int card); (print_string " ")) deck); (print_string "\n");;

(* Ignore "Player 1 deck : " *)
(input_line inputFile);;

let player1Deck = (read_deck []);;

(* Ignore "Player 2 deck : " *)
(input_line inputFile);;

let player2Deck = (read_deck []);;

let card_played deck = (List.hd deck);;

let play_move (player1Deck, player2Deck) =
    let player1CardPlayed = (card_played player1Deck) in
    let player2CardPlayed = (card_played player2Deck) in

    let remainingPlayer1DeckAfterPlay = (List.tl player1Deck) in
    let remainingPlayer2DeckAfterPlay = (List.tl player2Deck) in

    if player1CardPlayed > player2CardPlayed
    then ((remainingPlayer1DeckAfterPlay @ [player1CardPlayed; player2CardPlayed]), remainingPlayer2DeckAfterPlay)
    else (remainingPlayer1DeckAfterPlay, (remainingPlayer2DeckAfterPlay @ [player2CardPlayed; player1CardPlayed]))
;;

let rec play (player1Deck, player2Deck) =
    if (List.length player2Deck) = 0
    then player1Deck
    else if (List.length player1Deck) = 0
        then player2Deck
        else (play (play_move (player1Deck,player2Deck)))
;;

let winningDeck = (play (player1Deck, player2Deck));;
(print_int (List.fold_left (+) 0 (List.mapi (function index -> function cardNumber -> (index + 1) * cardNumber) (List.rev winningDeck))));;