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

let init_previous_rounds () = (Hashtbl.create 10000);;

let decks_after_player_one_wins player1Deck player1Card player2Deck player2Card = ((player1Deck @ [player1Card; player2Card]), player2Deck);;

let decks_after_player_two_wins player1Deck player1Card player2Deck player2Card  = (player1Deck, (player2Deck @ [player1Card; player2Card]));;

let rec sub_list l length = 
    if length = 0
    then []
    else (List.hd l)::(sub_list (List.tl l) (length - 1))
;;

let rec play previousRounds (player1Deck, player2Deck) returnValueOnPlayer1Win returnValueOnPlayer2Win roundNumber gameNumber gamesLaunched  =
    (print_string "\nGame ");
    (print_int gameNumber);
    (print_string " Round ");
    (print_int roundNumber);
    (print_string "\n\nP1 Deck: ");
    (print_deck player1Deck);
    (print_string "P2 Deck: ");
    (print_deck player2Deck);
    if (List.length player1Deck) = 0
    then ((returnValueOnPlayer2Win (player1Deck, player2Deck)), gamesLaunched)

    else if (List.length player2Deck) = 0
    then ((returnValueOnPlayer1Win (player1Deck, player2Deck)), gamesLaunched)

    else if (Hashtbl.mem previousRounds (player1Deck, player2Deck))
    then ((returnValueOnPlayer1Win (player1Deck, player2Deck)), gamesLaunched)

    else (
        (Hashtbl.add previousRounds (player1Deck, player2Deck) 1);
        let player1CardPlayed = (card_played player1Deck) in
        let player2CardPlayed = (card_played player2Deck) in
        (print_string "P1 plays: ");
        (print_int player1CardPlayed);
        (print_string "\nP2 plays: ");
        (print_int player2CardPlayed);
        (print_string "\n");

        let remainingPlayer1DeckAfterPlay = (List.tl player1Deck) in
        let remainingPlayer2DeckAfterPlay = (List.tl player2Deck) in

        let decks_after_player_one_wins () = 
            ((remainingPlayer1DeckAfterPlay @ [player1CardPlayed; player2CardPlayed]), remainingPlayer2DeckAfterPlay)
        in

        let decks_after_player_two_wins () = 
            (remainingPlayer1DeckAfterPlay, (remainingPlayer2DeckAfterPlay @ [player2CardPlayed; player1CardPlayed]))
        in

        let next_play decks playerString gamesLaunchedBySubGame = 
            (print_string "Player ");
            (print_string playerString);
            (print_string " wins round ");
            (print_int roundNumber);
            (print_string " of game ");
            (print_int gameNumber); 
            (print_string "\n");
            (play previousRounds decks returnValueOnPlayer1Win returnValueOnPlayer2Win (roundNumber + 1) gameNumber (gamesLaunched + gamesLaunchedBySubGame))
        in

        let next_play_after_player_one_wins () gamesLaunchedBySubGame = 
            (next_play (decks_after_player_one_wins ()) "one" gamesLaunchedBySubGame)
        in

        let next_play_after_player_two_wins () gamesLaunchedBySubGame = 
            (next_play (decks_after_player_two_wins ()) "two" gamesLaunchedBySubGame)
        in
        
        if (player1CardPlayed > (List.length remainingPlayer1DeckAfterPlay)) || (player2CardPlayed > (List.length remainingPlayer2DeckAfterPlay))
        then 
            if player1CardPlayed > player2CardPlayed
            then (next_play_after_player_one_wins () 0)
            else (next_play_after_player_two_wins () 0)
        else 
            ((print_string "Launch sub game\n");
            let (subgameResult, gamesLaunchedBySubGame) = 
                (play (init_previous_rounds ()) ((sub_list remainingPlayer1DeckAfterPlay player1CardPlayed), (sub_list remainingPlayer2DeckAfterPlay player2CardPlayed)) (function decks -> [1]) (function decks -> [2]) 1 (gamesLaunched + 2) 0) in
            if (List.hd subgameResult) = 1
            then (next_play_after_player_one_wins () (1 + gamesLaunchedBySubGame))
            else (next_play_after_player_two_wins () (1 + gamesLaunchedBySubGame))
            )
    )
;;

let (winningDeck, gamesLaunched) = 
    (play 
        (init_previous_rounds ()) 
        (player1Deck, player2Deck) 
        (function (p1Deck, p2Deck) -> p1Deck) 
        (function (p1Deck, p2Deck) -> p2Deck)
        1
        1
        0
    )
;;

(print_int (List.fold_left (+) 0 (List.mapi (function index -> function cardNumber -> (index + 1) * cardNumber) (List.rev winningDeck))));;