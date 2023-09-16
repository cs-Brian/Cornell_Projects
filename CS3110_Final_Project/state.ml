open Player
open Board 
open Cards

(* ------------------------------- TYPES ---------------------------------- *)

type state = {
  mutable players : player list; 
  mutable board : boardLinkedList;
  mutable player_turn : player option;
  mutable player_roll : int;
  mutable number_of_dice : int ;
  mutable number_of_sides : int ;
}

let game_state = {
  players = [] ; 
  board = board ; 
  player_turn = None ; 
  player_roll = 0 ;
  number_of_dice = 0 ;
  number_of_sides = 0 ; 
}

(* ------------------------------ METHODS --------------------------------- *)

let is_game_over state = 
  let non_bankrupt_list = 
    List.filter (fun p -> is_bankrupt p = false) state.players 
  in List.length non_bankrupt_list = 1

(** [winner_aux players] is a helper function to [winner] that takes in a list
    of [players] and returns the winner of the game. *)
let rec winner_aux players = 
  match players with 
  | [] -> failwith "There must be at least one player in the game"
  | h :: t -> if (is_bankrupt h = false) then h else winner_aux t

let winner state = 
  winner_aux state.players