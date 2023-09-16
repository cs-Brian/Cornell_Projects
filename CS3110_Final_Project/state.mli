(** A module representing a monopoly game. *)

(* Note that methods of the state module take in some argument(s) 
   (often one of which is of type player) and updates the state imperatively,
   rather than returning a new state. *)

(** [state] is the state of the monopoly game. *)
type state = {
  mutable players : Player.player list ; 
  mutable board : Board.boardLinkedList ;
  mutable player_turn : Player.player option ;
  mutable player_roll : int ;
  mutable number_of_dice : int ;
  mutable number_of_sides : int ;
}

(** [game_state] is the initial game state. *)
val game_state : state

(** [is_game_over players] returns whether or not all but one of the players in
    the list of [players] have gone bankrupt. If this is the case, then the game 
    is over and the player that is not bankrupt is the winner. *)
val is_game_over : state -> bool

(** [winner players] is the player that has won the monopoly game, i.e. the 
    player in the list of [players] that has not gone bankrupt. *)
val winner : state -> Player.player