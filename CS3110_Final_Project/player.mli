(** A module representing a monopoly player. *)

(* Note that methods of the player module take in some argument(s) 
   (often one of which is of type player) and updates the player imperatively,
   rather than returning a new player. *)

(** [player] represents a monopoly player. *)
type player = {
  name : string ; 
  mutable money : int ;
  mutable position : Board.tile ;
  mutable passed_go : bool ;
  mutable properties : Board.tile list ;
  mutable railroads : Board.tile list ;
  mutable utilities : Board.tile list ;
  mutable cards : Cards.card list ;
  mutable number_of_turns : int ;
  mutable has_withdrawn : bool ;
  mutable is_in_jail : bool ;
  mutable turns_in_jail : int
}

(** [next_tile tile spaces p] is the next tile a player will land on after [tile] 
    when moving [spaces] spaces. If the player [p] is going to land on the
    go tile then they receive additional money. *)
val next_tile : Board.tile -> int -> player -> Board.tile

(** [initialize_players name_list] creates a list of new player structs with 
    default values and names in [name_list]. *)
val initialize_players : string list -> player list

(** [is_bankrupt player] is whether or not the given [player] has gone 
    bankrupt, i.e. whether or not their money takes on a negative value. *)
val is_bankrupt : player -> bool

(** [withdraw player board] withdraws player [p] from the game with board 
    [board]. *)
val withdraw : player -> Board.boardLinkedList -> unit