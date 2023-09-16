(** Monopoly is a script-based application. 
    The main file runs the script, which includes giving players prompts 
    and receiving and processing player input. *)

(** [player_turn player state num_dice num_sides] performs a single turn of 
    [player] based on the [state] and the dice roll using [num_dice] and 
    [num_sides], and updates the player and state accordingly. A player's turn 
    consists of moving the player to the next tile based on their dice roll, 
    prompts the player to take an action based on the tile they land on, and 
    based on the chosen action, updates the player and the state of the game. *) 
val player_turn : Player.player -> State.state -> unit

(** [take_player_turns players state num_dice num_sides] performs the turn of
    each player in the player list [players] one at a time, based on the [state]
    of the game, the number of dice [num_dice], and the number of sides on each 
    die num_sides. *)
val take_player_turns : Player.player list -> State.state -> unit

(** [read_multiple_input n] takes in [n] inputs and puts them into a list, and
    returns that list. *)
val read_multiple_input : int -> string list

(** [read_multiple_names_input n] takes in [n] inputs (each input being a 
    player name) and returns that list of [n] player names. It also ensures that 
    player names are valid and that there are no duplicate names. *)
val read_multiple_names_input : int -> string list

(** [initialize_game] initializes the game and returns the corresponding game
    state record. *)
val initialize_game : unit -> State.state

(** [run] is the main function that runs the game. *)
val run : unit -> unit