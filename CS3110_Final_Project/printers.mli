(** Functions for stringify-ing and printing out the game's metadata. *)

(** [print_player_turn_notification p] notifies player [p] of their turn. *)
val print_player_turn_notification : Player.player -> unit

(** [string_of_int_list lst] is a string representation of int list [lst]. *)
val string_of_int_list : int list -> string

(** [prompt_view_properties st] prompts any player to view free and available
    properties in the game with state [st], as well as select information about
    those properties. *)
val prompt_metadata_print : Player.player -> State.state -> unit
(** Functions for stringify-ing and printing out the game's metadata. *)

(** [print_player_turn_notification p] notifies player [p] of their turn. *)
val print_player_turn_notification : Player.player -> unit

(** [string_of_int_list lst] is a string representation of int list [lst]. *)
val string_of_int_list : int list -> string

(** [prompt_view_properties st] prompts any player to view free and available
    properties in the game with state [st], as well as select information about
    those properties. *)
val prompt_metadata_print : Player.player -> State.state -> unit

(** [prompt_jail_escape pl st] lets the player know that they are in jail, and
    gives them the appropriate options that a player in jail has. *)
val prompt_jail_escape : Player.player -> State.state -> unit
