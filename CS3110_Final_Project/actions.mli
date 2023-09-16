(* A module representing a player's actions for a given turn. *)

(** A module representing the prompting and execution of actions that a single
    player can take on their turn, based on the current state of the game(board) 
    and the tile that the player is on. *)

(** [move player spaces] updates the [player]'s position by moving them [spaces]
    spaces. *)
val move : Player.player -> int -> unit

(** [take_action player state] prompts the [player] to take an action on a turn,
    based on the current [state] of the game. *)
val take_action : Player.player -> State.state -> unit

(** [can_build player] based on the [player] the function returns true if they
    are able to build on their properties and false otherwise. *)
val can_build : Player.player -> bool

(** [take_build_action player] lets a [player] build if they can or
    prints a string saying they are unable to build. *)
val take_build_action : Player.player -> unit

(** [tiles_to_props tile] takes in [tile] and returns it as a property. *)
val tiles_to_props : Board.tile -> Board.property

(** [get_finished_colorgroups prop_assoc_list build_colors] takes in an 
    association list [prop_assoc_list] with keys being Board.propertyColor 
    and values being the number of propeties owned of that color and returns a 
    Board.propertyColor list [build_colors] with colors the player can build 
    on. *)
val get_finished_colorgroups : (Board.propertyColor * int) list -> 
  Board.propertyColor list -> Board.propertyColor list

(** [props_assoc properties props] takes in a player's [properties] and creates
    an association list [props] with the Board.propertyColor as a key and the 
    number of properties of a given color as the value. *)
val props_assoc : Board.property list ->
  (Board.propertyColor * int) list -> (Board.propertyColor * int) list

(** [check_hh color player] checks if [player] is eligable to build a house 
    or hotel on properties of color [color]. *)
val check_hh : Board.propertyColor -> Player.player -> string

(** [get_props player] returns a list of properties a [player] can build on
    after checking build rules. *)
val get_props : Player.player -> (Board.propertyColor * string) list

(** [sell_colors player] returns a list of colors a [player] can sell
    buildings on on*)
val sell_colors : Player.player -> Board.propertyColor list

(** [sell_buildings player] lets [player] sell buildings on their properties*)
val sell_buildings : Player.player -> unit

(** [rent_of_property prop] calculates the rent of a property [prop] based
    on the number of houses or hotels on it. *)
val rent_of_property : Board.property -> int

(** [mortgage_prompt player] is the prompt that shows up when a player inputs
    that they want to mortgage a property. If the property is able to 
    be mortgaged then it wil be, otherwise they must try a different action. *)
val mortgage_prompt : Player.player -> unit

(** [lift_mortgage_prompt player] is the prompt that shows up when a player
    inputs that they want to lift the mortgage on a property. If the mortgage 
    can be lifted then it is and the player pays, otherwise they must try
    a different action. *)
val lift_mortgage_prompt : Player.player -> unit

(** [has_card] is whether or not the card with text [c_text] is in the card list
    [c_list]. *)
val has_card : Cards.card list -> string -> bool

(** [send_to_jail p] sends player p to jail. *)
val send_to_jail : Player.player -> unit

