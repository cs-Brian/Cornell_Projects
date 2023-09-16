(** A module representing the gameboard of the original monopoly. *)

(** All of the colors that a property value could possibly be. *)
type propertyColor = 
  | BROWN | LIGHTBLUE | MAGENTA | ORANGE | RED 
  | YELLOW | GREEN | DARKBLUE

(** A property type to be used on tiles with classification Property. *)
type property = {
  color : propertyColor ;
  rent_prices : (int * int) list ;
  price_of_house : int ;
  price_of_hotel : int ;
  mutable num_houses: int ;
  mutable num_hotels: int ;
  name : string ;
}

(** [classification] is the type of a tile on a game_board *)
type classification = 
  | Go | Free | Jail | To_Jail | Tax | Railroad | Utility 
  | Chance | Chest | Property of property

(** [tile] is the representaton of any tile on the board where 
      [index] is the position of the tile in the board,
      [name] is the tile's name on the board
      [classification] is the category that the tile falls into
      [owner] is the owner of the property None if no one owns it,
      [amount] is the amount of money the player could pay for a given tile,
      [next] is the next tile that the current tile will point to
    Requires:
      [index] to be between 0 and 39,
      [classification] to be either "go", "free", "jail", "to_jail", "tax", 
      "railroad", "utility", "chance", "chest", "tax", "brown", "lightblue", 
      "magenta", "orange", "red", "yellow", "green", or "blue"
      [next] optinally points to another tile or nothing
*)
type tile = {
  index : int ;
  name : string ;
  classification : classification ;
  mutable owner : string option ;
  cost : int ;
  mutable next : tile option ;
  mortgage : int ;
  mutable is_mortgaged : bool ;
}

(** [boardLinkedList] is the board that is a linked-list consisting of tiles. *)
type boardLinkedList = {
  head : tile ;
  mutable chance_deck : Cards.card list ;
  mutable community_chest_deck : Cards.card list ;
}

(* Tiles *)
(** [tile_names] is the names of all of the tiles in order of the board. *)
val tile_names : string array

(** [get_tile_by_name name tile] traverses through the board starting at [tile]
    and returns the tile found that has name [name].*)
val get_tile_by_name : string -> tile option -> tile

(** [name_of_tile tile] is the name of tile [tile]. *)
val name_of_tile : tile -> string

val darkblue2 : tile
val tax2 : tile
val darkblue1 : tile
val chance3  : tile
val railroad4 : tile
val green3 : tile
val chest3 : tile
val green1 : tile
val to_jail  : tile
val yellow3  : tile
val utility2 : tile
val yellow2 : tile
val yellow1 : tile
val railroad3 : tile
val red3 : tile
val red2 : tile
val chance2 : tile
val red1 : tile
val free_parking : tile
val orange3 : tile
val orange2 : tile
val chest2 : tile
val orange1 : tile
val railroad2 : tile
val magenta3 : tile
val magenta2 : tile
val utility1 : tile
val magenta1 : tile
val jail : tile
val lightblue3 : tile
val lightblue2 : tile
val chance1 : tile
val lightblue1 : tile
val railroad1 : tile
val tax1 : tile
val brown2 : tile
val chest1 : tile
val brown1 : tile
val go : tile

val prop_darkblue1 : property
val prop_darkblue2 : property
val prop_green1 : property
val prop_green2 : property
val prop_green3 : property
val prop_yellow1 : property
val prop_yellow2 : property
val prop_yellow3 : property
val prop_red1 : property
val prop_red2 : property
val prop_red3 : property
val prop_orange1 : property
val prop_orange2 : property
val prop_orange3 : property
val prop_magenta1 : property
val prop_magenta2 : property
val prop_magenta3 : property
val prop_lightblue1 : property
val prop_lightblue2 : property
val prop_lightblue3 : property
val prop_brown1 : property
val prop_brown2 : property

(** [board] is the gameboard in the original monopoly. *)
val board : boardLinkedList