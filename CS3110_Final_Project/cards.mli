(** A module representing the possible cards in monopoly. *)

(** [cardType] represents the type of card. *)
type cardType = CommunityChest | Chance

(** The type [action] representing the action a player should take after
    picking up a card.
    COLLECT: How much a player collects or pays cash
    MOVE: A location to which a player moves 
    KEEP: A card that a player keeps, ex. go to jail
    ALL: The player collects from or pays everyone
    TAXES: The player has to pay a certaina amount for each house and hotel. *)
type action = 
  | Collect of int 
  | Move of int
  | Keep
  | All of int
  | Taxes of (int * int)

(** [card] represents a monopoly card. *)
type card = { 
  cardType : cardType; 
  text : string; 
  action : action
}

(** [community_chest_deck] is the deck of community chest cards. *)
val community_chest_deck : card list 

(** [cahnce_deck] is the deck of chance cards. *)
val chance_deck : card list