open Cards
open Board

let start_money = 1500
let initial_num_turns = 1
let pass_go_increment = 200

(* ------------------------------- TYPES ---------------------------------- *)

type player = {name: string ; 
               mutable money : int ;
               mutable position : tile ;
               mutable passed_go : bool ;
               mutable properties : tile list ;
               mutable railroads : tile list ;
               mutable utilities : tile list ;
               mutable cards : card list ;
               mutable number_of_turns : int ;
               mutable has_withdrawn : bool ; 
               mutable is_in_jail : bool ;
               mutable turns_in_jail : int } 

(* ------------------------------ METHODS --------------------------------- *)

let initialize_players name_list = 
  List.map 
    (fun n -> {
         name = n ; 
         money = start_money ;
         position = go ;
         passed_go = false ;
         properties = [] ;
         railroads = [] ;
         utilities = [] ;
         cards = [] ;
         number_of_turns = initial_num_turns ;
         has_withdrawn = false ; 
         is_in_jail = false ;
         turns_in_jail = 0
       }) 
    name_list

let rec next_tile tile spaces player = 
  match spaces with 
  | 0 -> tile
  | n -> 
    begin 
      match tile.next with 
      | None ->
        player.money <- player.money + pass_go_increment; 
        player.passed_go <- true;
        print_string 
          ("You passed Go and collected $" ^ string_of_int pass_go_increment ^
           "!\n\n");
        next_tile go (n - 1) player
      | Some t ->
        next_tile t (n - 1) player
    end 

let is_bankrupt player = player.money < 0

(** [clear tiles] resets the fields of the given [tiles] to default values. *)
let rec clear tiles = 
  match tiles with 
  | [] -> ()
  | h :: t -> 
    h.owner <- None ; h.is_mortgaged <- false ;
    begin 
      match h.classification with 
      | Property p -> p.num_houses <- 0 ; p.num_hotels <- 0
      | _ -> () 
    end ;
    clear t

(** [return_card cs b] returns player cards [cs] to board [b]'s deck *)
let rec return_cards cs b =
  match cs with 
  | [] -> ()
  | h :: t -> 
    begin
      match h.cardType with 
      | Chance -> b.chance_deck <- h :: b.chance_deck
      | CommunityChest -> b.community_chest_deck <- h :: b.community_chest_deck
    end ;
    return_cards t b

let withdraw p board = 
  p.has_withdrawn <- true ; p.money <- ~-1 ; p.number_of_turns <- ~-1 ;
  clear p.properties ; clear p.utilities ; clear p.railroads ; 
  return_cards p.cards board ;
  p.cards <- [] ;
  print_string ("\n" ^ p.name ^ " has withdrawn from the game!\n")