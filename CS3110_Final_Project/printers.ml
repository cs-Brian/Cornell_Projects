open Player
open Actions
open State
open Board
open Cards
open Errors

let valid_yes_or_no_responses = 
  [
    "YES" ; "YEs" ; "Yes" ; "yes" ; "Y" ; "y" ;
    "NO" ; "No" ; "no" ; "N" ; "n"
  ]

let valid_view_responses = 
  [
    "r" ; "R" ; "a" ; "A" ; "o" ; "O" ; "s" ; "S" ;
    "g" ; "G" ; "m" ; "M" ; "h" ; "H" ; "b" ; "B";
    "w" ; "W" ; "q" ; "Q" ; "l" ; "L" ;
  ]

let valid_jail_responses = "e" :: "E" :: "c" :: "C" :: valid_view_responses

let metadata_options = 
  "\nEnter ['r' : roll, " ^ 
  "'b' : build houses/hotels, " ^ 
  "'h' : sell houses/hotels, " ^ 
  "'m' : mortgage, " ^
  "'a' : available properties, " ^ 
  "'o' : owned properties, " ^ 
  "'s' : stats, " ^ 
  "'g' : gameboard, " ^ 
  "'w' : withdraw, " ^ 
  "'q' : quit]\n"

let exit_message = "\nQuitting the game... Thanks for playing Monopoly!\n\n"

let print_player_turn_notification (p : Player.player) =
  print_string ("\n" ^ p.name ^ "'s" ^ " turn:\n")

(** [yes_or_no resp] is "y" if [resp] is "yes" (or some variant of yes, 
    according to the valid 'yes or no' responses). "n" otherwise. *)
let yes_or_no resp = 
  if (String.contains resp 'y' || String.contains resp 'Y') then "y" else "n"

(* -------------------- DEBUGGING PRINTERS FOR PLAY-TESTING ----------------- *)

(** [string_of_int_list_aux lst] is the helper function to 
    [string_of_int_list lst] that recurses through the [lst] and converts it to 
    a string. *)
let rec string_of_int_list_aux = function 
  | [] -> ""
  | h :: [] -> string_of_int h
  | h :: t -> string_of_int h ^ "; " ^ string_of_int_list_aux t 

let string_of_int_list lst = 
  "[" ^ string_of_int_list_aux lst ^ "]"

(* ------------------------ PLAYER STATS PRINTER ---------------------------- *)

(** [string_of_player_money p] is a string representation of a player's money
    amount [m]. *)
let string_of_player_money m =
  string_of_int m

(** [get_property_color prop] is the color associated with property [prop], as
    a string. *)
let string_of_property_color prop = 
  match prop.color with 
  | BROWN -> "Brown"
  | LIGHTBLUE -> "Light Blue"
  | MAGENTA -> "Magenta"
  | ORANGE -> "Orange"
  | RED -> "Red"
  | YELLOW -> "Yellow"
  | GREEN -> "Green"
  | DARKBLUE -> "Dark blue"

(** [string_of_player_tile t] is a string representation of board tile [t], 
    which includes the name of the owner of the tile if [owner_requested] is 
    true, but does not include it otherwise. *)
let string_of_player_tile t owner_requested = 
  let owner = match t.owner with 
    | None -> "no one"
    | Some o -> o in 
  match t.classification with 
  | Property p -> if owner_requested then 
      t.name ^ ", owned by " ^ owner  ^ 
      " (" ^ string_of_property_color p ^ ", " ^ string_of_int p.num_houses ^ 
      " houses, " ^ string_of_int p.num_hotels ^ " hotels)" 
    else 
      t.name ^ " (" ^ string_of_property_color p ^ ", " ^ 
      string_of_int p.num_houses ^ " houses, " ^ string_of_int p.num_hotels ^ 
      " hotels)"
  | _ -> if owner_requested then t.name ^ ", owned by " ^ owner else t.name

(** [string_of_board_items items] is a string representation of a player's list
    owned board locations [items]. *)
let rec string_of_board_items items = 
  match items with 
  | [] -> ""
  | h :: [] -> string_of_player_tile h false
  | h :: t -> string_of_player_tile h false ^ ", " ^ string_of_board_items t

(** [string_of_card c] is a string representation of card [c]. *)
let string_of_card c = 
  let c_type = match c.cardType with 
    | CommunityChest -> "Community Chest"
    | Chance -> "Chance" in 
  c_type ^ " -> " ^ c.text

(** [string_of_cards cards] is a string representation of the list of cards 
    [cards]. *)
let rec string_of_cards cards = 
  match cards with 
  | [] -> ""
  | h :: [] -> string_of_card h
  | h :: t -> string_of_card h ^ ", " ^ string_of_cards t

(** [string_of_player p] is a string representation of player [p]'s stats. *)
let string_of_player p =
  "\n" ^
  "Cash: $" ^ string_of_player_money p.money ^ "\n" ^
  "Properties: " ^ string_of_board_items p.properties ^ "\n" ^ 
  "Railroads: " ^ string_of_board_items p.railroads ^ "\n" ^ 
  "Utilities: " ^ string_of_board_items p.utilities ^ "\n" ^ 
  "Cards: " ^ string_of_cards p.cards ^ "\n"

let prompt_player_print p = 
  print_string 
    "\nWould you like to print out your stats? [y/n]\n";
  let response = 
    read_line () |> validate_response valid_yes_or_no_responses in 
  let y_or_n = yes_or_no response in 
  match y_or_n with 
  | "n" -> ()
  | "y" -> print_string (string_of_player p)
  | _ -> failwith "Response must have been yes or no. Impossible to get here."

(* -------------------------- PROPERTIES PRINTER ---------------------------- *)

(** [string_of_available_property t] is the string representation of the 
    property at tile t. Empty string if the tile is owned, or its classification 
    is not of Railroad nor Utility nor Property. *)
let string_of_available_property t =
  match t.classification with 
  | Railroad -> 
    if t.owner = None then 
      t.name ^ " ~ price: " ^ string_of_int t.cost else ""
  | Utility -> 
    if t.owner = None then 
      t.name ^ " ~ price: " ^ string_of_int t.cost else ""
  | Property p -> 
    if t.owner = None then 
      t.name ^ 
      " ~ price: $" ^ string_of_int t.cost ^ 
      ", rent cost: $" ^ string_of_int (List.assoc 0 p.rent_prices)
    else ""
  | _ -> ""

(** [string_of_available_properties t acc] is a string representation of all of 
    the available properties starting at tile [t], starting with accumulator 
    string acc. *)
let rec string_of_available_properties t acc = 
  match t.next with 
  | None -> string_of_available_property t ^ acc
  | Some tile -> 
    let available_prop = string_of_available_property t in 
    if available_prop <> "" then 
      available_prop ^ "\n" ^ 
      string_of_available_properties tile acc 
    else available_prop ^ string_of_available_properties tile acc

(** [string_of_owned_property t] is a string representation of tile [t], or the
    empty string if [t] is owned or does not have classification Property, 
    Railroad, or Utility. *)
let string_of_owned_property t = 
  match t.owner with 
  | None -> ""
  | Some o -> 
    match t.classification with 
    | Property p -> 
      t.name ^ " ~ Owner: " ^ o ^ ", current rent cost: $" ^ 
      string_of_int (rent_of_property p)
    | Railroad | Utility -> t.name ^ " ~ Owner: " ^ o
    | _ -> ""

(** [string_of_owned_properties t acc] is a string representation of all of 
    the owned properties starting at tile [t], starting with accumulator 
    string acc. *)
let rec string_of_owned_properties t acc =
  match t.next with 
  | None -> string_of_owned_property t ^ acc 
  | Some tile -> 
    let owned_prop = string_of_owned_property t in 
    if owned_prop <> "" then 
      owned_prop ^ "\n" ^ string_of_owned_properties tile acc 
    else owned_prop ^ string_of_owned_properties tile acc 

(* ------------------------- BUILD HOUSE/HOTEL PRINTER ---------------------- *)

let prompt_building p = take_build_action p

(* -------------------------- SELL HOUSE/HOTEL PRINTER ---------------------- *)

(** [prompt_selling p] prompts player [p] to sell a house or hotel back to the
    bank. *)
let prompt_selling p = 
  if List.length (sell_colors p) > 0 then 
    begin print_string 
        "\nWould you like to sell a house or hotel back to the bank?\n";
      let response = 
        read_line () |> validate_response valid_yes_or_no_responses in         
      let y_or_n = yes_or_no response in 
      match y_or_n with 
      | "n" -> ()
      | "y" -> sell_buildings p
      | _ -> failwith 
               "Response must have been yes or no. Impossible to get here."
    end
  else print_string ("\nYou do not have any buildings to sell!\n")

(* -------------------------- GAME STATS PRINTER ---------------------------- *)

(** [string_of_rent_prices rps] is a string representation of the association
    list [rps] that represents rent prices. Keys are the number of properties,
    values are the prices. *)
let rec string_of_rent_prices rps = 
  match rps with 
  | [] -> ""
  | h :: t -> 
    let num_buildings = fst h in let price = snd h in 
    if num_buildings < 5 then 
      if num_buildings <> 1 then 
        begin
          string_of_int num_buildings ^ " houses costs $" ^ string_of_int price 
          ^ ", " ^ string_of_rent_prices t
        end
      else string_of_int num_buildings ^ " house costs $" ^ string_of_int price 
           ^ ", " ^ string_of_rent_prices t
    else
      "one hotel costs $" ^ string_of_int price 
      ^ ", " ^ string_of_rent_prices t

(** [string_of_property_data prop] is the string representation of the data of
    property [prop]. *)
let string_of_property_data prop = 
  "Color: " ^ string_of_property_color prop ^ "\n" ^
  "Price of house: $" ^ string_of_int prop.price_of_house ^ "\n" ^
  "Price of hotel: $" ^ string_of_int prop.price_of_hotel ^ "\n" ^ 
  "Number of houses: " ^ string_of_int prop.num_houses ^ "\n" ^
  "Number of hotels: " ^ string_of_int prop.num_hotels ^ "\n" ^
  "Rent Prices: " ^ string_of_rent_prices prop.rent_prices ^ "\n"

(** [get_players_on_tile players tile] is the list of players (their names) in
    [players] that are currently on tile [tile], as a string. *)
let rec get_players_on_tile players tile = 
  match players with 
  | [] -> ""
  | h :: [] -> if h.position = tile then h.name else ""
  | h :: t -> 
    if h.position = tile then h.name ^ ", " ^ get_players_on_tile t tile else
      get_players_on_tile t tile

(** [string_of_gameboard_tile ps t] is a string represenation of tile [t], meant 
    for printing out the entire state of the game with players [ps], rather than 
    for an individual player. *)
let string_of_gameboard_tile ps t = 
  let owner = match t.owner with 
    | None -> "no one"
    | Some o -> o in 
  let tile_string = t.name ^ "\n" ^ "Owned by: " ^ owner in 
  let with_prop_data = match t.classification with 
    | Property p -> 
      "\n" ^ tile_string ^ "\n" ^ 
      string_of_property_data p ^
      "Mortgage: $" ^ string_of_int t.mortgage ^ "\n"
    | _ -> tile_string ^ "Mortgage: $" ^ string_of_int t.mortgage ^ "\n" in 
  with_prop_data ^ get_players_on_tile ps t

(** [string_of_gameboard_aux tile acc] is a string representation of a game 
    board starting at tile [tile] and with players [ps], with a starting 
    acculumator string [acc]. *)
let rec string_of_gameboard ps tile acc = 
  match tile.next with 
  | None -> string_of_gameboard_tile ps tile ^ acc 
  | Some t -> 
    string_of_gameboard_tile ps tile ^ string_of_gameboard ps t acc

let prompt_gameboard_print st = 
  print_string "\nWould you like to see the game board at this point? [y/n]\n" ;
  let response = 
    read_line () |> validate_response valid_yes_or_no_responses in 
  let y_or_n = yes_or_no response in 
  match y_or_n with 
  | "n" -> ()
  | "y" -> print_string (string_of_gameboard st.players (st.board.head) "")
  | _ -> failwith "Response must have been yes or no. Impossible to get here."

(* ------------------------------ MAIN PROMPT ------------------------------- *)

let show_owned_props st = 
  let owned_props = string_of_owned_properties st.board.head "" in 
  if owned_props = "" then 
    print_string "\nNo properties are currently owned.\n" 
  else print_string ("\n" ^ owned_props ^ "\n") 

let show_available_props st = 
  let available_props = string_of_available_properties st.board.head "" in 
  if available_props = "" then 
    print_string "\nNo properties are currently available.\n" 
  else print_string ("\n" ^ available_props ^ "\n")

let rec prompt_metadata_print p st = 
  print_string metadata_options ;
  match read_line () |> validate_response valid_view_responses with 
  | "r" | "R" -> print_string "\n" ;
  | "w" | "W" -> withdraw p st.board ;
  | "q" | "Q" -> print_string exit_message ; exit 0
  | "a" | "A" -> show_available_props st ; prompt_metadata_print p st
  | "b" | "B" -> prompt_building p ; prompt_metadata_print p st
  | "h" | "H" -> prompt_selling p ; prompt_metadata_print p st
  | "m" | "M" -> mortgage_prompt p ; prompt_metadata_print p st
  | "l" | "L" -> lift_mortgage_prompt p; prompt_metadata_print p st 
  | "o" | "O" -> show_owned_props st ; prompt_metadata_print p st
  | "s" | "S" -> print_string (string_of_player p) ; prompt_metadata_print p st
  | "g" | "G" -> 
    print_string (string_of_gameboard st.players (st.board.head) "") ;
    prompt_metadata_print p st
  | _ -> failwith "Player must have entered a valid response."

(** [player_pay_out pl] gives the option to let a player pay out of jail. *)
let player_pay_out pl = 
  print_string ("\nDo you want to pay $50 get out of jail? [y/n]\n");
  let response = 
    read_line () |> validate_response valid_yes_or_no_responses in 
  let y_or_n = yes_or_no response in 
  match y_or_n with 
  | "n" -> false 
  | "y" -> 
    begin 
      if pl.money >= 50 then 
        begin 
          pl.money <- pl.money - 50; 
          pl.is_in_jail <- false ; pl.turns_in_jail <- 0; true
        end
      else (print_string ("You do not have enough money"); false)
    end 
  | _ -> false  

(* [use_out_of_jail_cards pl] allows the player to use their get out of jail
   free card if they have cards in their stack. *)
let use_out_of_jail_cards pl = if List.length pl.cards <> 0
  then 
    begin 
      print_string ("\nYou have a get out jail free card. Would you like " ^
                    "to use it? [y/n]\n");
      let response = 
        read_line () |> validate_response valid_yes_or_no_responses in 
      let y_or_n = yes_or_no response in 
      match y_or_n with 
      | "n" -> print_string "\nYou did not use your card.\n"; false 
      | "y" -> 
        begin 
          pl.cards <- List.tl pl.cards; 
          pl.is_in_jail <- false; pl.turns_in_jail <- 0 ; true
        end
      | _ -> false 
    end
  else false

let prompt_jail_escape (pl : Player.player) st = 
  let used_card = use_out_of_jail_cards pl in 
  let pay_out = if used_card then false else player_pay_out pl in 
  if used_card || pay_out 
  then print_string "\nYou're no longer in jail!\n" else ();
  prompt_metadata_print pl st
