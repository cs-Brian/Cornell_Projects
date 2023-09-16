open Player
open State
open Board
open Cards
open Dice
open Errors

let valid_yes_no = ["YES"; "Yes"; "yes"; "Y"; "y"; "NO"; "No"; "no"; "N"; "n"]

(** [find_player name p_list] is the player record corresponding to the player
    with name [name]. *)
let rec find_player name (p_list : player list) =
  match p_list with 
  | [] -> failwith "Player list must not be empty"
  | h :: t -> if h.name = name then h else find_player name t

(** [add_money p p_list amount] adds [amount] to the money of the player in 
    player list [p_list] with the name p_name. *)
let rec add_money p_name (p_list : player list) amount = 
  match p_list with 
  | [] -> ()
  | h :: t -> if p_name = h.name then h.money <- h.money + amount 
    else add_money p_name t amount

(** [name_is name p] is whether or not the name of player [p] is [name]. *)
let name_is (name : string) (p : player) = name = p.name

(** [find_player_by_name name] is the player value with name [name]. *)
let find_player_by_name name state = 
  List.find (name_is name) state.players  

let move player spaces = 
  player.position <- next_tile player.position spaces player

(** [add_asset p asset] adds asset [asset] to player [p]'s collection based
    on [asset]'s classification. 
    Requires: 
      [asset] either has classification Railroad, Utility, or Property. *)
let add_asset p asset = 
  match asset.classification with 
  | Railroad -> p.railroads <- asset :: p.railroads
  | Utility -> p.utilities <- asset :: p.utilities
  | _ -> p.properties <- asset :: p.properties

(** [purchase player asset] updates the given [player] after the purchase the 
    given [asset]. *)
let purchase p asset =
  if p.passed_go then begin
    if p.money - asset.cost < 0 then 
      print_string "This property is not owned by anyone else, 
    but you do not have enough money to buy it.\n" 
    else
      begin 
        print_string 
          ("\nLooks like " ^ asset.name ^ 
           " isn't owned! Would you like to buy it for $" 
           ^ string_of_int asset.cost ^ "?[y/n]\n");
        let player_input = read_line () |> validate_response valid_yes_no in
        match player_input with 
        | "YES" | "Yes" | "yes" | "Y" | "y" ->  
          begin 
            add_asset p asset;
            p.money <- p.money - asset.cost;
            asset.owner <- Some p.name ;
            print_string ("\nYou now own " ^ asset.name ^ ".\n") 
          end
        | "NO" | "No" | "no" | "N" | "n" -> 
          print_string ("\nYou chose not to buy " ^ asset.name ^ ".\n")
        | _ -> ()
      end
  end 
  else print_string(
      "\nYou need to completely go around the board once " ^
      "before you can purchase properties! \n") 

(** [pay_taxes p tile] makes a player [p] pay a tax 
    of the [tile]'s amount. *)
let pay_taxes p tile = 
  print_string ("\nPaid " ^ string_of_int (~-1 * tile.cost) ^ " in taxes.\n");
  p.money <- p.money + tile.cost

(** [exchange_money p1 p2 amt] removes amount [amt] from [p1]'s money and puts
    it into [p2]'s bank account. *)
let exchange_money (p1 : player) (p2 : player) amt = 
  p1.money <- p1.money - amt;
  p2.money <- p2.money + amt;
  if ((p1.name <> p2.name) && (amt <> 0)) then print_string 
      ("\n" ^ p1.name ^ " pays " ^ p2.name ^ " $" ^ string_of_int amt ^ ".\n")

(** [land_on_railroad p tile from_card] makes the player [p] pay the owner of
    [tile] if it is owned. Otherwise, the player has the option to buy [tile].  
    Requires:
      [tile] must have a classification Railroad. *)
let land_on_railroad p tile state from_card = 
  match tile.owner with 
  | None -> purchase p tile
  | Some n -> begin
      let owner = find_player_by_name n state in
      let railroad_price = 
        50 * int_of_float (2. ** float_of_int (List.length owner.railroads)) in
      let factor = if from_card then 2 else 1 in
      if tile.is_mortgaged 
      then print_string
          ("\nIt's your lucky day! This railroad is owned already " ^ 
           "but it's mortgaged so you don't have to pay anything!\n") 
      else exchange_money p owner (factor * railroad_price) 
    end  

let rent_of_property prop = 
  match prop.num_houses with 
  | 0 -> if prop.num_hotels = 1 
    then (List.assoc 5 prop.rent_prices) else (List.assoc 0 prop.rent_prices) 
  | 4 -> if prop.num_hotels = 1 
    then (List.assoc 5 prop.rent_prices) else (List.assoc 4 prop.rent_prices) 
  | n -> List.assoc n prop.rent_prices 

(** [house_and_hotel_rent owner p prop] makes player [p] pay [owner] based on 
    the number of houses and hotels that player [owner] has on [prop].
    Requires:
      The tile [tile] associated with prop has an owner whose name is 
      the same as [owner]. *)
let house_and_hotel_rent owner p prop tile = 
  let rent = rent_of_property prop in
  if tile.is_mortgaged 
  then print_string
      ("\nIt's your lucky day! This property is owned already " ^ 
       "but it's mortgaged so you don't have to pay anything!\n") 
  else exchange_money p owner rent

(** [land_on_property p tile] makes player [p] pay the owner of tile [tile]
    if the tile is owned. Otherwise, player [p] has the option to purchase
    [tile].  *)
let land_on_property p tile state = 
  match tile.owner with 
  | None -> purchase p tile
  | Some n -> begin
      let owner = find_player_by_name n state in
      match tile.classification with
      | Property prop -> house_and_hotel_rent owner p prop tile 
      | _ -> ()
    end

(**  if [tile] is owned, [land_on_utility p tile from_card] makes player [p] 
     pay the owner based on how many utilities they have. Otherwise, the player 
     has the option to purchase [tile].
     Requires:
      [tile] has classification Utility. *)
let land_on_utility p tile state from_card = 
  match tile.owner with 
  | None -> purchase p tile
  | Some n -> begin
      let owner = find_player_by_name n state in
      let amt = if (List.length owner.utilities) = 2 || from_card
        then (10 * state.player_roll) else (4 * state.player_roll) in
      if tile.is_mortgaged 
      then print_string
          ("\nIt's your lucky day! This utility is owned already " ^ 
           "but it's mortgaged so you don't have to pay anything!\n") 
      else exchange_money p owner amt
    end

(** [pay_all_players p players amt] has player [p] pay all players in [players]
    amount [amt]. *)
let rec pay_all_players p players amt = 
  match players with
  | [] -> ()
  | h :: t -> exchange_money p h amt; pay_all_players p t amt

(** [get_houses_of_prop tile] gets the number of houses on tile [tile].
    Requires:
      [tile] has classification Property. *)
let get_houses_of_prop tile = 
  match tile.classification with 
  | Property p -> p.num_houses
  | _ -> 0

(** [get_hotels_of_prop tile] gets the number of hotels on tile [tile].
    Requires:
      [tile] has classification Property. *)
let get_hotels_of_prop tile = 
  match tile.classification with
  | Property p -> p.num_hotels
  | _ -> 0

(** [pay_per_house_hotel] calculates [house_price] for each house on properties 
    [props] and [hotel_price] for each hotel on on properties [props].
    Requires:
      Every element in [props] has classification Property. *)
let rec pay_per_house_hotel props house_price hotel_price = 
  match props with 
  | [] -> 0
  | h :: t -> 
    (house_price * get_houses_of_prop h + hotel_price * get_hotels_of_prop h) + 
    pay_per_house_hotel t house_price hotel_price  

(** [add_card player card] adds card [card] to player [p]'s personal cards. *)
let add_card player card = player.cards <- card :: player.cards

(** [find_nearest_railroad pos] finds the closest railroad tile to 
    position [pos]. *)
let find_nearest_railroad pos = (55 - pos) mod 10

(** [find_nearest_utility pos] finds distance to the closest utility tile from 
    position [pos]. *)
let find_nearest_utility pos = 
  match pos with 
  | n when n < 12 -> 12 - n
  | n when n > 12 && n < 28 -> 28 - n
  | n -> (40 - n) + 12

(** [find_illinois_ave pos] is the distance from index [pos] to illinois ave. *)
let find_illinois_ave pos = if pos < 24 then 24 - pos else (40 - pos) + 24

(*----------------------CARD FUNCTIONS--------------------------------------*)

(** [has_card c_list c_text] returns whether or not a card in [c_list]'s 
    text is equal to [text]. *)
let rec has_card c_list c_text = 
  match c_list with 
  | [] -> false
  | h :: t -> if h.text = c_text then true else has_card t c_text

(** [has_cards cards deck_type] checks to see if [cards] have any cards that
    are of type [deck_type]. *)
let rec has_cards cards deck_type =
  match cards with 
  | [] -> false
  | h :: t -> (h.cardType = deck_type) || has_cards t deck_type

(** [check_player_cards' players deck] checks to see if any players have
    cards from the deck [deck]. *)
let rec check_player_cards players deck_type = match players with 
  | [] -> false 
  | h :: t -> has_cards h.cards deck_type || check_player_cards t deck_type

(** [make_new_deck deck_type state] makes a new [deck_type] deck and updates
    the state [state] so that the board's [deck_type] deck is the same. *)
let make_new_deck deck_type state = 
  let original_deck = match deck_type with 
    | CommunityChest -> community_chest_deck
    | Chance -> chance_deck in 
  let new_deck = if check_player_cards state.players deck_type then 
      List.tl original_deck else original_deck in
  new_deck 

(** [remove_card_from_deck card state] removes card [card] from the deck in 
    [state]'s game board and re-creates [state]'s board deck if the card that
    was removed was the last card in the deck. *)
let remove_card_from_deck card state  = 
  let current_deck : card list = match card.cardType with
    | CommunityChest -> state.board.community_chest_deck  
    | Chance -> state.board.chance_deck in 
  let deck_after_removal = 
    List.filter (fun card1 -> card <> card1) current_deck in
  let new_deck = if List.length deck_after_removal = 0 then 
      make_new_deck card.cardType game_state 
    else deck_after_removal in
  match card.cardType with 
  | CommunityChest -> state.board.community_chest_deck <- new_deck 
  | Chance -> state.board.chance_deck <- new_deck 

(** [choose_random_card deck] chooses a random card in the given deck
    and updates the state of the game so the card chosen isn't in the 
    deck anymore. *)
let choose_random_card deck state = 
  let chosen_card = deck |> List.length |> Random.int |> List.nth deck in
  remove_card_from_deck chosen_card state; 
  chosen_card

(** [follow_card_instructions p actions card] has player [p] implement the 
    actions in [actions] that are associated with card [card].
    Requires:
      [actions] is the same list as [card.action]. *)
let rec follow_card_instructions 
    (p : player) 
    (card : card) 
    (state : state) : unit =
  print_string (card.text ^ "\n") ;
  match card.action with 
  | Collect n -> p.money <- p.money + n
  | Move spaces -> move_action p spaces state
  | Keep -> add_card p card
  | All n -> pay_all_players p [] n
  | Taxes (house , hotel) -> 
    p.money <- p.money -
               pay_per_house_hotel (p.properties) house hotel

(** [move_backwards player] moves [player] back 3 spaces depending on what
    their current position is.
    Requires:
      [player].position has a classification of Chance since those are the
      only tiles that a player can move backwards from. *)
and move_backwards player state = 
  match player.position.index with 
  | 7 -> player.position <- tax1; pay_taxes player player.position
  | 22 -> player.position <- orange3; 
    land_on_property player player.position state
  | n -> player.position <- chest3; follow_card_instructions player 
      (choose_random_card 
         game_state.board.community_chest_deck state) state

(** [send_to_jail pl] sends [pl] to jail and updates their position on the 
    board. *)
and send_to_jail pl = 
  pl.is_in_jail <- true ; 
  pl.position <- jail ; 
  pl.number_of_turns <- 0

(** [move_action player spaces] moves player [p] based on the number of spaces
    [spaces]. *)
and move_action player spaces state = 
  match spaces with
  | -3 -> move_backwards player state
  | 0 -> player.position <- go; player.money <- player.money + 200
  | 5 -> move player (find_nearest_railroad player.position.index); 
    land_on_railroad player player.position state true
  | 10 -> send_to_jail player
  | 20 -> move player (find_nearest_utility player.position.index);
    land_on_utility player player.position state true
  | 24 -> move player (find_illinois_ave player.position.index);
    land_on_property player player.position state
  | 39 -> move player (39 - player.position.index);
    land_on_property player player.position state
  | _ -> ()

(** [tile_action p tile] has the player perform some action based on the 
    classification of the tile [tile] that they have just landed on. *)
let tile_action p tile state = 
  match tile.classification with 
  | Go -> ()
  | Free -> ()
  | Jail -> ()
  | To_Jail -> send_to_jail p
  | Tax -> pay_taxes p tile
  | Railroad -> land_on_railroad p tile state false
  | Utility -> land_on_utility p tile state false
  | Chance -> follow_card_instructions p 
                (choose_random_card 
                   game_state.board.chance_deck game_state) state
  | Chest -> follow_card_instructions p 
               (choose_random_card 
                  game_state.board.community_chest_deck game_state) state
  | Property property -> land_on_property p tile state


(*------------------------START HOTEL/HOUSE PURCHASING---------------------- *) 

let rec props_assoc properties props = 
  match properties with
  | [] -> props
  | h :: t -> if List.mem_assoc h.color props then 
      let oldval = List.assoc h.color props in 
      List.remove_assoc h.color props |> 
      List.cons (h.color, oldval +1) 
      |> props_assoc t 
    else props_assoc t ((h.color, 1) :: props)

(** [get_finished_colorgroups_helper k v] takes in a Board.propertyColor [k] 
    and returns a boolean if number of properties own [v] is equal to the total 
    number of properties for a given color. *)
let get_finished_colorgroups_helper k v = 
  match k with 
  | Board.BROWN | Board.DARKBLUE -> v = 2
  | Board.MAGENTA | Board.LIGHTBLUE | Board.ORANGE | Board.RED 
  | Board.YELLOW | Board.GREEN -> v = 3

let rec get_finished_colorgroups prop_assoc_list 
    (build_colors: Board.propertyColor list) = 
  match prop_assoc_list with
  | [] -> build_colors
  | (k,v) :: t -> if get_finished_colorgroups_helper k v then 
      get_finished_colorgroups t (k :: build_colors) 
    else get_finished_colorgroups t build_colors

let rec tiles_to_props tile = 
  match tile.classification with
  | Property p -> p
  | _ -> failwith "IMPOSSIBLE"  

(** [props_list player] based on the [player] the function returns a list of 
    Board.propertyColor of colors the player can build on. *)
let props_list player = 
  let prop_assoc_list = 
    props_assoc (List.map tiles_to_props player.properties) [] in
  get_finished_colorgroups prop_assoc_list [] 

(** [get_props_by_color color] returns a list of properties that are of 
    [color]. *)
let get_props_by_color color =
  match color with
  | Board.BROWN -> [Board.prop_brown1; Board.prop_brown2]
  | Board.LIGHTBLUE -> 
    [Board.prop_lightblue1; Board.prop_lightblue2; Board.prop_lightblue3]
  | Board.MAGENTA -> 
    [Board.prop_magenta1; Board.prop_magenta2; Board.prop_magenta3]
  | Board.ORANGE -> 
    [Board.prop_orange1; Board.prop_orange2; Board.prop_orange3]
  | Board.RED -> 
    [Board.prop_red1; Board.prop_red2; Board.prop_red3]
  | Board.YELLOW -> 
    [Board.prop_yellow1; Board.prop_yellow2; Board.prop_yellow3]
  | Board.GREEN -> 
    [Board.prop_green1; Board.prop_green2; Board.prop_green3]
  | Board.DARKBLUE -> [Board.prop_darkblue1; Board.prop_darkblue2;]

(** [check_hh_helper props total] returns the [total] number of houses on 
    a property color group [props]. *)
let rec check_hh_helper props total =
  match props with 
  | [] -> total
  | h :: t -> check_hh_helper t h.num_houses + total 

(** [get_house_total color total] returns a string "HOUSE" or "HOTEL" based on 
    [total] number of houses on a particular [color] group. *)
let get_house_total color total =
  match color with 
  | Board.BROWN | Board.DARKBLUE -> if total = 8 then "HOTEL" else "HOUSE"
  | _ -> if total = 12 then "HOTEL" else "HOUSE"

(** [check_buying_cap props player btype] takes in a list of [props] and checks
    if a [player] has enough money to build a building of type [btype]. *)
let check_buying_cap props player btype = 
  match props with 
  | [] -> false
  | h :: t -> 
    match btype with 
    | "HOUSE" -> h.price_of_house < player.money
    | _ -> h.price_of_hotel < player.money

(** [remove_colors props colors remove keep] takes in a list of colors [colors] 
    and returns a list [keep] excluding all elements in [remove]. *)
let rec remove_colors colors remove keep = 
  match colors with 
  | [] -> keep
  | h :: t -> if List.mem h remove then remove_colors t remove keep
    else remove_colors t remove (h :: keep)

(** [check_is_mort props colors] takes in a list of properties [props] and 
    returns a list of [colors] that have properties mortgaged. *)
let rec check_is_mort props colors = 
  match props with 
  | [] -> colors
  | h :: t -> if h.is_mortgaged then let prop = tiles_to_props h in
      if List.mem prop.color colors then check_is_mort t colors else
        check_is_mort t (prop.color :: colors) else check_is_mort t colors

(** [check_hh color player] check if [player] is eligable to build a house 
    or hotel on properties of color [color]. *)
let check_hh color player =
  let props = get_props_by_color color in
  let num_house = check_hh_helper props 0 in
  let btype = get_house_total color num_house in
  if (check_buying_cap props player btype) then btype else "NONE"

(** [check_build_rules color_list player] returns an association list [lst] 
    of properties a [player] can build on based from [color_list]. *)
let rec check_build_rules color_list player lst = 
  match color_list with 
  | [] -> lst
  | h :: t -> let btype = check_hh h player in 
    if btype <> "NONE" then check_build_rules t player ((h, btype) :: lst) 
    else check_build_rules t player lst

let get_props player = 
  let colors = (props_list player) in
  let remove = check_is_mort player.properties [] in
  let new_colors = remove_colors colors remove [] in 
  check_build_rules new_colors player []

(** [get_build_colors props colors] take an association list [props] and returns
    a list of [colors]. *)
let rec get_build_colors props colors = 
  match props with 
  | [] -> colors
  | (color, b) :: t -> get_build_colors t (color :: colors)

(** [check_eveness_hotel props lst] takes in a list of [props] and returns a 
    list [lst] of buildable props [lst] based on eveness of hotels. *)
let rec check_eveness_hotel props lst = 
  match props with
  | [] -> lst
  | h :: t -> if h.num_hotels > 0 then check_eveness_hotel t lst else 
      check_eveness_hotel t (h :: lst)

(** [check_hotels prop_colors new_colors] takes in a list of colors 
    [prop_colors] and returns a list [new_colors] of buildable props based on 
    number of hotels built. *)
let rec check_hotels prop_colors new_colors = 
  match prop_colors with
  | [] -> new_colors
  | h :: t -> if (List.length (check_eveness_hotel (get_props_by_color h) [])) 
                 != 0 then
      check_hotels t (h:: new_colors) else check_hotels t new_colors

(** [get_props_final player] returns a list of colors based on eligable 
    properties that a [player] can build on. *)
let get_props_final player =  get_build_colors (get_props player) []


let can_build player = 
  List.length (check_hotels (get_props_final player) []) > 0 

(** [prop_color_to_string color] takes in a Board.propertyColor [color] and 
    returns it as a string. *)
let prop_color_to_string color = 
  match color with
  | Board.BROWN -> "Brown"
  | Board.LIGHTBLUE -> "Light Blue"
  | Board.MAGENTA -> "Magenta"
  | Board.ORANGE -> "Orange"
  | Board.RED -> "Red"
  | Board.YELLOW -> "Yellow"
  | Board.GREEN -> "Green"
  | Board.DARKBLUE -> "Dark Blue"

(** [string_to_prop_color color] takes in a string [color] and 
    returns it as a Board.propertyColor. *)
let string_to_prop_color color = 
  match color with
  | "Brown" -> Board.BROWN 
  | "Light Blue" -> Board.LIGHTBLUE
  | "Magenta" -> Board.MAGENTA
  | "Orange" -> Board.ORANGE
  | "Red" -> Board.RED
  | "Yellow" -> Board.YELLOW
  | "Green" -> Board.GREEN
  | _ -> Board.DARKBLUE

(** [list_to_string colors str] takes in a list of strings [colors] and 
    returns it as a single string [str]. *)
let rec list_to_string colors str = 
  match colors with 
  | [] -> str
  | h :: t -> list_to_string t  h  ^ ", " ^ str

(** [string_prop_colors prop_colors] takes in a list of Board.propertyColor 
    [prop_colors] and returns it as a list of strings. *)
let string_prop_colors prop_colors = 
  List.map prop_color_to_string prop_colors

(** [build_hotel player property name] updates a [players] total hotels on a
    given [property] and subtracts the hotel cost from their money. Prints that 
    the player has built a hotel on property [name]. *)
let build_hotel player property name = 
  property.num_hotels <- property.num_hotels + 1;
  player.money <- player.money - property.price_of_hotel;
  print_string ("\nYou just built a hotel on " ^ name ^ "!\n")

(** [build_house player property name] updates a [players] total houses on a
    given [property] and subtracts the house cost from their money. Prints that 
    the player has built a house on property [name]. *)
let build_house player property name = 
  property.num_houses <- property.num_houses + 1;
  player.money <- player.money - property.price_of_house;
  print_string ("\nYou just built a house on " ^ name ^ "!\n")

(** [build player property name] has [player] build a [btype] on [prop] named
    [name]. *)
let build player prop name btype = 
  match btype with  
  | "HOUSE" -> build_house player prop name
  | "HOTEL" -> build_hotel player prop name
  | _ -> ()

(** [build_hh player name prop] updates a [players] total houses/hotels on a
    given [prop] with helper functions and asks user if they want to build a 
    house or hotel on property [name]. *)
let build_hh player name prop btype = 
  print_string 
    ("\nYou can build a " ^ btype ^ " on " ^ name ^ ". Would you like to?\n");
  let player_input = read_line () |> validate_response valid_yes_no in
  match player_input with
  | "YES" | "Yes" | "yes" | "Y" | "y" -> build player prop name btype
  | "NO" | "No" | "no" | "N" | "n" -> print_string ("\nSkipping!\n"); ()
  | _ -> ()

(** [check_eveness_house props lst num] takes in a list of [props] and returns 
    a list [lst] of buildable props [lst] based on eveness of houses and using 
    [num] as a minimum value. *)
let rec check_eveness_house props lst num = 
  match props with
  | [] -> lst
  | h :: t -> if h.num_houses > num then check_eveness_house t lst num else 
    if h.num_houses = num then check_eveness_house t (h :: lst) h.num_houses 
    else check_eveness_house t [h] h.num_houses

(** [get_hh_num_helper props num byte] helper to get_h_or_h_num, returns 
    the smallest number [num] of houses or hotels [btype] built on [props]. *)
let rec get_hh_num_helper props num btype = 
  match props with
  | [] -> num
  | h :: t -> match btype with
    | "HOUSE" -> if num < h.num_houses then get_hh_num_helper t num btype
      else get_hh_num_helper t h.num_houses btype
    | _ -> if num < h.num_hotels then get_hh_num_helper t num btype
      else get_hh_num_helper t h.num_hotels btype

(** [get_hh_num props byte] returns the number of 
    houses or hotels [btype] built on the first element in [props]. *)
let get_hh_num props btype = 
  match props with
  | [] -> failwith "ERROR: EMPTY PROPS"
  | h :: t -> match btype with
    | "HOUSE" -> h.num_houses
    | _ -> h.num_hotels

(** [check_eveness_build color bytpe] returns a list of eligable properties of 
    [color] group to build [btype] on. *)
let check_eveness_build color btype =
  let props = get_props_by_color color in
  let num_init = get_hh_num props btype in
  let num = get_hh_num_helper props num_init btype in
  if btype = "HOUSE" then check_eveness_house props [] num  
  else check_eveness_hotel props [] 

(** [get_prop_names props str] returns the names of [props] as a string [str].*)
let rec get_prop_names (props: Board.property list) str =
  match props with
  | [] -> str
  | h :: t -> get_prop_names t (h.name ^ ", " ^ str)

(** [get_prop_names props lst] returns the names of [props] a list [lst]. *)
let rec get_list_prop_names (props: Board.property list) lst =
  match props with
  | [] -> lst
  | h :: t -> get_list_prop_names t (h.name :: lst)

(**  [get_prop_by_name name tile] is the property value of the tile that has 
     the name [name]
     Requires: [name] is a valid property name on the current gameboard. *)
let rec get_prop_by_name name tile = 
  match tile with 
  | Some t -> if name = t.name then 
      begin 
        match t.classification with 
        | Property p -> p 
        | _ -> failwith " this isn't a property"
      end
    else get_prop_by_name name t.next
  | None -> failwith "this is an invalid property!"


(** [build_on player color props] Based on the color group [color] the [player] 
    wants to build on out of [props], matches the user input to property name. 
    Uses helper functions to build on selected property. *)
let build_on player color props = 
  let btype = List.assoc color props  in
  let buildable = check_eveness_build color btype in
  let prop_str = get_prop_names buildable "" in
  let valid_responses = get_list_prop_names buildable [] in
  print_string (
    "\nBased on your current buildings, you can build on properties " 
    ^ prop_str 
    ^ "which property would you like to build on?\n");
  let player_input = read_line () |> 
                     validate_response ("nvm" :: valid_responses) in
  if player_input = "nvm" then print_string ("\nSkipping!\n") else
    let prop = get_prop_by_name player_input (Some go) in 
    build_hh player player_input prop btype

(** [build_on_props player props] Based on the buildable color group 
    properties [props] owned by [player], the player selects what color 
    they want to build on. *)
let build_on_props player props = 
  let prop_colors = check_hotels (get_props_final player) [] in
  let colors_list = string_prop_colors prop_colors in
  begin 
    print_string ("\nYou can build on properties of color " ^
                  list_to_string colors_list "" 
                  ^ "which color would you like to build on?\n");
    let player_input = 
      read_line () |> validate_response ("nvm" :: colors_list) in
    match player_input with 
    | "Brown" | "Light Blue" | "Magenta" | "Orange" | 
      "Red" | "Yellow" | "Green" | "Blue" | "Dark Blue" -> 
      build_on player (string_to_prop_color player_input) props
    | "nvm" -> print_string ("\nSkipping!\n"); ()
    | _ -> print_string("\nThis isn't a valid color.\n")
  end

(** [build_action player] calls function build_on_props so [player] can
    build on properties. *)
let build_action player =
  build_on_props player (get_props player)

(*------------------------END HOTEL/HOUSE PURCHASING------------------------ *) 

(*------------------------START HOTEL/HOUSE SELLING------------------------- *) 

(** [builded_colors props colors] takes in a list of [props] and returns a
    list of eligable build colors [colors]. *)
let rec builded_colors props colors =
  match props with 
  | [] -> colors
  | h :: t -> if h.num_houses > 0 then 
      if List.mem h.color colors then builded_colors t colors 
      else builded_colors t (h.color :: colors) 
    else builded_colors t colors

(** [sell_house player prop] updates [player] stats and sells 
    a house off of [prop] *)
let sell_house player prop = 
  player.money <- player.money + prop.price_of_house/2;
  prop.num_houses <- prop.num_houses - 1

(** [sell_hotel player prop] updates [player] stats and sells 
    a hotel off of [prop] *)
let sell_hotel player prop = 
  player.money <- player.money + prop.price_of_hotel/2;
  prop.num_hotels <- 0;
  prop.num_houses <- 0;
  print_string ("\nYou just sold a HOTEL on " ^ prop.name ^ "!\n")

(** [finish_sell player prop btype] updates [player] stats and sells a [btype]
    off of [prop] *)
let finish_sell player prop btype =
  match btype with 
  | "HOUSE" -> sell_house player prop
  | "HOTEL" -> sell_hotel player prop
  | _ -> failwith"Impossible"

(** [sell player name prop btype] sells a [btype] off [prop] named [name]
    and updates a [player] stats *)
let sell player name prop btype = 
  print_string ("\nCONFIRM: You want to sell a " ^ btype ^ " on property " 
                ^ name ^ "?\n");
  let player_input = read_line () |> validate_response valid_yes_no in
  match player_input with
  | "YES" | "Yes" | "yes" | "Y" | "y" -> finish_sell player prop btype;
    print_string ("\nYou just sold a " ^ btype ^ " on property " ^ name ^ "!\n")
  | "NO" | "No" | "no" | "N" | "n" ->  print_string ("\nSkipping!\n"); ()
  | _ -> failwith"Impossible"

(** [check_eveness_sell props lst num] takes in a list of [props] and returns a 
    list [lst] of props with sellable buildings based on num. *)
let rec check_eveness_sell props lst num = 
  match props with
  | [] -> lst
  | h :: t -> if h.num_houses < num then check_eveness_sell t lst num else 
    if h.num_houses = num then check_eveness_sell t (h :: lst) num
    else check_eveness_sell t [h] h.num_houses

(** [get_sell_hotels props hotel_props num] takes in a list of props [props] and 
    returns a list of props [hotel_props] whose number of hotels equals [num].*)
let rec get_sell_hotels props hotel_props num = 
  match props with
  | [] -> hotel_props
  | h :: t -> if h.num_hotels = num 
    then get_sell_hotels t (h :: hotel_props) num
    else get_sell_hotels t hotel_props num 

(** [sell_eveness props num btype] takes in a list of props [props] and inital
    number [num] and building type [btype] and returns a list of eligable sell 
    properties. *)
let sell_eveness props num btype = 
  match btype with 
  | "HOUSE" -> check_eveness_sell props [] num
  | "HOTEL" -> let lst = get_sell_hotels props [] num in 
    if List.length lst = 0 then props else lst
  | _ -> failwith"Impossible"

(** [get_hotel_total props total] returns the [total] number of hotels on a
    list of properties [props]. *)
let rec get_hotel_total props total =
  match props with 
  | [] -> total
  | h :: t -> get_hotel_total t h.num_hotels + total 

(** [sell_btype props] takes in a list of [props] and returns based
    on the type of buliding that can be sold on those properties. *)
let sell_btype props = if get_hotel_total props 0 > 0 then "HOTEL" else "HOUSE"

(** [sell_smallest_no props num btype] takes in a list of [props] and an intial
    [num] and returns the largest number of [btype] for those properties. *)
let rec sell_largest_no props num btype = 
  match props with
  | [] -> num
  | h :: t -> match btype with
    | "HOUSE" -> if num > h.num_houses then get_hh_num_helper t num btype
      else sell_largest_no t h.num_houses btype
    | _ -> if num > h.num_hotels then get_hh_num_helper t num btype
      else sell_largest_no t h.num_hotels btype

(** [sell_hh player colors] sells a building on property of [color] that a
    [player] owns. *)
let sell_hh player color = 
  let props = get_props_by_color color in
  let btype = sell_btype props in
  let num_init = sell_largest_no props 0 btype in
  let even_props = sell_eveness props num_init btype in
  let prop_str = get_prop_names even_props "" in
  let valid_responses = get_list_prop_names even_props [] in
  begin 
    print_string 
      ("\nYou can sell buildings of type " ^ btype ^ " on properties " 
       ^ prop_str ^ "which property would you like to sell on?\n");
    let player_input = read_line () |> 
                       validate_response ("nvm" :: valid_responses) in
    if player_input = "nvm" then print_string ("\nSkipping!\n") else
      let prop = get_prop_by_name player_input (Some go) in 
      sell player player_input prop btype
  end 

(** [sell_buildings player colors] takes in a list of eligable colors [colors]
    a [player] can sell buildings on and lets player build on them. *)
let sell_buildings player colors = 
  let colors_list = string_prop_colors colors in
  begin 
    print_string ("\nYou can sell on properties of color " ^
                  list_to_string colors_list "" 
                  ^ "which color would you like to sell on?\n");
    let player_input = 
      read_line () |> validate_response ("nvm" :: colors_list) in
    match player_input with 
    | "Brown" | "Light Blue" | "Magenta" | "Orange" | 
      "Red" | "Yellow" | "Green" | "Blue" | "Dark Blue" -> 
      sell_hh player (string_to_prop_color player_input)
    | "nvm" -> print_string ("\nSkipping!\n"); ()
    | _ -> print_string("\nThis isn't a valid color.\n")
  end 

let sell_colors player = 
  let props = List.map tiles_to_props player.properties in
  builded_colors props []

let sell_buildings player = sell_buildings player (sell_colors player)

(*--------------------------END HOTEL/HOUSE SELLING------------------------- *) 

let take_build_action player = if can_build player then build_action player 
  else print_string ("\nYou are unable to build on any properties right now.\n")

let take_action player state = 
  tile_action player player.position state

(* ------------ BEGIN MORTGAGING -------------------------------*)

(** [mortgage_prop player tile] changes [tile] so that it is now mortgaged
    and gives the player [player] the mortgage value of [tile].*) 
let mortgage_prop player tile =
  tile.is_mortgaged <- true;
  player.money <- player.money + tile.mortgage

(** [can_mortgage tile] checks to see if a tile [tile] can be mortgaged. If a
    property has houses on it then it cannot be mortgaged. *)
let can_mortgage tile player player_input = 
  let mortgage = match tile.classification with 
    | Property p -> check_hh_helper (get_props_by_color p.color) 0 = 0
    | Railroad | Utility -> true
    | _ -> false in 
  if mortgage then 
    begin
      mortgage_prop player tile; 
      print_string ("\nYou have now mortgaged " ^ player_input ^ ".\n")
    end
  else print_string(
      "\nYou cannot mortgage a property that has houses and hotels " ^
      " on any of its colors. Please sell them first and try again.\n")

(** [tile_owned_by_player tile player] is whether or not tile [tile] is owned
    by player [player]. *)
let tile_owned_by_player tile (player : player) = 
  let tile_owner = match tile.owner with 
    | Some p -> p 
    | None -> "" in 
  tile_owner <> "" && tile_owner = player.name

(** [print_owned_tiles tiles] prints out all the tiles in tiles. *)
let rec print_tiles tiles = 
  match tiles with 
  | [] -> print_string "\n";
  | h :: t -> print_string ("\n" ^ h.name ^ "\n"); print_tiles t

let mortgage_prompt (player : player) = 
  let players_assets = 
    player.properties @ player.utilities @ player.railroads in
  let can_be_mortgaged = 
    List.filter (fun t -> not t.is_mortgaged) players_assets in
  if List.length can_be_mortgaged <> 0 then 
    begin
      print_string("\nWhich property would you like to mortgage? These are " ^ 
                   "all the properties you can mortgage: \n");
      print_tiles can_be_mortgaged; 
      let player_input = 
        read_line () |> String.uppercase_ascii 
        |> validate_response (List.map name_of_tile can_be_mortgaged) in
      let tile = get_tile_by_name player_input (Some go) in 
      can_mortgage tile player player_input
    end
  else print_string("\nYou do not have any properties to mortgage. " ^ 
                    "Try to mortgage again later!\n")

(** [lift_mortgage player tile] takes the mortgage off of tile [tile] and makes
    the player pay the mortgage amount of [tile] plus 10% interest. *)
let lift_mortgage player tile = 
  let money_owed = 
    tile.mortgage |> float_of_int |> ( *. ) 0.1 
    |> int_of_float |> (+) tile.mortgage in
  if money_owed <= player.money then 
    begin
      tile.is_mortgaged <- false;
      player.money <- player.money - money_owed 
    end
  else print_string(
      "You do not have enough money to lift this mortgage. You can " ^
      "try again later.")

let lift_mortgage_prompt player = 
  let player_assets = player.properties @ player.utilities @ player.railroads in
  let mortgaged_props = List.filter (fun t -> t.is_mortgaged) player_assets in
  if List.length mortgaged_props = 0 then 
    print_string ("\nYou don't have any properties to lift a mortgage " ^
                  "from. Please try again after you've mortgaged!\n")
  else 
    begin
      print_string("\nWhich property would you like to lift mortgage from? " ^
                   "These are all the properties that you have mortgaged: \n");
      print_tiles mortgaged_props;
      let player_input = 
        read_line () |> String.uppercase_ascii 
        |> validate_response (List.map name_of_tile mortgaged_props) in
      let chosen_tile = get_tile_by_name player_input (Some go) in 
      lift_mortgage player chosen_tile;
      print_string (
        "\nYou have now lifted the mortgage off of " ^ chosen_tile.name ^ ".\n")
    end