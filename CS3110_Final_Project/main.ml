open Dice
open Player
open State
open Actions
open Board
open Errors
open Printers

let consecutive_doubles_to_jail = 3

let consecutive_doubles_to_jail_notification = 
  "\nYou have rolled " ^ string_of_int consecutive_doubles_to_jail ^ 
  " doubles in a row and have moved to JAIL!\n"

let local_bankrupt_notification = 
  "\nYou have gone bankrupt and are no longer in the game!\n" 

let double_rolled_notification =
  "\nYou have rolled a double-- it's your turn again.\n"

let global_bankrupt_notification (pl : Player.player) =
  "\n" ^ pl.name ^ " has gone bankrupt! They have been removed from the game!\n"

let move_notification spaces pl = 
  "You have rolled a " ^ string_of_int spaces ^ 
  " and have moved to " ^ pl.position.name ^ ".\n"

(** [move_and_action pl st spaces] moves the player [pl] [spaces] spaces in the
    monopoly game with state [st]. The player then takes an action based on the 
    tile they land on. *)
let move_and_action pl st spaces =
  move pl spaces ;  
  print_string (move_notification spaces pl) ; take_action pl st 

(** [handle_consecutive_doubles pl st d_roll] checks to see if player [pl] has
    rolled a double based on their dice roll [d_roll].
    If the player rolled a double...
      If the player is in jail and they have spent at least one full turn in 
      jail, then they move to the next space based on the dice_roll [d_roll] and 
      takeaction based on the tile that they landed on. 
      Otherwise, check to see if the player has rolled 3 doubles in a row. If 
      so, send them to jail. If not, simply increment the number of doubles they
      rolled in a row.
    If the player hadn't rolled a double, reset the number of doubles they
    rolled in a row. *)
let handle_consecutive_doubles pl st d_roll =
  let rolled_double = is_all_same d_roll in
  if rolled_double then
    begin
      if pl.is_in_jail && pl.turns_in_jail >= 1 then 
        (move_and_action pl st (List.fold_left ( + ) 0 d_roll) ; 
         pl.number_of_turns <- 0 ; 
         pl.is_in_jail <- false ; pl.turns_in_jail <- 0)
      else 
        begin
          if pl.number_of_turns = consecutive_doubles_to_jail then 
            send_to_jail pl
          else
            (print_string double_rolled_notification ;
             pl.number_of_turns <- pl.number_of_turns + 1)
        end
    end
  else 
    pl.number_of_turns <- 0 

(** [roll_and_move pl st] checks if the player [pl] has withdrawn from the game.
    with state [st]. If they haven't, then give them a dice roll, use that to
    calculate the number of spaces they should move, move the player and allow
    them to take and action based on the tile they landed on, and handle the
    case in which they rolled a double. *)
let roll_and_move pl st =
  (if pl.has_withdrawn = false then
     let dice_roll = roll st.number_of_dice st.number_of_sides in
     let spaces_to_move = List.fold_left (+) 0 dice_roll in 
     move_and_action pl st spaces_to_move ;
     handle_consecutive_doubles pl st dice_roll)

let check_if_left_jail pl st = 
  if pl.is_in_jail = false then (roll_and_move pl st)
  else 
    begin 
      pl.number_of_turns <- 0 ; 
      print_string (
        "\nYou did not roll a double. You have " ^ 
        string_of_int (3 - pl.turns_in_jail) ^ " turns in jail left.\n")
    end

let check_escaped_jail pl st = 
  if pl.turns_in_jail = consecutive_doubles_to_jail 
  then 
    begin 
      print_string ("\nYou have escaped jail! And had to pay $50\n");
      pl.money <- pl.money - 50; pl.is_in_jail <- false; pl.turns_in_jail <- 0 ;
      prompt_metadata_print pl st; roll_and_move pl st
    end
  else 
    begin
      prompt_jail_escape pl st;
      check_if_left_jail pl st 
    end

let handle_jail pl st = 
  if not pl.is_in_jail then 
    (prompt_metadata_print pl st ; roll_and_move pl st) 
  else 
    check_escaped_jail pl st 

(** [handle_consecutive_player_turns pl st] traps the player in a while-loop 
    until they either stop rolling doubles, or if they rolled 
    3 doubles in a row and therefore get sent to jail.
    If the player goes bankrupt on one of their turns, then set the number of
    times they rolled a double to 0, effectively ending the while-loop.
    Otherwise,
      If the player is not in jail, prompt them to print metadata, and then 
      allow them to roll and move. *)
let handle_consecutive_player_turns pl st = 
  while pl.number_of_turns > 0 do 
    if is_bankrupt pl then 
      (print_string local_bankrupt_notification ; pl.number_of_turns <- 0)
    else handle_jail pl st 
  done 

(** [handle_post_turn pl st] performs the tasks that should happen after player 
    [pl]'s turn ends, i.e. if the player has not withdrawn, then set their 
    number of turns to 1 for the next time they roll. If they went bankrupt, 
    notify the remaining players in the game with state [st].
    If after their turn, the player is in jail, increment the number of turns
    that they have been in jail for. *)
let handle_post_turn pl st = 
  if pl.has_withdrawn = false then pl.number_of_turns <- 1 ;
  if is_bankrupt pl && pl.has_withdrawn = false then 
    print_string (global_bankrupt_notification pl);
  if pl.is_in_jail then pl.turns_in_jail <- pl.turns_in_jail + 1

let player_turn (player : Player.player) state = 
  print_player_turn_notification player ;
  handle_consecutive_player_turns player state ;
  handle_post_turn player state

let rec take_player_turns players state =  
  match players with
  | [] -> ()
  | h :: t -> 
    if (is_game_over state = false) then
      if is_bankrupt h = false && h.has_withdrawn = false then
        (player_turn h state ; take_player_turns t state)
      else take_player_turns t state

let rec read_multiple_input n = 
  match n with 
  | 0 -> []
  | _ -> read_line () :: read_multiple_input (n - 1)

let rec read_multiple_names_input n  = 
  match n with 
  | 0 -> []
  | _ -> 
    let current_player_list = read_multiple_names_input (n - 1) in  
    (read_line () |> validate_name |> 
     validate_non_duplicate_name current_player_list) :: current_player_list

let start_game st pls nd ns = 
  st.players <- pls ; 
  st.number_of_dice <- nd ;
  st.number_of_sides <- ns ;
  print_string "\nReady to begin!\n" ;
  st

let annouce_ordering_logic nd ns = 
  print_string 
    ("\nEveryone rolls " 
     ^ string_of_int nd ^ " " ^  string_of_int ns ^ 
     " sided die to determine the order of the turns.\n");
  print_string "\n...\n" 

let print_ordering ord = 
  print_string ("Thus, the ordering of the turns will go as follows:
    " ^ ord ^ "\n"); print_string "\nSetting up game...\n"

let initialize_game () = 
  print_string "\nWelcome to Monopoly! Enter the number of players:\n";
  let num_players = read_line () |> validate_int in
  print_string "\nEnter the names of the players:\n";
  let player_names = num_players |> read_multiple_names_input |> shuffle in
  print_string "\nEnter number of dice:\n";
  let num_dice = read_line () |> validate_int in 
  print_string "\nEnter number of sides on each die:\n";
  let num_sides = read_line () |> validate_int in
  annouce_ordering_logic num_dice num_sides ;
  let rolls = Dice.player_rolls player_names num_dice num_sides in
  print_string ("\n" ^ string_of_player_rolls rolls ^ "\n");
  let sorted_rolls = sort_by_roll rolls in 
  let sorted_rolls_string = string_of_ordering sorted_rolls in
  print_ordering sorted_rolls_string ;
  let ordered_player_names = List.map (fun p -> fst p) sorted_rolls in
  let players = initialize_players ordered_player_names in
  start_game game_state players num_dice num_sides

let run () = 
  let game = initialize_game () in

  while (is_game_over game = false) do
    take_player_turns 
      game.players 
      game 
  done;

  let winner = winner game in 
  print_string ("\n" ^ winner.name ^ " has won the game!\n")

let _ = run ()
