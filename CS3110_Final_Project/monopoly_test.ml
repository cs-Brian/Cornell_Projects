open OUnit2
open Player
open Board
open State
open Actions
open Dice


(* ---------------------------- TEST PLAN -------------------------------- *)

(** TEST PLAN:
    In our test plan the modules that we have tested are player, board, dice, 
    and some of the functions in actions. We approached OUnit testing by 
    testing all functions that did not have user input involved. 
    For a lot of the functions in actions and also in printers and main, 
    a lot depended on what the user chose to do and after going to office 
    hours that it was best to test those situations manually. Something else 
    we did was test the helper functions of the important functions that 
    depended on user input. 

    Modules tested with OUnit: Player, Dice, Board, Actions (parts of building 
    houses and hotels). 

    For testing module Player, we checked to see that the key functionality was
    working, such as a a player's location once they move, having the correct
    next tile, asserting they had the correct amount of money and checking
    if they were bankrupt. We were also able to confirm this by playing the game
    when the the title of the player's location is printed out and manually 
    checking that when a player moves from tile x to tile y, it is equal to the 
    number they rolled. 

    For testing module Dice, we applied a similar logic. We made sure that 
    our sort function was working as expected since that dictates the players'
    turn order and made sure that it was being printed correctly. Through game 
    play we confirmed that it was working as expected for the player and that
    edge cases (everyone rolling the same number or multiple people rolling
    the same number) were handled.

    For testing module Board, we knew that our move and build tests would also
    ensure that the board's functionality was correct. We additionally tested
    the board by making sure that a tile's names would give the correct tile, 
    and by entering a certain position on a board we would get the correct tile.

    For testing the build feature in module Actions we tested key helper 
    functions that return property lists or numbers since we could choose 
    the input and know for sure what the expected output would be. 
    By testing these major functions that are integral to what and where a 
    player can build, and also call on the smaller functions that we did not
    test directly through OUnit, we knew that the foundation was correct. 
    To make sure that the user prompts were correct, we played throught the game 
    multiple times and checked all edge cases (colors that have 2 properties, 
    applying different build rules) manually through game play.

    We approached testing by doing black box testing. We decided not to go
    with glass box testing because we believed that all of those nit cases 
    in small conditionals or pattern matching would be encountered at least
    once with the manual testing we did while playing multiple games and if 
    there were any bugs or issues there we would catch them in that moment.

    Modules tested manually: Printers, Errors, Cards, a bulk of Actions, Main,
    and State.

    We tested these modules by either manually creating the game situations 
    we needed and double checking to make sure that everything that was 
    supposed to happen happened. In doing this, we did take more of a glassbox
    approach because we wanted to make sure every case was hit to make sure the
    right thing happened. We also manually tested by playing a few games as a 
    team because the best way to make sure that everything in the game is going
    to go well is by just playing the game :)
    Additionally, each person responsible for a feature throughly tested by 
    playing the game and making sure to test each test case on top of us playing
    together as a team.  

    We believe that this testing approach demonstarates the correctness of the 
    system because we left nothing unchecked and we made sure that for all code
    that we wrote there was at least 2 people double checking to see that that
    was correct. There was a lot that we could not test with OUnit and that's 
    mainly because of the complexities of this game but just because we could
    not OUnit test doesn't mean we didn't check functionality! We did not save
    testing for the end because we knew that that would result in us not 
    considering edge cases so because we were testing the entire time we 
    believe that our code is correct.
*)


(* --------------------------- HELPER FUNCTIONS ---------------------------- *)

(* PLAYER TESTS *)
(** A [move_test name player spaces expected_position] test named [name] 
    determines whether or not the [player] has the correct [expected_position] 
    after being moved [spaces] spaces on the board. *)
let move_test 
    (name : string) 
    (player : Player.player)
    (spaces : int)
    (expected_position : tile) : test = 
  name >:: (fun _ -> 
      player.position <- go;
      move player spaces ;
      assert_equal expected_position player.position 
        ~printer: (fun tile -> tile.name))

(** A [next_tile_test name start_tile spaces player expected_position] is a test 
    named [name] that determines whether or not the next tile for a [player] is 
    equal to [expecteed_output] based on their [start_tile] after being moved x
    [spaces]on the board. *)
let next_tile_test 
    (name : string)
    (start_tile : tile)
    (spaces : int)
    (player : Player.player) 
    (expected_output : tile) : test =
  name >:: (fun _ -> assert_equal expected_output 
               (next_tile start_tile spaces player))

(** A [is_bankrupt_test name start_tile player expected_output] is a test 
    named [name] that asserts that when a [player] is bankrupt the output is
    equal to [expected_output]. *)
let is_bankrupt_test 
    (name : string)
    (player : Player.player)
    (expected_output : bool) : test = 
  name >:: (fun _ -> assert_equal expected_output (is_bankrupt player))

(* END OF PLAYER TESTS *)

(** A [money_test name player expected_amount] test named [name] determines 
    whether or not the [player] has the correct money [expected_amount]. *)
let money_test 
    (name : string) 
    (player : Player.player)
    (expected_amount : int) : test = 
  name >:: (fun _ -> assert_equal expected_amount player.money
               ~printer: string_of_int)

(** A [tiles_to_props_test name tiles prop expected_output] test named [name] 
    determines whether or not the [props] has the correct properties 
    [expected_output] based on the given [tiles].  *)
let tiles_to_props_test 
    (name : string) 
    (tile : Board.tile)
    (expected_output : Board.property) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (tiles_to_props tile))

(** A [props_assoc_test name tiles properties expected_output] test named [name] 
    determines whether or not the [props] has the correct keys and values 
    [expected_output] based on the given [properties]. *)
let can_build_test 
    (name : string) 
    (player: Player.player)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (can_build player))

(** A [props_assoc_test name tiles properties expected_output] test named [name] 
    determines whether or not the [props] has the correct keys and values 
    [expected_output] based on the given [properties]. *)
let props_assoc_test 
    (name : string) 
    (properties: Board.property list)
    (props: (Board.propertyColor * int) list)
    (expected_output : (Board.propertyColor * int) list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (props_assoc properties props))

(** A [can_build_list_test name prop_assoc_list build_colors expected_output] 
    test named [name] determines whether or not the [build_colors] is the 
    correct Board.propertyColor list [expected_output] 
    based on the given [prop_assoc_list]. *)
let get_finished_colorgroups_test 
    (name : string) 
    (prop_assoc_list: (Board.propertyColor * int) list)
    (build_colors: Board.propertyColor list)
    (expected_output : Board.propertyColor list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (get_finished_colorgroups prop_assoc_list build_colors))


(** [check_hh_test name color player expected_output] is a test 
    named [name] determines whether or not a [player] can build on property
    of [color] and checks that the type of building it outputs is equal to
    [expected_output]. *)
let check_hh_test
    (name : string) 
    (color: Board.propertyColor)
    (player: Player.player)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (check_hh color player))

(** [get_props_test name player expected_output] is a test 
    named [name] determines whether or not the properties a [player] can build 
    on is equal to [expected_output]. *)
let get_props_test
    (name : string) 
    (player: Player.player)
    (expected_output : (Board.propertyColor * string) list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_props player))

(** [take_action_test name p state expected_position] is a test named [name]
    that checks if a [player]'s position after taking an action in the current
    [state] is equal to [expected_position] *)
let take_action_test
    (name : string) 
    (p: Player.player)
    (state: State.state)
    (expected_position : tile) : test = 
  name >:: (fun _ -> 
      take_action p state ;
      assert_equal expected_position p.position 
        ~printer: (fun tile -> tile.name))

(** A [sell_colors_test name p expected_output] 
    test named [name] determines whether or not the [expected_output] is the 
    correct list of sellable colors player [p] can build on. *)
let sell_colors_test
    (name : string) 
    (p: Player.player)
    (expected_output : Board.propertyColor list) : test = 
  name >:: (fun _ -> assert_equal expected_output (sell_colors p))

(* END OF BUILD/SELL TESTS *)

(* BOARD TESTS*)

(** [tile_name_test name pos expected_output] checks that the JSON with all 
    the tile names was parsed correctly by ensuring that the name of the tile
    at positon 39 - [pos] is the same as [expected_output]. *)
let tile_name_test
    (name : string)
    (pos : int)
    (expected_output : string) : test = 
  name >:: (fun _ -> assert_equal expected_output tile_names.(pos))
(** we can also check the get_tile_by_name function  *)

let get_tile_by_name_test
    (name : string)
    (tile_name : string)
    (expected_output : Board.tile) : test = 
  name >:: (fun _ -> assert_equal expected_output 
               (get_tile_by_name tile_name (Some go)))

(* END OF BOARD TESTS*)

(* BEGIN DICE TESTS *)

let string_of_player_rolls_test 
    (name : string)
    (player_rolls : (string * int) list)
    (expected_output : string) : test = 
  name >:: (
    fun _ -> assert_equal expected_output 
        (string_of_player_rolls player_rolls))

let sort_by_roll_test 
    (name : string)
    (player_rolls : (string * int) list)
    (expected_output : (string * int) list) : test = 
  name >:: (fun _ -> assert_equal expected_output (sort_by_roll player_rolls))

let is_all_same_test
    (name : string)
    (rolls : int list)
    (expected_output : bool) : test = 
  name >:: (fun _ -> assert_equal expected_output (is_all_same rolls))

(* END OF DICE TESTS *)

(* ----------------------- END OF HELPER FUNCTIONS -------------------------- *)

(* -------------------------- TESTING CONSTANTS ----------------------------- *)

let players = initialize_players ["Miah" ; "Brian" ; "Tayyaba" ; "Gio"]
let p1 = List.hd players
let player_with_no_money = {p1 with money = 0}
let bankrupt_player = {p1 with money = ~-10}


(*--------------------------BUY HOUSE/HOTEL CONSTANTS-------------------------*)
let tiles1 = [Board.darkblue1; Board.darkblue2; Board.magenta1; Board.magenta2;] 
let tiles2 = [Board.darkblue1; Board.darkblue2; 
              Board.magenta1; Board.magenta2; Board.magenta3;
              Board.brown1; Board.brown2;] 


let props1 = [Board.prop_magenta2; Board.prop_magenta1; 
              Board.prop_darkblue2; Board.prop_darkblue1]
let props2 = [Board.prop_magenta3; Board.prop_magenta2; Board.prop_magenta1; 
              Board.prop_brown1; Board.prop_brown2;
              Board.prop_darkblue2; Board.prop_darkblue1]

let props_assoc1 = [(Board.DARKBLUE, 2); (Board.MAGENTA, 2)]
let props_assoc2 = [(Board.DARKBLUE, 2); (Board.BROWN, 2); (Board.MAGENTA, 3)]
let props_assoc3 = [(Board.DARKBLUE, "HOUSE")]
let props_assoc4 = [(Board.DARKBLUE, "HOUSE"); (Board.MAGENTA, "HOUSE");
                    (Board.BROWN, "HOUSE");]


let pb1 = {name = "Builder" ; 
           money = 15000 ;
           position = go ;
           passed_go = true;
           properties = tiles1 ;
           railroads = [] ;
           utilities = [];
           cards = [];
           number_of_turns = 1;
           has_withdrawn = false;
           is_in_jail = false ;
           turns_in_jail = 0} 

let pb2 = {name = "Builder" ; 
           money = 15000 ;
           position = go ;
           passed_go = true;
           properties = [] ;
           railroads = [] ;
           utilities = [];
           cards = [];
           number_of_turns = 1;
           has_withdrawn = false;
           is_in_jail = false ;
           turns_in_jail = 0} 

let pb3 = {name = "Builder" ; 
           money = 15000 ;
           position = go ;
           passed_go = true;
           properties = tiles2 ;
           railroads = [] ;
           utilities = [];
           cards = [];
           number_of_turns = 1;
           has_withdrawn = false;
           is_in_jail = false ;
           turns_in_jail = 0} 

(*------------------------END OF BUY HOUSE/HOTEL CONSTANTS--------------------*)

(*----------------------START OF SELL HOUSE/HOTEL CONSTANTS-------------------*)

let s1 = {name = "Seller" ; 
          money = 15000 ;
          position = go ;
          passed_go = true;
          properties = tiles2;
          railroads = [] ;
          utilities = [];
          cards = [];
          number_of_turns = 1;
          has_withdrawn = false;
          is_in_jail = false ;
          turns_in_jail = 0} 

(*------------------------END OF SELL HOUSE/HOTEL CONSTANTS-------------------*)


let p5 = {name = "Clarkson"; 
          money = 200;
          position = to_jail;
          passed_go = true;
          properties = [] ;
          railroads = [] ;
          utilities = [];
          cards = [];
          number_of_turns = 1;
          has_withdrawn = false;
          is_in_jail = false ;
          turns_in_jail = 0}

let state1 = {
  players = [p5]; 
  board = board; 
  player_turn = Some(p1); 
  player_roll = 0;
  number_of_dice = 2;
  number_of_sides = 6;
}

let p6 = {name = "Gries" ; 
          money = 200 ;
          position = jail ;
          passed_go = true;
          properties = [] ;
          railroads = [] ;
          utilities = [];
          cards = [];
          number_of_turns = 1;
          has_withdrawn = false;
          is_in_jail = false ;
          turns_in_jail = 0} 

(* ------------------------ END OF TESTING CONSTANTS ------------------------ *)

let tests = [
  move_test "not moving at all starting at Go" p1 0 go ;
  move_test "move 1 space starting at Go" p1 1 brown1 ;
  move_test "move 25 spaces starting at Go" p1 25 railroad3 ;
  (** NOTE: I honestly think we should omit this test.
      if a player lands in to_jail then their final positon should 
      actually be jail  *)
  move_test "move 30 spaces starting at Go" p1 30 to_jail ;
  move_test "move completely around the board" p1 40 go ;

  next_tile_test "move 1 space from jail" jail 1 p1 magenta1;
  next_tile_test "move 10 places from jail" jail 10 p1 free_parking;
  next_tile_test "move from boardwalk to go" darkblue2 1 p1 go;
  next_tile_test "move around the board twice" go 80 p1 go;
  next_tile_test "pass jail but dont stay" lightblue3 2 p1 magenta1;

  is_bankrupt_test "player who isn't bankrupt" p1 false;
  is_bankrupt_test "player with no money isn't bankrupt" 
    player_with_no_money false;
  is_bankrupt_test "player who is bankrupt" bankrupt_player true;

  (*-------------------START OF BOARD TEST-------------------*)

  tile_name_test "last tile is boardwalk" 0 "BOARDWALK";
  tile_name_test "first tile is Go" 39 "GO";
  tile_name_test "10th tile is jail" 29 "JAIL";
  tile_name_test "middle of board is free parking" 19 "FREE PARKING";
  tile_name_test "communit chest card" 37 "COMMUNITY CHEST";

  get_tile_by_name_test "boardwalk all lowercase" "boardwalk" darkblue2;
  get_tile_by_name_test "boardwalk all uppercase" "BOARDWALK" darkblue2;
  get_tile_by_name_test "boardwalk weird casing" "bOArDWalk" darkblue2;
  get_tile_by_name_test "go tile" "go" go;
  get_tile_by_name_test "tile with 2 words" "free PARKing" free_parking;
  (*-------------------END OF BOARD TEST-------------------*)

  (*-------------------START OF BUILD/SELL HOTEL/HOUSE TEST-------------------*)

  props_assoc_test "2MAG 2DB PROPS_ASSOC_1" props1 [] props_assoc1;
  props_assoc_test "3MAG 2DB 2B PROPS_ASSOC_2" props2 [] props_assoc2;

  get_finished_colorgroups_test "2MG 2DB EXP: DB" 
    props_assoc1 [] [Board.DARKBLUE];

  get_finished_colorgroups_test "2MG 2DB 2B EXP: DB & B" 
    props_assoc2 [] [Board.MAGENTA; Board.BROWN; Board.DARKBLUE;];

  can_build_test "pb1 true" pb1 true;
  can_build_test "pb2 false" pb2 false;

  tiles_to_props_test "db1 to pdb1" Board.darkblue1 Board.prop_darkblue1;
  tiles_to_props_test "db2 to pdb2" Board.darkblue2 Board.prop_darkblue2;

  check_hh_test "pb3 HOUSE" Board.MAGENTA pb1 "HOUSE";
  check_hh_test "pb2 HOUSE" Board.DARKBLUE pb1 "HOUSE";

  get_props_test "pb1 props test" pb1 props_assoc3;
  get_props_test "pb3 props test" pb3 props_assoc4;

  sell_colors_test "no buildings" s1 [];

  (*-------------------END OF BUILD/SELL HOTEL/HOUSE TEST---------------------*)

  (*-------------------START OF DICE TEST---------------------*)
  string_of_player_rolls_test "Miah Gio Game" [("Miah", 6); ("Gio", 2);] 
    "Miah rolled a total of 6!\nGio rolled a total of 2!\n" ;
  string_of_player_rolls_test "No rolls" [] "";
  string_of_player_rolls_test "All players rolled"
    [("Miah", 12); ("Brian", 8); ("Tayyaba", 9); ("Gio", 0)]
    ("Miah rolled a total of 12!\nBrian rolled a total of 8!\n" ^ 
     "Tayyaba rolled a total of 9!\nGio rolled a total of 0!\n");

  sort_by_roll_test "rolls in ascending order" [("Miah", 2); ("Gio", 6);] 
    [("Gio", 6); ("Miah", 2)];
  sort_by_roll_test "rolls in descending order" [("Miah", 6); ("Gio", 2)]
    [("Miah", 6); ("Gio", 2)];
  sort_by_roll_test "no rolls" [] [];
  sort_by_roll_test "4 players rolls random dice order"
    [("Miah", 8); ("Brian", 12); ("Tayyaba", 9); ("Gio", 0)]
    [("Brian", 12); ("Tayyaba", 9); ("Miah", 8); ("Gio", 0)];

  is_all_same_test "smallest list possible" [1] true;
  is_all_same_test "list size 2 same element" [1; 1] true;
  is_all_same_test "list size 2 different elemnts" [1; 2] false;
  is_all_same_test "2 same 1 different" [1; 1; 2] false;
  is_all_same_test "3 of the same" [1; 1; 1] true;

  (*-------------------START OF DICE TEST---------------------*)

  take_action_test "move to jail when on to_jail tile" p5 state1 jail;
  take_action_test "move out of jail when player bails out of jail" 
    p6 state1 jail;

]

let suite = "monopoly tests" >::: tests

let _ = run_test_tt_main suite