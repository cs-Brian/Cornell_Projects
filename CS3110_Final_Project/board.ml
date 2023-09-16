open Cards

(* ------------------------------- TYPES ---------------------------------- *)  
type propertyColor = 
  | BROWN | LIGHTBLUE | MAGENTA | ORANGE | RED 
  | YELLOW | GREEN | DARKBLUE

type property = {
  color : propertyColor;
  rent_prices : (int * int) list;
  price_of_house : int;
  price_of_hotel : int;
  mutable num_houses: int;
  mutable num_hotels: int;
  name : string
}

type classification = 
  | Go | Free | Jail | To_Jail | Tax | Railroad | Utility 
  | Chance | Chest | Property of property

type tile = {
  index : int;
  name : string;
  classification : classification;
  mutable owner :  string option;
  cost : int;
  mutable next : tile option;
  mortgage : int;
  mutable is_mortgaged : bool;
}

type boardLinkedList = {
  head : tile;
  mutable chance_deck : card list;
  mutable community_chest_deck : card list;
}

type rentList = {
  rent_price : (int * int) list;
}

open Yojson.Basic.Util

let tile_names = 
  Yojson.Basic.from_file "original_gameboard.json" |> member "tiles" |> to_list 
  |> List.map to_string |> Array.of_list

(* ------------------------- Initialize the Board ------------------------- *)

(* Properties of the game board *)
let prop_darkblue2 = {
  color = DARKBLUE;
  rent_prices = [(0, 50); (1, 200); (2, 600); (3, 1400); (4, 1700); (5, 2000)];
  price_of_house  = 200;
  price_of_hotel  = 5 * 200;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(0);
}

let prop_darkblue1 = {
  color =  DARKBLUE;
  rent_prices = [(0, 35); (1, 175); (2, 500); (3, 1100); (4, 1300); (5, 1500)];
  price_of_house  = 200;
  price_of_hotel  = 5 * 200;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(2);
}

let prop_green3 = {
  color =  GREEN;
  rent_prices = [(0, 30); (1, 170); (2, 510); (3, 1100); (4, 1300); (5, 1525)];
  price_of_house  = 200;
  price_of_hotel  = 5 * 200;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(5);
}

let prop_green2 = {
  color =  GREEN;
  rent_prices = [(0, 28); (1, 150); (2, 450); (3, 1000); (4, 1200); (5, 1400)];
  price_of_house  = 200;
  price_of_hotel  = 5 * 200;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(7);
}

let prop_green1 = {
  color =  GREEN;
  rent_prices = [(0, 26); (1, 130); (2, 390); (3, 900); (4, 1100); (5, 1275)];
  price_of_house  = 200;
  price_of_hotel  = 5 * 200;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(8);
}

let prop_yellow3 = {
  color =  YELLOW;
  rent_prices = [(0, 26); (1, 130); (2, 390); (3, 900); (4, 1100); (5, 1250)];
  price_of_house  = 150;
  price_of_hotel  = 5 * 150;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(10);
}

let prop_yellow2 = {
  color =  YELLOW;
  rent_prices = [(0, 24); (1, 120); (2, 360); (3, 850); (4, 1025); (5, 1200)];
  price_of_house  = 150;
  price_of_hotel  = 5 * 150;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(12);
}

let prop_yellow1 = {
  color =  YELLOW;
  rent_prices = [(0, 22); (1, 110); (2, 330); (3, 800); (4, 975); (5, 1150)];
  price_of_house  = 150;
  price_of_hotel  = 5 * 150;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(13);
}

let prop_red3 = {
  color =  RED;
  rent_prices = [(0, 22); (1, 110); (2, 350); (3, 800); (4, 1000); (5, 1150)];
  price_of_house  = 150;
  price_of_hotel  = 5 * 150;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(15);
}

let prop_red2 = {
  color =  RED;
  rent_prices = [(0, 20); (1, 100); (2, 300); (3, 750); (4, 925); (5, 1100)];
  price_of_house  = 150;
  price_of_hotel  = 5 * 150;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(16);
}

let prop_red1 = {
  color =  RED;
  rent_prices = [(0, 18); (1, 90); (2, 250); (3, 700); (4, 875); (5, 1050)];
  price_of_house  = 150;
  price_of_hotel  = 5 * 150;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(18);
}

let prop_orange3 = {
  color =  ORANGE;
  rent_prices = [(0, 18); (1, 90); (2, 240); (3, 650); (4, 850); (5, 1050)];
  price_of_house  = 100;
  price_of_hotel  = 5 * 100;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(20);
}

let prop_orange2 = {
  color =  ORANGE;
  rent_prices = [(0, 16); (1, 80); (2, 220); (3, 600); (4, 800); (5, 1000)];
  price_of_house  = 100;
  price_of_hotel  = 5 * 100;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(21);
}

let prop_orange1 = {
  color =  ORANGE;
  rent_prices = [(0, 14); (1, 70); (2, 200); (3, 550); (4, 750); (5, 950)];
  price_of_house  = 100;
  price_of_hotel  = 5 * 100;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(23);
}

let prop_magenta3 = {
  color =  MAGENTA;
  rent_prices = [(0, 14); (1, 70); (2, 200); (3, 550); (4, 750); (5, 1000)];
  price_of_house  = 100;
  price_of_hotel  = 5 * 100;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(25);
}

let prop_magenta2 = {
  color =  MAGENTA;
  rent_prices = [(0, 12); (1, 60); (2, 180); (3, 500); (4, 700); (5, 900)];
  price_of_house  = 100;
  price_of_hotel  = 5 * 100;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(26);
}

let prop_magenta1 = {
  color =  MAGENTA;
  rent_prices = [(0, 10); (1, 50); (2, 150); (3, 450); (4, 625); (5, 750)];
  price_of_house  = 100;
  price_of_hotel  = 5 * 100;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(28);
}

let prop_lightblue3 = {
  color =  LIGHTBLUE;
  rent_prices = [(0, 10); (1, 50); (2, 110); (3, 330); (4, 500); (5, 650)];
  price_of_house  = 50;
  price_of_hotel  = 5 * 50;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(30);
}

let prop_lightblue2 = {
  color =  LIGHTBLUE;
  rent_prices = [(0, 8); (1, 40); (2, 100); (3, 300); (4, 450); (5, 600)];
  price_of_house  = 50;
  price_of_hotel  = 5 * 50;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(31);
}

let prop_lightblue1 = {
  color =  LIGHTBLUE;
  rent_prices = [(0, 6); (1, 30); (2, 90); (3, 270); (4, 400); (5, 550)];
  price_of_house  = 50;
  price_of_hotel  = 5 * 50;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(33);
}

let prop_brown2 = {
  color =  BROWN;
  rent_prices = [(0, 4); (1, 20); (2, 60); (3, 180); (4, 320); (5, 450)];
  price_of_house  = 50;
  price_of_hotel  = 5 * 50;
  num_houses = 0;
  num_hotels = 0;
  name = tile_names.(36);
}

let prop_brown1 = {
  color =  BROWN;
  rent_prices = [(0, 2); (1, 10); (2, 30); (3, 90); (4, 160); (5, 250)];
  price_of_house  = 50;
  price_of_hotel  = 5 * 50;
  num_houses = 0;
  num_hotels = 0;
  name  = tile_names.(38);
}

(* Initializing all tiles in the original monopoly gameboard. *)

let darkblue2 = {
  index = 39;
  name = tile_names.(0);
  classification = Property prop_darkblue2;
  owner = None;
  cost = 400;
  next = None;
  mortgage = 200;
  is_mortgaged = false;
}

let tax2 = {
  index = 38;
  name = tile_names.(1);
  classification = Tax;
  owner = None;
  cost = ~-100;
  next = Some darkblue2;
  mortgage = 0;
  is_mortgaged = false;
}


let darkblue1 = {
  index = 37;
  name = tile_names.(2);
  classification = Property prop_darkblue1;
  owner = None;
  cost = 350;
  next = Some tax2;
  mortgage = 180;
  is_mortgaged = false;
}

let chance3 = {
  index = 36;
  name = tile_names.(3);
  classification = Chance;
  owner = None;
  cost = 0;
  next = Some darkblue1;
  mortgage = 0;
  is_mortgaged = false;
}

let railroad4 = {
  index = 35;
  name = tile_names.(4);
  classification = Railroad;
  owner = None;
  cost = 200;
  next = Some chance3;
  mortgage = 100;
  is_mortgaged = false;
}

let green3 = {
  index = 34;
  name = tile_names.(5);
  classification = Property prop_green3;
  owner = None;
  cost = 320;
  next = Some railroad4;
  mortgage = 160;
  is_mortgaged = false;
}

let chest3 = {
  index = 33;
  name = tile_names.(6);
  classification = Chest;
  owner = None;
  cost = 0;
  next = Some green3;
  mortgage = 0;
  is_mortgaged = false;
}

let green2 = {
  index = 32;
  name = tile_names.(7);
  classification = Property prop_green2;
  owner = None;
  cost = 300;
  next = Some chest3;
  mortgage = 150;
  is_mortgaged = false;
}

let green1 = {
  index = 31;
  name = tile_names.(8);
  classification = Property prop_green1;
  owner = None;
  cost = 300;
  next = Some green2;
  mortgage = 150;
  is_mortgaged = false;
}

let to_jail = {
  index = 30;
  name = tile_names.(9);
  classification = To_Jail;
  owner = None;
  cost = 0;
  next = Some green1;
  mortgage = 0;
  is_mortgaged = false;
}

let yellow3 = {
  index = 29;
  name = tile_names.(10);
  classification = Property prop_yellow3;
  owner = None;
  cost = 280;
  next = Some to_jail;
  mortgage = 140;
  is_mortgaged = false;
}

let utility2 = {
  index = 28;
  name = tile_names.(11);
  classification = Utility;
  owner = None;
  cost = 150;
  next = Some yellow3;
  mortgage = 75;
  is_mortgaged = false;
}

let yellow2 = {
  index = 27;
  name = tile_names.(12);
  classification = Property prop_yellow2;
  owner = None;
  cost = 260;
  next = Some utility2;
  mortgage = 130;
  is_mortgaged = false;
}

let yellow1 = {
  index = 26;
  name = tile_names.(13);
  classification = Property prop_yellow1;
  owner = None;
  cost = 260;
  next = Some yellow2;
  mortgage = 130;
  is_mortgaged = false;
}

let railroad3 = {
  index = 25;
  name = tile_names.(14);
  classification = Railroad;
  owner = None;
  cost = 200;
  next = Some yellow1;
  mortgage = 100;
  is_mortgaged = false;
}

let red3 = {
  index = 24;
  name = tile_names.(15);
  classification = Property prop_red3;
  owner = None;
  cost = 240;
  next = Some railroad3;
  mortgage = 120;
  is_mortgaged = false;
}

let red2 = {
  index = 23;
  name = tile_names.(16);
  classification = Property prop_red2;
  owner = None;
  cost = 220;
  next = Some red3;
  mortgage = 110;
  is_mortgaged = false;
}

let chance2 = {
  index = 22;
  name = tile_names.(17);
  classification = Chance;
  owner = None;
  cost = 0;
  next = Some red2;
  mortgage = 0;
  is_mortgaged = false;
}

let red1 = {
  index = 21;
  name = tile_names.(18);
  classification = Property prop_red1;
  owner = None;
  cost = 220;
  next = Some chance2;
  mortgage = 110;
  is_mortgaged = false;
}

let free_parking = {
  index = 20;
  name = tile_names.(19);
  classification = Free;
  owner = None;
  cost = 0;
  next = Some red1;
  mortgage = 0;
  is_mortgaged = false;
}

let orange3 = {
  index = 19;
  name = tile_names.(20);
  classification = Property prop_orange3;
  owner = None;
  cost = 200;
  next = Some free_parking;
  mortgage = 100;
  is_mortgaged = false;
}

let orange2 = {
  index = 18;
  name = tile_names.(21);
  classification = Property prop_orange2;
  owner = None;
  cost = 180;
  next = Some orange3;
  mortgage = 90;
  is_mortgaged = false;
}

let chest2 = {
  index = 17;
  name = tile_names.(22);
  classification = Chest;
  owner = None;
  cost = 0;
  next = Some orange2;
  mortgage = 0;
  is_mortgaged = false;
}

let orange1 = {
  index = 16;
  name = tile_names.(23);
  classification = Property prop_orange1;
  owner = None;
  cost = 180;
  next = Some chest2;
  mortgage = 90;
  is_mortgaged = false;
}

let railroad2 = {
  index = 15;
  name = tile_names.(24);
  classification = Railroad;
  owner = None;
  cost = 200;
  next = Some orange1;
  mortgage = 100;
  is_mortgaged = false;
}

let magenta3 = {
  index = 14;
  name = tile_names.(25);
  classification = Property prop_magenta3;
  owner = None;
  cost = 160;
  next = Some railroad2;
  mortgage = 80;
  is_mortgaged = false;
}

let magenta2 = {
  index = 13;
  name = tile_names.(26);
  classification = Property prop_magenta2;
  owner = None;
  cost = 140;
  next = Some magenta3;
  mortgage = 70;
  is_mortgaged = false;
}

let utility1 = {
  index = 12;
  name = tile_names.(27);
  classification = Utility;
  owner = None;
  cost = 150;
  next = Some magenta2;
  mortgage = 75;
  is_mortgaged = false;
}

let magenta1 = {
  index = 11;
  name = tile_names.(28);
  classification = Property prop_magenta1;
  owner = None;
  cost = 140;
  next = Some utility1;
  mortgage = 70;
  is_mortgaged = false;
}

let jail = {
  index = 10;
  name = tile_names.(29);
  classification = Jail;
  owner = None;
  cost = 0;
  next = Some magenta1;
  mortgage = 0;
  is_mortgaged = false;
}

let lightblue3 = {
  index = 9;
  name = tile_names.(30);
  classification = Property prop_lightblue3;
  owner = None;
  cost = 120;
  next = Some jail ;
  mortgage = 60;
  is_mortgaged = false;
}

let lightblue2 = {
  index = 8;
  name = tile_names.(31);
  classification = Property prop_lightblue2;
  owner = None;
  cost = 100;
  next = Some lightblue3;
  mortgage = 50;
  is_mortgaged = false;
}

let chance1 = {
  index = 7;
  name = tile_names.(32);
  classification = Chance;
  owner = None;
  cost = 0;
  next = Some lightblue2;
  mortgage = 0;
  is_mortgaged = false;
}

let lightblue1 = {
  index = 6;
  name = tile_names.(33);
  classification = Property prop_lightblue1;
  owner = None;
  cost = 100;
  next = Some chance1;
  mortgage = 50;
  is_mortgaged = false;
}

let railroad1 = {
  index = 5;
  name = tile_names.(34);
  classification = Railroad;
  owner = None;
  cost = 200;
  next = Some lightblue1;
  mortgage = 100;
  is_mortgaged = false;
}

let tax1 = {
  index = 4;
  name = tile_names.(35);
  classification = Tax;
  owner = None;
  cost = ~-200;
  next = Some railroad1;
  mortgage = 0;
  is_mortgaged = false;
}

let brown2 = {
  index = 3;
  name = tile_names.(36);
  classification = Property prop_brown2;
  owner = None;
  cost = 60;
  next = Some tax1;
  mortgage = 30;
  is_mortgaged = false;
}

let chest1 = {
  index = 2;
  name = tile_names.(37);
  classification = Chest;
  owner = None;
  cost = 0;
  next = Some brown2;
  mortgage = 0;
  is_mortgaged = false;
}

let brown1 = {
  index = 1;
  name  = tile_names.(38);
  classification = Property prop_brown1;
  owner = None;
  cost = 60;
  next = Some chest1;
  mortgage = 30;
  is_mortgaged = false;
}

let go = {
  index = 0;
  name = tile_names.(39);
  classification = Go;
  owner = None;
  cost = 0;
  next = Some brown1;
  mortgage = 0;
  is_mortgaged = false;
};;

let board = {
  head = go;
  chance_deck = chance_deck;
  community_chest_deck = community_chest_deck;
}

let rec get_tile_by_name name tile = 
  let name_uppercase = String.uppercase_ascii name in
  match tile with 
  | Some t -> 
    begin
      if t.name = name_uppercase then t 
      else get_tile_by_name name t.next
    end
  | None -> failwith "this tile doesn't exist"

let name_of_tile t = t.name