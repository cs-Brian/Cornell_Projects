open Random

let rec roll num sides = 
  Random.self_init () ;
  match num with 
  | 0 -> []
  | n -> (Random.int sides) + 1 :: roll (n - 1) sides

let player_rolls p_list num sides =
  List.map (fun p -> (p, List.fold_left ( + ) 0 (roll num sides))) p_list

let rec string_of_player_rolls p_rolls = 
  match p_rolls with 
  | [] -> ""
  | h :: t -> 
    (fst h) ^ " rolled a total of " ^ string_of_int (snd h) ^ "!\n" 
    ^ (string_of_player_rolls t)

(* [compare_roll] returns 0 if the player-roll pairs [roll1] and [roll2] have
   the same roll, -1 if [roll1] has a greater roll than [roll2], and 1 if 
   [roll1] has a smaller roll than roll2.
   A roll is an ordered pair with the first element being a string representing
   the player's name, and the second element being an int representing the 
   (sum of that) player's roll. *)
let compare_roll roll1 roll2 = 
  if snd roll1 > snd roll2 then -1 
  else if snd roll1 < snd roll2 then 1
  else 0

let sort_by_roll p_rolls = 
  List.sort compare_roll p_rolls

let rec string_of_ordering sorted_rolls = 
  match sorted_rolls with 
  | [] -> ""
  | h :: t -> 
    if t = [] then (fst h) ^ ". " ^ string_of_ordering t
    else (fst h) ^ ", " ^ string_of_ordering t

let shuffle lst =
  Random.self_init () ;
  let rand_pairs = List.map (fun elem -> (Random.bits (), elem)) lst in
  let sorted_by_rand = List.sort compare rand_pairs in
  List.map snd sorted_by_rand

let is_all_same dice_roll = 
  let uniques = List.filter (fun d -> d <> (List.hd dice_roll)) dice_roll in 
  (List.length uniques) = 0