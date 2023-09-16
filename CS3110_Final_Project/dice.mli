(** A module representing dice. *)

(** [roll num sides] rolls [num] [sides] sided die 
    and returns the a list of the values that each dice lands on.
    i.e. Dice.roll 2 6 rolls 2 6 sided die and may return [2 ; 2].
    This same function call could also return [1 ; 5]. *)
val roll : int -> int -> int list

(** [player_rolls p_list num sides] generates an association list, mapping each
    player name in [plist] to the sum of a dice roll of [num] die, each with 
    [sides] sides.
    Ex: plist = [["Miah", "Gio", "Brian", "Tayyaba"]], num = 2, sides = 6
    could result in 
    [[("Miah",[1;5]) ; ("Gio",[3;3]) ; ("Brian",[4;3]) ; ("Tayyaba",[5;6])]]
    which would return 
    [[("Miah", 6) ; ("Gio", 6) ; ("Brian", 7) ; ("Tayyaba", 11)]]. *)
val player_rolls : 'a list -> int -> int -> ('a * int) list

(** [string_of_player_rolls p_rolls] take [p_rolls] and stringifies each player 
    and the total the rolled in the format "<player-name> rolled a total of 
    <total-roll>!". *)
val string_of_player_rolls : (string * int) list -> string

(** [sort_by_roll p_rolls] takes in a list of player_rolls [p_rolls] and returns 
    that list, but sorted in descending order by each player's roll. 
    Ex: If [p_rolls] is 
    [[("Miah", 6) ; ("Gio", 6) ; ("Brian", 7) ; ("Tayyaba", 11)]], then this 
    could return (not necessarily a stable sort) 
    [[("Tayyaba", 11) ; ("Brian", 7) ; ("Miah", 6) ; ("Gio", 6)]]. *)
val sort_by_roll : ('a * 'b) list -> ('a * 'b) list

(** [string_of_ordering sorted_rolls] converts the names of the players in the 
    order in which they appear in [sorted_rolls] to a string. *)
val string_of_ordering : (string * 'a) list -> string

(** [shuffle lst] randomly shuffles the ordering of the elements in [lst].
    This function assigns each element in the list a random int by creating an
    association list mapping each element to a random value,
    sorts that association list based on the random int in each pair, and then
    returns a mapping from that sorted association list to the snd value in each
    association pair (i.e. discard the random ints). *)
val shuffle : 'a list -> 'a list

(** [is_all_same dice_roll] is whether or not all dice in the [dice_roll] list
    landed on the same number.
    Ex: [dice_roll] = [[4 ; 4]], [is_all_same dice_roll] is true
        [dice_roll] = [[6 ; 6 ; 2]], [is_all_same dice_roll] is false. 
    Requires: [dice_roll] is nonempty. *)
val is_all_same : int list -> bool