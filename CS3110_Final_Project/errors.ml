(** [is_valid_int s] returns whether or not [s] is a string that can be
    converted to an integer that is positive. *)
let is_valid_int s = 
  let n = 
    try int_of_string s with 
    | Failure _ -> ~-1 in 
  n > 0

let validate_int s = 
  let s'  = ref s in 
  while (is_valid_int !s' = false) do 
    print_string "Please enter a valid number: ";
    s' := read_line ()
  done;
  int_of_string !s'

let tab_character_index = 9
let space_character_index = 32

(** [string_to_char_list s s_length lst] is [lst] but with the characters of 
    [s] appended to it (reversed), with s_length being the length of [s]. *)
let rec string_to_char_list s s_length lst =
  match s_length with 
  | -1 -> lst
  | n -> s.[n] :: string_to_char_list s (n - 1) lst

(** [is_all val_lst lst] is whether or not [lst] is the list containing only 
    values in [val_lst] as values. *)
let rec is_all val_lst lst = 
  match lst with 
  | [] -> true 
  | h :: t -> List.mem h val_lst && is_all val_lst t

(** [is_all_forbidden s] is whether or not [s] is the string containing only 
    'forbidden' characters, which are the invisible characters, like TAB and
    SPACE. *)
let is_all_forbidden s = 
  let char_list = string_to_char_list s ((String.length s) - 1) [] in 
  let forbidden_chars = 
  (*
  Add to this list if we find more 'invisible' characters that users can enter
  *)
    [
      Char.chr space_character_index ; 
      Char.chr tab_character_index
    ] in

  is_all forbidden_chars char_list

(** [is_valid_name p] is whether or not string [p] is a valid player 
    name. A valid player name is any string that is not 'invisible', i.e. any 
    string that is not all TAB keys or all SPACE keys. *)
let is_valid_name p = 
  (p = "") = false && is_all_forbidden p = false

let validate_name name = 
  let name' = ref name in 
  while (is_valid_name !name' = false) do 
    print_string "Please enter a valid name: ";
    name' := read_line ()
  done;
  !name'

let validate_non_duplicate_name name_list name = 
  let name' = ref name in 
  while (List.mem !name' name_list = true) do
    print_string 
      "A player already has this name. Please enter a different one: " ;
    name' := read_line ()
  done;
  !name'

let validate_response response_list response = 
  let response' = ref response in 
  while (List.mem !response' response_list = false) do
    print_string "Please enter a valid response: " ;
    response' := read_line ()
  done;
  !response'