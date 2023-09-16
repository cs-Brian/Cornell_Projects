(** Error handling on user input. *)

(** [validate_int s] traps the user until they have entered a string [s] that 
    can be converted to an integer. Returns the validated string, converted to 
    an int. *)
val validate_int : string -> int

(** [validate_name name] traps the user until they have entered a valid player 
    name [name]. Returns the valid name. *)
val validate_name : string -> string

(** [validate_non_duplicate_name name_list name] traps the user until they have
    entered a player name that is not in [name_list]. Returns the validated 
    name. *)
val validate_non_duplicate_name : string list -> string -> string

(** [validate_response response_list response] traps the user until they have
    entered a valid response. A valid response is one that exists in
    [response_list]. Returns the validated response. *)
val validate_response : string list -> string -> string