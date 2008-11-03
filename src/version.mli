val truere : Str.regexp
val get_option : (string * 'a) list -> string -> 'a
val get_graphics : string -> string
val get_interface : string -> string
val is_true : string -> string
val rephrase : string -> string
val name : string
val major_version : int
val minor_version : int
val release_version : int
val patch_version : string
type release_options = Development | Candidate of int | Official
val release_option : release_options
val ( --> ) : 'a -> ('a -> 'b) -> 'b
val append : string -> string -> string
val if_run : bool -> ('a -> 'b -> 'b) -> 'a -> 'b -> 'b
val option_to_string : string -> string
val small_version_string : string
val version_string : string
val string : string
