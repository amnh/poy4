val index : (string * string) list
val find_with_regexp : Str.regexp -> string * string -> bool
val output_help_item : 'a * string -> unit
val help : string option -> unit
val help_if_exists : string -> unit
