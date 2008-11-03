val just_exit : bool ref
val only_run_argument_script : bool ref
val dump_file : string ref
val input : string list ref
val change_working_directory : string -> unit
val parse_list : (string * Arg.spec * string) list
val anon_fun : string -> unit
val usage : string
