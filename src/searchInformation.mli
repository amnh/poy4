val handle_tree_information :
  ('a, 'b) Ptree.p_tree Sexpr.t ->
  string -> [< `Maximum | `Minimum | `Number | `Summary ] -> string
val append : string -> string -> string
val with_commas : bool -> string -> string list -> string
val handle_taxa_information : Data.d -> string -> string
val handle_character_information : Data.d -> string -> [< `Type ] -> string
val handle_file_information : Data.d -> string -> [< `Filename ] -> string
val show_information :
  ('a, 'b) Ptree.p_tree Sexpr.t option ->
  Data.d option ->
  string option -> Data.OutputInformation.t list option -> string
