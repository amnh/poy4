val ndebug : bool
type xml_tree = Leaf of string | Node of string * xml_tree list
val print_points : int * int * string -> unit
val sort_tree : 'a Parser.Tree.t -> 'a Parser.Tree.t
val to_matrix :
  ?sep:int -> ?bd:int -> bool -> string Parser.Tree.t -> char array array
val draw :
  ?sep:int -> ?bd:int -> bool -> out_channel -> string Parser.Tree.t -> unit
val to_string : ?sep:int -> ?bd:int -> bool -> string Parser.Tree.t -> string
val draw_parenthesis : out_channel -> string Parser.Tree.t -> unit
val for_formatter :
  ?separator:string -> bool -> bool -> bool -> string Parser.Tree.t -> string
