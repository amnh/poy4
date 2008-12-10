exception Illegal_formater of string
val is_xml_filename : string option -> bool
val sort_matrix : String.t array array -> unit
val build_contents_row : Tags.xml -> string array
val build_names_row :
  ?contents_name:string ->
  'a * (string * [> `String of string ]) list * 'b -> string array
type t = Tags.xml Tags.contents
val build_set : t -> string
val build_table_with_contents_as_set :
  ?cn:Tags.tag -> Tags.xml list -> string array array
val character_type_as_attribute :
  string * (string * string) list * 'a ->
  string * (string * string) list * 'a
val filter_tag : Tags.tag -> Tags.xml -> Tags.xml list
val build_values_as_list :
  Status.c ->
  ('b * 'c *
   [< `Bool of bool
    | `Delayed of unit -> 'a Sexpr.t
    | `Empty
    | `Float of float
    | `FloatFloatTuple of float * float
    | `Fun of unit -> string
    | `Int of int
    | `IntFloatTuple of int * float
    | `IntTuple of int * int
    | `Set of 'a Sexpr.t list
    | `Single of 'a
    | `String of string ]
   as 'a) ->
  unit
val output_rows : Status.c -> 'a array -> unit
val output_characters : Status.c -> Tags.xml -> unit
val output_taxa :
  Status.c ->
  'a * 'b *
  [< `Bool of bool
   | `Delayed of unit -> Tags.xml Sexpr.t
   | `Empty
   | `Float of float
   | `FloatFloatTuple of float * float
   | `Fun of unit -> string
   | `Int of int
   | `IntFloatTuple of int * float
   | `IntTuple of int * int
   | `Set of Tags.xml Sexpr.t list
   | `Single of Tags.xml
   | `String of string ] ->
  unit
val output_list :
  string ->
  Status.c ->
  ('b * 'c *
   [< `Bool of bool
    | `Delayed of unit -> 'a Sexpr.t
    | `Empty
    | `Float of float
    | `FloatFloatTuple of float * float
    | `Fun of unit -> string
    | `Int of int
    | `IntFloatTuple of int * float
    | `IntTuple of int * int
    | `Set of 'a Sexpr.t list
    | `Single of 'a
    | `String of string ]
   as 'a) ->
  unit
val format_attributes :
  Status.c ->
  (string *
   [< `Bool of bool
    | `Float of float
    | `FloatFloatTuple of float * float
    | `Fun of unit -> string
    | `Int of int
    | `IntFloatTuple of int * float
    | `IntTuple of int * int
    | `String of string ])
  list -> unit
val aux_data_to_status : Status.c -> Tags.xml -> unit
val data_to_status : string option -> Tags.xml -> unit
val get_name_class_and_cost : (string * 'a) list -> 'a * 'a * 'a
val find_item : 'a -> ('a * 'b) list -> 'b
val get_recost : (string * 'a) list -> 'a
val get_ref_code : (string * 'a) list -> 'a
val get_map : 'a -> [> `String of string ]
val min_and_max :
  string option * string option ->
  string * 'a *
  [< `Bool of bool
   | `Delayed of unit -> 'b Sexpr.t
   | `Empty
   | `Float of float
   | `FloatFloatTuple of float * float
   | `Fun of unit -> string
   | `Int of int
   | `IntFloatTuple of int * float
   | `IntTuple of int * int
   | `Set of 'b Sexpr.t list
   | `Single of 'b
   | `String of string ] ->
  string option * string option
val addcs_to_formater : Tags.xml -> Tags.unstructured array
val nonaddcs_to_formater : Tags.xml -> Tags.unstructured array
val sankcs_to_formater : Tags.xml -> Tags.unstructured array
val seq_to_formater : Tags.xml -> Tags.unstructured array
val breakinv_to_formater : Tags.xml -> Tags.unstructured array
val chrom_to_formater : Tags.xml -> Tags.unstructured array
val genome_to_formater : Tags.xml -> Tags.unstructured array
val annchrom_to_formater : Tags.xml -> Tags.unstructured array
val node_character_to_formater : Tags.xml -> Tags.unstructured array
val node_to_formater : Status.c -> Tags.xml -> unit
val forest_to_formater : Status.c -> Tags.xml -> unit
val trees_to_formater : 
    string option -> Status.formatter_output list -> Tags.xml -> unit
