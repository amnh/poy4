exception Illegal_formater of string
val is_xml_filename : string option -> bool
val sort_matrix : String.t array array -> unit
val build_contents_row : Xml.xml -> string array
val build_names_row :
  ?contents_name:string ->
  'a * (string * [> `String of string ]) list * 'b -> string array
type t = Xml.xml Xml.contents
val build_set : t -> string
val build_table_with_contents_as_set :
  ?cn:Xml.tag -> Xml.xml list -> string array array
val character_type_as_attribute :
  string * (string * string) list * 'a ->
  string * (string * string) list * 'a
val filter_tag : Xml.tag -> Xml.xml -> Xml.xml list
val build_values_as_list :
  Status.c -> Xml.xml ->
  unit
val output_rows : Status.c -> 'a array -> unit
val output_characters : Status.c -> Xml.xml -> unit
val output_taxa :
  Status.c -> Xml.xml -> unit
val output_list :
  string ->
  Status.c -> Xml.xml -> unit
val format_attributes :
  Status.c -> Xml.attributes -> unit
val aux_data_to_status : Status.c -> Xml.xml -> unit
val data_to_status : string option -> Xml.xml -> unit
val get_name_class_and_cost : (string * 'a) list -> 'a * 'a * 'a
val find_item : 'a -> ('a * 'b) list -> 'b
val get_recost : (string * 'a) list -> 'a
val get_ref_code : (string * 'a) list -> 'a
val get_map : 'a -> [> `String of string ]
val min_and_max :
  string option * string option -> Xml.xml -> string option * string option
val addcs_to_formater : Xml.xml -> Xml.unstructured array
val nonaddcs_to_formater : Xml.xml -> Xml.unstructured array
val sankcs_to_formater : Xml.xml -> Xml.unstructured array
val seq_to_formater : Xml.xml -> Xml.unstructured array
val breakinv_to_formater : Xml.xml -> Xml.unstructured array
val chrom_to_formater : Xml.xml -> Xml.unstructured array
val genome_to_formater : Xml.xml -> Xml.unstructured array
val annchrom_to_formater : Xml.xml -> Xml.unstructured array
val node_character_to_formater : Xml.xml -> Xml.unstructured array
val node_to_formater : Status.c -> Xml.xml -> unit
val forest_to_formater : Status.c -> Xml.xml -> unit
val trees_to_formater : 
    string option -> Status.formatter_output list -> Xml.xml -> unit
