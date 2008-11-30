type direction = Without of int | Median of (int * int)
module type S =
  sig
    type n
    type other_n
    val fix_preliminary : n -> n
    val distance :
      ?para:int option -> ?parb:int option -> float -> n -> n -> float
    val median : int option -> int option -> n option -> n -> n -> n
    val median_3 : int option -> n -> n -> n -> n -> n
    val to_string : n -> string
    val total_cost : int option -> n -> float
    val node_cost : int option -> n -> float
    val update_leaf : n -> n
    val taxon_code : n -> int
    val union_distance : n -> n -> float
    val is_collapsable : [ `Any | `Dynamic | `Static ] -> n -> n -> bool
    val to_xml : Data.d -> out_channel -> n -> unit
    val num_height : int option -> n -> int
    val num_otus : int option -> n -> int
    val get_sequences :
      int option ->
      n ->
      (int * Sequence.s * Cost_matrix.Two_D.m * Cost_matrix.Three_D.m *
       Alphabet.a)
      list
    val get_dynamic_preliminary : int option -> n -> DynamicCS.t list
    val get_dynamic_adjusted : int option -> n -> DynamicCS.t list
    val edge_distance : n -> n -> float
    val support_chars : int -> int option -> n -> (float * int) list
    val load_data :
      ?silent:bool -> ?classify:bool -> Data.d -> Data.d * n list
    val n_chars : ?acc:int -> n -> int
    val prioritize : n -> n
    val reprioritize : n -> n -> n
    val f_codes : int list -> n -> n
    val min_child_code : int option -> n -> int
    type e
    type nad8
    val new_characters :
      int ->
      nad8 All_sets.IntegerMap.t ->
      (All_sets.IntegerMap.key * int array All_sets.IntegerMap.t list) list ->
      nad8 All_sets.IntegerMap.t
    val build_node :
      nad8 All_sets.IntegerMap.t -> All_sets.Integers.elt list -> n -> n
    val set_exclude_info : e -> n -> n
    val excludes_median : int option -> n -> n -> e
    val has_excluded : e -> bool
    module T : sig val add_exclude : All_sets.Integers.t -> n -> n end
    module Union :
      sig
        type u
        val union : int option -> n -> u -> u -> u
        val union_preliminary : int option -> u -> n -> u
        val union_final : int option -> u -> n -> u
        val leaf : int option -> int option -> n -> u
        val distance : u -> u -> float
        val saturation : u -> float
        val distance_node : int option -> n -> u -> float
        val compare : u -> u -> int
        val get_sequence : int option -> int -> u -> SeqCS.union_element
      end
    val compare : n -> n -> int
    val for_support : int -> (int * n) list -> int list -> int list -> n list
    val root_cost : n -> float
    val to_single : n option -> int option -> n -> int option -> n -> n
    val character_costs :
      int option -> n -> ([ `Add | `NonAdd | `Sank ] * int * float) list
    val get_nonadd_8 : int option -> n -> (NonaddCS8.t * NonaddCS8.t) list
    val get_nonadd_16 : int option -> n -> (NonaddCS16.t * NonaddCS16.t) list
    val get_nonadd_32 : int option -> n -> (NonaddCS32.t * NonaddCS32.t) list
    val get_add : int option -> n -> (AddCS.t * AddCS.t) list
    val get_sank : int option -> n -> (SankCS.t * SankCS.t) list
    val get_dynamic : int option -> n -> (DynamicCS.t * DynamicCS.t) list
    val recode : (int -> int) -> n -> n
    val to_other : n -> other_n
    val force : n -> n
  end
