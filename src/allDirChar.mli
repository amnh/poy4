module IntSet :
  sig
    type elt = int
    type t = All_sets.Integers.t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
val debug_profile_memory : bool
val current_snapshot : string -> unit
module M :
  sig
    type a = AllDirNode.AllDirF.n
    type b = AllDirNode.OneDirF.n
    type phylogeny = (a, b) Ptree.p_tree
    val ( --> ) : 'a -> ('a -> 'b) -> 'b
    val force_node : AllDirNode.node_dir -> Node.node_data
    val create_lazy_node :
      (AllDirNode.node_data, 'a) Ptree.p_tree ->
      int -> int -> int -> AllDirNode.node_dir
    val create_lazy_edge :
      bool ->
      (AllDirNode.node_data, 'a) Ptree.p_tree ->
      int -> int -> Node.Standard.n AllDirNode.my_lazy
    val create_lazy_interior_down :
      (AllDirNode.node_data, 'a) Ptree.p_tree ->
      int -> int -> int -> AllDirNode.node_data
    val create_lazy_interior_up :
      (AllDirNode.node_data, 'a) Ptree.p_tree ->
      int -> int -> int -> int -> AllDirNode.node_data
    val create_root : int -> int -> phylogeny -> a Ptree.root
    val adjusted_node_distance :
      int -> int -> (AllDirNode.node_data, 'a) Ptree.p_tree -> float
    val parser_tree_with_lengths :
      (AllDirNode.node_data, 'a) Ptree.p_tree -> string Parser.Tree.t
    val list_of_costs : (AllDirNode.node_data, 'a) Ptree.p_tree -> float list
    val check_cost : (AllDirNode.AllDirF.n, 'a) Ptree.p_tree -> int -> float
    val check_cost_all_handles :
      (AllDirNode.AllDirF.n, 'a) Ptree.p_tree -> float
    val check_assertion_two_nbrs : int -> Tree.node -> string -> bool
    val get_pre_active_ref_code :
      (AllDirNode.node_data, 'a) Ptree.p_tree -> IntSet.t
    val assign_single :
      (AllDirNode.AllDirF.n, 'a) Ptree.p_tree ->
      (AllDirNode.AllDirF.n, 'a) Ptree.p_tree
    val unadjust : 'a -> 'a
    val get_single : int -> AllDirNode.AllDirF.n -> Node.node_data
    val get_unadjusted : int -> AllDirNode.AllDirF.n -> Node.node_data
    val get_active_ref_code :
      (AllDirNode.AllDirF.n, 'a) Ptree.p_tree -> IntSet.t * IntSet.t
    val adjust_tree :
      ?ignore_initial_cost:bool ->
      int option ->
      All_sets.Integers.t option All_sets.IntegerMap.t option ->
      (AllDirNode.AllDirF.n, 'a) Ptree.p_tree ->
      (AllDirNode.AllDirF.n, 'a) Ptree.p_tree
    val assign_single_and_readjust :
      int option ->
      (AllDirNode.AllDirF.n, 'a) Ptree.p_tree ->
      (AllDirNode.AllDirF.n, 'a) Ptree.p_tree
    val refresh_all_edges :
      bool ->
      (AllDirNode.node_data, 'a) Ptree.p_tree ->
      (AllDirNode.node_data, Node.Standard.n AllDirNode.my_lazy) Ptree.p_tree
    val refresh_roots : phylogeny -> (a, b) Ptree.p_tree
    val internal_downpass : bool -> phylogeny -> phylogeny
    val clear_internals : phylogeny -> phylogeny
    val blindly_trust_downpass :
      (AllDirNode.node_data, AllDirNode.OneDirF.n) Ptree.p_tree ->
      'a * int ->
      float *
      (AllDirNode.node_data, AllDirNode.OneDirF.n) Ptree.p_tree Lazy.t ->
      Tree.edge ->
      float *
      (AllDirNode.node_data, AllDirNode.OneDirF.n) Ptree.p_tree Lazy.t
    val blindly_trust_single :
      (AllDirNode.AllDirF.n, AllDirNode.OneDirF.n) Ptree.p_tree ->
      'a * int ->
      float *
      (AllDirNode.AllDirF.n, AllDirNode.OneDirF.n) Ptree.p_tree lazy_t ->
      Tree.edge ->
      float *
      (AllDirNode.AllDirF.n, AllDirNode.OneDirF.n) Ptree.p_tree lazy_t
    val blindly_trust_adjusted :
      (AllDirNode.AllDirF.n, AllDirNode.OneDirF.n) Ptree.p_tree ->
      'a * int ->
      float *
      (AllDirNode.AllDirF.n, AllDirNode.OneDirF.n) Ptree.p_tree lazy_t ->
      Tree.edge ->
      float *
      (AllDirNode.AllDirF.n, AllDirNode.OneDirF.n) Ptree.p_tree lazy_t
    val general_pick_best_root :
      (('a, 'b) Ptree.p_tree ->
       Ptree.edge list * All_sets.Integers.elt ->
       float * ('a, 'b) Ptree.p_tree Lazy.t ->
       Ptree.edge -> float * ('a, 'b) Ptree.p_tree Lazy.t) ->
      ('a, 'b) Ptree.p_tree -> ('a, 'b) Ptree.p_tree
    val pick_best_root :
      (AllDirNode.node_data, AllDirNode.OneDirF.n) Ptree.p_tree ->
      (AllDirNode.node_data, AllDirNode.OneDirF.n) Ptree.p_tree
    val downpass : phylogeny -> phylogeny
    val uppass :
      (AllDirNode.AllDirF.n, AllDirNode.OneDirF.n) Ptree.p_tree ->
      (AllDirNode.AllDirF.n, AllDirNode.OneDirF.n) Ptree.p_tree
    val create_edge :
      (AllDirNode.node_data, Node.Standard.n AllDirNode.my_lazy) Ptree.p_tree ->
      int ->
      int ->
      (AllDirNode.node_data, Node.Standard.n AllDirNode.my_lazy) Ptree.p_tree
    val clear_vertex :
      int ->
      int ->
      int ->
      int ->
      (AllDirNode.node_data, 'a) Ptree.p_tree ->
      (AllDirNode.node_data, 'a) Ptree.p_tree
    val debug_clear_subtree : bool
    val clear_subtree :
      int ->
      int ->
      (AllDirNode.node_data, 'a) Ptree.p_tree ->
      (AllDirNode.node_data, 'a) Ptree.p_tree
    val clear_up_over_edge :
      int * int ->
      (AllDirNode.node_data, 'a) Ptree.p_tree ->
      (AllDirNode.node_data, 'a) Ptree.p_tree
    val set_clade_root :
      phylogeny ->
      int ->
      AllDirNode.AllDirF.n -> int -> (a, AllDirNode.a_node) Ptree.p_tree
    val clean_ex_neighbor :
      int ->
      int ->
      (AllDirNode.node_data, 'a) Ptree.p_tree ->
      (AllDirNode.node_data, 'a) Ptree.p_tree
    val get_edge_n_force :
      int -> int -> ('a, 'b AllDirNode.my_lazy) Ptree.p_tree -> 'b
    val replace_topology :
      Tree.u_tree -> ('a, 'b) Ptree.p_tree -> ('a, 'b) Ptree.p_tree
    val add_component_root :
      ('a, 'b) Ptree.p_tree ->
      All_sets.IntegerMap.key -> 'a Ptree.root -> ('a, 'b) Ptree.p_tree
    val get_other_neighbors :
      All_sets.IntegerMap.key * All_sets.IntegerMap.key ->
      ('a, 'b) Ptree.p_tree ->
      'c option All_sets.IntegerMap.t option ->
      'c option All_sets.IntegerMap.t option
    val break_fn : int * int -> phylogeny -> (a, b) Ptree.breakage
    val debug_join_fn : bool
    val get_one : [< `Edge of 'a * 'b * 'c * 'd | `Single of 'a * 'e ] -> 'a
    val join_fn :
      'a ->
      Tree.join_jxn ->
      Tree.join_jxn ->
      (a, 'b) Ptree.p_tree ->
      (a, AllDirNode.OneDirF.n) Ptree.p_tree * Tree.join_delta
    type tmp = Edge of (int * int) | Clade of a
    val cost_fn :
      Tree.join_jxn ->
      Tree.join_jxn -> 'a -> a -> phylogeny -> Ptree.clade_cost
    val reroot_fn :
      bool ->
      Tree.edge -> (a, b) Ptree.p_tree -> (a, b) Ptree.p_tree * 'a list
    val root_costs :
      ('a, AllDirNode.OneDirF.n) Ptree.p_tree ->
      (Tree.EdgeMap.key * float) list
    val string_of_node : 'a -> string
    val features :
      Methods.local_optimum ->
      (string * string) list -> (string * string) list
    val incremental_uppass : 'a -> 'b -> 'a
    val assign_final_states :
      (AllDirNode.AllDirF.n, 'a) Ptree.p_tree ->
      (AllDirNode.AllDirF.n, 'a) Ptree.p_tree
    val to_formatter :
      Tags.attributes ->
      Data.d -> (AllDirNode.AllDirF.n, b) Ptree.p_tree -> Tags.xml
  end
module F :
  sig
    type a = AllDirNode.AllDirF.n
    type b = AllDirNode.OneDirF.n
    val break_fn : (a, b) Ptree.break_fn
    val join_fn : (a, b) Ptree.join_fn
    val cost_fn : (a, b) Ptree.cost_fn
    val reroot_fn : (a, b) Ptree.reroot_fn
    val string_of_node : a -> string
    val features :
      Methods.local_optimum ->
      (string * string) list -> (string * string) list
    val clear_internals : (a, b) Ptree.p_tree -> (a, b) Ptree.p_tree
    val downpass : (a, b) Ptree.p_tree -> (a, b) Ptree.p_tree
    val uppass : (a, b) Ptree.p_tree -> (a, b) Ptree.p_tree
    val incremental_uppass :
      (a, b) Ptree.p_tree -> Ptree.incremental list -> (a, b) Ptree.p_tree
    val to_formatter :
      Tags.attributes -> Data.d -> (a, b) Ptree.p_tree -> Tags.xml
    val root_costs : (a, b) Ptree.p_tree -> (Tree.edge * float) list
    val unadjust : (a, b) Ptree.p_tree -> (a, b) Ptree.p_tree
  end
module CharScripting :
  sig
    type cs = CharacterScripting.Standard.cs
    type n = AllDirNode.AllDirF.n
    type character_input_output =
        [ `Characters of cs Sexpr.t | `Floats of float Sexpr.t ]
    val distance : float -> cs -> cs -> float
    val median : cs -> cs -> cs
    val scriptchar_operations :
      n list ->
      Data.d ->
      [ `Distance of
          (CharacterScripting.characters * CharacterScripting.characters) *
          CharacterScripting.characters
      | `Median of
          (CharacterScripting.characters * CharacterScripting.characters) *
          CharacterScripting.characters ] ->
      character_input_output
    val filter_char_operations :
      n list ->
      Data.d ->
      CharacterScripting.taxa * CharacterScripting.taxa ->
      CharacterScripting.characters -> (cs Sexpr.t * cs Sexpr.t) list
    val extract_character : int -> n -> cs
    val character_operations :
      [ `Distance of cs Sexpr.t * cs Sexpr.t
      | `Median of cs Sexpr.t * cs Sexpr.t ] ->
      [ `Characters of cs Sexpr.t | `Floats of float Sexpr.t ]
  end
