type ft_queue = {
  queue : (Tree.u_tree * float * Ptree.clade_cost) list Queue.t;
  stack : (Tree.u_tree * float * Ptree.clade_cost) list Stack.t;
}
val create : unit -> ft_queue
class type ['a, 'b] search_manager_sampler =
  object
    method any_trees : bool -> unit
    method clone : ('a, 'b) search_manager_sampler
    method evaluate : (('a, 'b) Ptree.p_tree * float) list -> unit
    method init :
      (('a, 'b) Ptree.p_tree * float * Ptree.clade_cost) list -> unit
    method next_tree : ('a, 'b) Ptree.p_tree * float -> unit
    method process :
      Ptree.incremental list ->
      Tree.join_jxn ->
      Tree.join_jxn ->
      'a ->
      ('a, 'b) Ptree.p_tree ->
      ('a, 'b) Ptree.p_tree option -> float -> float -> float option -> unit
    method results : (('a, 'b) Ptree.p_tree * float) list -> unit
  end
class ['a, 'b] do_nothing : ['a, 'b] search_manager_sampler
class ['a, 'b] composer :
  ('a, 'b) search_manager_sampler ->
  ('a, 'b) search_manager_sampler -> ['a, 'b] search_manager_sampler
module MakeApp :
  functor (Node : NodeSig.S) ->
    functor
      (Edge : sig
                type e
                type n = Node.n
                val has_information : bool
                val to_node : int -> int * int -> e -> n
                val of_node : int option -> n -> e
                val recode : (int -> int) -> e -> e
                val force : e -> e
              end) ->
      sig
        class print_next_tree :
          (Tree.u_tree * float * unit -> unit) ->
          [Node.n, Edge.e] search_manager_sampler
        class ['a, 'b] general_local_optimum_holder :
          ((Tree.u_tree * float * Ptree.clade_cost) list -> unit) ->
          (unit -> unit) -> ['a, 'b] search_manager_sampler
        class ['a, 'b] local_optimum_holder :
          ft_queue ->
          object
            constraint 'a = Node.n
            constraint 'b = Edge.e
            method any_trees : bool -> unit
            method clone : ('a, 'b) search_manager_sampler
            method evaluate : (('a, 'b) Ptree.p_tree * float) list -> unit
            method init :
              (('a, 'b) Ptree.p_tree * float * Ptree.clade_cost) list -> unit
            method next_tree : ('a, 'b) Ptree.p_tree * float -> unit
            method process :
              Ptree.incremental list ->
              Tree.join_jxn ->
              Tree.join_jxn ->
              'a ->
              ('a, 'b) Ptree.p_tree ->
              ('a, 'b) Ptree.p_tree option ->
              float -> float -> float option -> unit
            method results : (('a, 'b) Ptree.p_tree * float) list -> unit
          end
        class ['a, 'b] visited :
          (Ptree.incremental list ->
           Tree.join_jxn ->
           Tree.join_jxn -> ('a, 'b) Ptree.p_tree -> ('c, 'd) Ptree.p_tree) ->
          (Tree.u_tree * float * unit -> unit) ->
          ['a, 'b] search_manager_sampler
        class virtual timed_trajectory :
          float ->
          object
            val mutable timer : Timer.t
            method any_trees : bool -> unit
            method clone : (Node.n, Edge.e) search_manager_sampler
            method evaluate :
              ((Node.n, Edge.e) Ptree.p_tree * float) list -> unit
            method init :
              ((Node.n, Edge.e) Ptree.p_tree * float * Ptree.clade_cost) list ->
              unit
            method next_tree : (Node.n, Edge.e) Ptree.p_tree * float -> unit
            method process :
              Ptree.incremental list ->
              Tree.join_jxn ->
              Tree.join_jxn ->
              Node.n ->
              (Node.n, Edge.e) Ptree.p_tree ->
              (Node.n, Edge.e) Ptree.p_tree option ->
              float -> float -> float option -> unit
            method results :
              ((Node.n, Edge.e) Ptree.p_tree * float) list -> unit
            method private virtual timed_operation :
              (Node.n, Edge.e) Ptree.p_tree * float -> unit
          end
        class timed_printout :
          ft_queue ->
          float ->
          (Tree.u_tree * float * Ptree.clade_cost -> unit) ->
          [Node.n, Edge.e] search_manager_sampler
        class ['a] timed_cancellation :
          [< `Dynamic of unit -> float | `Fixed of float ] ->
          [Node.n, Edge.e] search_manager_sampler
          class counted_cancellation : int -> [Node.n, Edge.e]
          search_manager_sampler
      end
module MakeRes :
  functor (Node : NodeSig.S) ->
    functor
      (Edge : sig
                type e
                type n = Node.n
                val has_information : bool
                val to_node : int -> int * int -> e -> n
                val of_node : int option -> n -> e
                val recode : (int -> int) -> e -> e
                val force : e -> e
              end) ->
      sig
        val proportion : int -> int -> float
        val gap_saturation : Sequence.s -> float
        val count_bits : int -> int
        val poly_saturation : Sequence.s -> int -> float
        val total_saturation : Sequence.s -> float
        type union_node = {
          rooted : Node.Union.u;
          self : Node.Union.u;
          depth : int;
        }
        val union_depth :
          (Node.n, 'a) Ptree.p_tree -> int -> (union_node, 'b) Ptree.p_tree
        val union_distance :
          int option ->
          Node.n -> (union_node, 'a) Ptree.p_tree -> int -> float
        val is_handle_or_other_handle : int -> ('a, 'b) Ptree.p_tree -> bool
        val cluster_union :
          int -> (union_node, 'a) Ptree.p_tree -> int -> int
        val union_distance_cluster :
          int option ->
          Node.n -> int -> (union_node, 'a) Ptree.p_tree -> int -> float
        val get_all_sequences_data : Node.n -> Sequence.s list
        val get_sequence_data : Node.n -> Sequence.s
        val preliminary_gap_saturation : Node.n -> string
        val preliminary_poly_saturation : Node.n -> string
        class ['a] union_table :
          int ->
          (string -> unit) ->
          object
            val mutable current_tree : 'b option
            val results : string array
            val mutable union_tree : (union_node, 'c) Ptree.p_tree option
            method any_trees : bool -> unit
            method clone : (Node.n, 'a) search_manager_sampler
            method evaluate :
              ((Node.n, 'a) Ptree.p_tree * float) list -> unit
            method init :
              ((Node.n, 'a) Ptree.p_tree * float * Ptree.clade_cost) list ->
              unit
            method next_tree : (Node.n, 'a) Ptree.p_tree * float -> unit
            method process :
              Ptree.incremental list ->
              Tree.join_jxn ->
              Tree.join_jxn ->
              Node.n ->
              (Node.n, 'a) Ptree.p_tree ->
              (Node.n, 'a) Ptree.p_tree option ->
              float -> float -> float option -> unit
            method results : ((Node.n, 'a) Ptree.p_tree * float) list -> unit
            method private update_tree :
              (Node.n, 'a) Ptree.p_tree -> (union_node, 'c) Ptree.p_tree
          end
        class ['a] tests_before_next :
          (string -> unit) ->
          object
            val mutable counter : int
            method any_trees : bool -> unit
            method clone : (Node.n, 'a) search_manager_sampler
            method evaluate :
              ((Node.n, 'a) Ptree.p_tree * float) list -> unit
            method init :
              ((Node.n, 'a) Ptree.p_tree * float * Ptree.clade_cost) list ->
              unit
            method next_tree : (Node.n, 'a) Ptree.p_tree * float -> unit
            method process :
              Ptree.incremental list ->
              Tree.join_jxn ->
              Tree.join_jxn ->
              Node.n ->
              (Node.n, 'a) Ptree.p_tree ->
              (Node.n, 'a) Ptree.p_tree option ->
              float -> float -> float option -> unit
            method results : ((Node.n, 'a) Ptree.p_tree * float) list -> unit
          end
        class ['a] break_n_join_distances :
          (Node.n, 'a) Ptree.join_fn ->
          (string -> unit) ->
          object
            val mutable cost : string
            method any_trees : bool -> unit
            method clone : (Node.n, 'a) search_manager_sampler
            method evaluate :
              ((Node.n, 'a) Ptree.p_tree * float) list -> unit
            method init :
              ((Node.n, 'a) Ptree.p_tree * float * Ptree.clade_cost) list ->
              unit
            method next_tree : (Node.n, 'a) Ptree.p_tree * float -> unit
            method process :
              Ptree.incremental list ->
              Tree.join_jxn ->
              Tree.join_jxn ->
              Node.n ->
              (Node.n, 'a) Ptree.p_tree ->
              (Node.n, 'a) Ptree.p_tree option ->
              float -> float -> float option -> unit
            method results : ((Node.n, 'a) Ptree.p_tree * float) list -> unit
          end
      end
