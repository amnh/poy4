val debug : bool
val union_size : int
module type S =
  sig
    type n
    type e
    type u
    type node
    val leaf : int list -> int -> u -> node
    val compare_trees_data :
      (node, 'a) Ptree.p_tree -> (node, 'a) Ptree.p_tree -> int list
    val create_initial_tree :
      int option ->
      float ->
      (n, e) Ptree.p_tree ->
      (node, e) Ptree.p_tree * node All_sets.IntegerMap.t
    val update :
      int option ->
      float ->
      (n, e) Ptree.p_tree ->
      (node, e) Ptree.p_tree ->
      node All_sets.IntegerMap.t ->
      All_sets.Integers.t ->
      (node, e) Ptree.p_tree * node All_sets.IntegerMap.t *
      All_sets.Integers.t * All_sets.Integers.t
    val distance : node -> node -> float
    val get_union_vertex :
      int -> (node, e) Ptree.p_tree -> node All_sets.IntegerMap.t -> int
    module Clusters :
      sig
        type c = node All_sets.IntegerMap.t
        val distance : c -> int -> int -> float
        val update_matrix :
          SparceMatrix.m ->
          c -> All_sets.Integers.t -> All_sets.Integers.t -> SparceMatrix.m
        val initial_matrix : c -> SparceMatrix.m
        module Test :
          sig
            val unity :
              SparceMatrix.m ->
              c -> All_sets.Integers.t -> All_sets.Integers.t -> bool
          end
      end
  end
module Codes :
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
module IntMap :
  sig
    type key = int
    type 'a t = 'a All_sets.IntegerMap.t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
val ( --> ) : 'a -> ('a -> 'b) -> 'b
type 'a node1 = {
  union : 'a;
  children_unions : All_sets.Integers.t;
  code : int;
  size : int;
}
module Make :
  functor (Node : NodeSig.S) ->
    functor (Edge : Edge.EdgeSig) ->
      sig
        type n = Node.n
        type e = Edge.e
        type u = Node.Union.u
        type node = Node.Union.u node1
        val leaf : int list -> int -> u -> node
        val compare_trees_data :
          (node, 'a) Ptree.p_tree -> (node, 'a) Ptree.p_tree -> int list
        val create_initial_tree :
          int option ->
          float ->
          (n, e) Ptree.p_tree ->
          (node, e) Ptree.p_tree * node All_sets.IntegerMap.t
        val update :
          int option ->
          float ->
          (n, e) Ptree.p_tree ->
          (node, e) Ptree.p_tree ->
          node All_sets.IntegerMap.t ->
          All_sets.Integers.t ->
          (node, e) Ptree.p_tree * node All_sets.IntegerMap.t *
          All_sets.Integers.t * All_sets.Integers.t
        val distance : node -> node -> float
        val get_union_vertex :
          int -> (node, e) Ptree.p_tree -> node All_sets.IntegerMap.t -> int
        module Clusters :
          sig
            type c = node All_sets.IntegerMap.t
            val distance : c -> int -> int -> float
            val update_matrix :
              SparceMatrix.m ->
              c ->
              All_sets.Integers.t -> All_sets.Integers.t -> SparceMatrix.m
            val initial_matrix : c -> SparceMatrix.m
            module Test :
              sig
                val unity :
                  SparceMatrix.m ->
                  c -> All_sets.Integers.t -> All_sets.Integers.t -> bool
              end
          end
      end
module Make2 :
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
        type n = Node.n
        type e = Edge.e
        type u = Node.Union.u
        type node = Node.Union.u
        val leaf : int list -> int -> u -> node
        val compare_trees_data :
          (node, 'a) Ptree.p_tree -> (node, 'a) Ptree.p_tree -> int list
        val create_initial_tree :
          int option ->
          float ->
          (n, e) Ptree.p_tree ->
          (node, e) Ptree.p_tree * node All_sets.IntegerMap.t
        val update :
          int option ->
          float ->
          (n, e) Ptree.p_tree ->
          (node, e) Ptree.p_tree ->
          node All_sets.IntegerMap.t ->
          All_sets.Integers.t ->
          (node, e) Ptree.p_tree * node All_sets.IntegerMap.t *
          All_sets.Integers.t * All_sets.Integers.t
        val distance : node -> node -> float
        val get_union_vertex :
          int -> (node, e) Ptree.p_tree -> node All_sets.IntegerMap.t -> int
        module Clusters :
          sig
            type c = node All_sets.IntegerMap.t
            val distance : c -> int -> int -> float
            val update_matrix :
              SparceMatrix.m ->
              c ->
              All_sets.Integers.t -> All_sets.Integers.t -> SparceMatrix.m
            val initial_matrix : c -> SparceMatrix.m
            module Test :
              sig
                val unity :
                  SparceMatrix.m ->
                  c -> All_sets.Integers.t -> All_sets.Integers.t -> bool
              end
          end
      end
