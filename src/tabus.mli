module type S =
  sig
    type a
    type b
    type phylogeny = (a, b) Ptree.p_tree
    class type wem = [a, b] Ptree.wagner_edges_mgr
    val wagner_tabu : phylogeny -> int -> wem
    val wagner_constraint : All_sets.Integers.t -> phylogeny -> int -> wem
    val wagner_union : phylogeny -> int -> wem
    val distance_dfs_wagner : phylogeny -> int -> wem
    type emc
    type semc
    val union_join : int -> semc
    val distance_join : int -> semc
    val partitioned_join :
      [ `Height of int | `Sets of All_sets.IntSet.t ] -> int -> semc
    val reroot :
      [ `Height of int | `Sets of All_sets.IntSet.t ] option -> int -> semc
    val random_break : emc
    val sorted_break :
      bool -> [ `Height of int | `Sets of All_sets.IntSet.t ] option -> emc
    val only_once_break : emc
    class type tabu_mgr = [a, b] Ptree.tabu_mgr
    val join_to_tree_in_forest :
      phylogeny -> int -> tabu_mgr * All_sets.Integers.elt
    class standard_tabu : phylogeny -> semc -> semc -> emc -> tabu_mgr
  end
val timer_interval : int ref
module Make :
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
        type a = Node.n
        type b = Edge.e
        type phylogeny = (a, b) Ptree.p_tree
        class type wem = [a, b] Ptree.wagner_edges_mgr
        val wagner_tabu : phylogeny -> int -> wem
        val wagner_constraint :
          All_sets.Integers.t -> phylogeny -> int -> wem
        val wagner_union : phylogeny -> int -> wem
        val distance_dfs_wagner : phylogeny -> int -> wem
        type emc
        type semc
        val union_join : int -> semc
        val distance_join : int -> semc
        val partitioned_join :
          [ `Height of int | `Sets of All_sets.IntSet.t ] -> int -> semc
        val reroot :
          [ `Height of int | `Sets of All_sets.IntSet.t ] option ->
          int -> semc
        val random_break : emc
        val sorted_break :
          bool ->
          [ `Height of int | `Sets of All_sets.IntSet.t ] option -> emc
        val only_once_break : emc
        class type tabu_mgr = [a, b] Ptree.tabu_mgr
        val join_to_tree_in_forest :
          phylogeny -> int -> tabu_mgr * All_sets.Integers.elt
        class standard_tabu : phylogeny -> semc -> semc -> emc -> tabu_mgr
      end
