type incremental =
    [ `Children of int
    | `HandleC of int * int
    | `HandleNC of int * int
    | `No_Children of int ]
module type S =
  sig
    type a
    type b
    val get_trees_considered : unit -> int
    val reset_trees_considered : unit -> unit
    class type wagner_mgr = [a, b] Ptree.wagner_mgr
    class wagner_srch_mgr : bool -> int -> float -> wagner_mgr
    class type search_mgr = [a, b] Ptree.search_mgr
    class first_best_srch_mgr :
      (a, b) Sampler.search_manager_sampler -> search_mgr
    class standard_tabu_searcher : 
        (a, b) Sampler.search_manager_sampler -> 
            All_sets.Integers.t list -> int -> int -> 
                ((a, b) Ptree.p_tree -> (a, b) Ptree.p_tree) ->
                    ((a, b) Ptree.p_tree -> All_sets.Integers.t -> (a, b)
                    Ptree.p_tree) -> ((a, b) Ptree.p_tree -> (a, b)
                    Ptree.p_tree) ->
                         search_mgr
    class hold_n_fb_srch_mgr :
      int ->
      Methods.keep_method ->
      (a, b) Sampler.search_manager_sampler -> search_mgr
    class hold_n_threshold_srch_mgr :
      int ->
      Methods.keep_method ->
      float -> (a, b) Sampler.search_manager_sampler -> search_mgr
    class annealing_srch_mgr :
      int ->
      Methods.keep_method ->
      float ->
      float -> float -> (a, b) Sampler.search_manager_sampler -> search_mgr
    class classic_poy_drifting_srch_mgr :
      int ->
      Methods.keep_method ->
      float -> float -> (a, b) Sampler.search_manager_sampler -> search_mgr
    class all_possible_joins :
      string option -> (a, b) Sampler.search_manager_sampler -> search_mgr
    class supports_manager :
      float array ->
      (int * int) list ->
      int ->
      Status.status ->
      int ref -> float -> (a, b) Sampler.search_manager_sampler -> search_mgr
    class all_neighbors_srch_mgr :
      (a, b) Ptree.join_fn ->
      (a, b) Ptree.break_fn ->
      (a, b) Ptree.reroot_fn ->
      ((a, b) Ptree.p_tree -> Ptree.incremental list -> (a, b) Ptree.p_tree) ->
      (a, b) Sampler.search_manager_sampler -> search_mgr
  end
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
        val get_trees_considered : unit -> int
        val reset_trees_considered : unit -> unit
        class type wagner_mgr = [a, b] Ptree.wagner_mgr
        class wagner_srch_mgr : bool -> int -> float -> wagner_mgr
        class type search_mgr = [a, b] Ptree.search_mgr
        class first_best_srch_mgr :
          (a, b) Sampler.search_manager_sampler -> search_mgr
        class standard_tabu_searcher : 
            (a, b) Sampler.search_manager_sampler -> 
                All_sets.Integers.t list -> int -> int -> 
                    ((a, b) Ptree.p_tree -> (a, b) Ptree.p_tree) ->
                        ((a, b) Ptree.p_tree -> All_sets.Integers.t -> (a, b)
                        Ptree.p_tree) -> ((a, b) Ptree.p_tree -> (a, b)
                        Ptree.p_tree) ->
                             search_mgr
        class hold_n_fb_srch_mgr :
          int ->
          Methods.keep_method ->
          (a, b) Sampler.search_manager_sampler -> search_mgr
        class hold_n_threshold_srch_mgr :
          int ->
          Methods.keep_method ->
          float -> (a, b) Sampler.search_manager_sampler -> search_mgr
        class annealing_srch_mgr :
          int ->
          Methods.keep_method ->
          float ->
          float ->
          float -> (a, b) Sampler.search_manager_sampler -> search_mgr
        class classic_poy_drifting_srch_mgr :
          int ->
          Methods.keep_method ->
          float ->
          float -> (a, b) Sampler.search_manager_sampler -> search_mgr
        class all_possible_joins :
          string option ->
          (a, b) Sampler.search_manager_sampler -> search_mgr
        class supports_manager :
          float array ->
          (int * int) list ->
          int ->
          Status.status ->
          int ref ->
          float -> (a, b) Sampler.search_manager_sampler -> search_mgr
        class all_neighbors_srch_mgr :
          (a, b) Ptree.join_fn ->
          (a, b) Ptree.break_fn ->
          (a, b) Ptree.reroot_fn ->
          ((a, b) Ptree.p_tree ->
           Ptree.incremental list -> (a, b) Ptree.p_tree) ->
          (a, b) Sampler.search_manager_sampler -> search_mgr
      end
