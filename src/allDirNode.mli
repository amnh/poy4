type exclude = Node.exclude
type direction = (int * int) option
type 'a my_lazy = Lazy of 'a Lazy.t | Eager of 'a
type a_node = Node.node_data my_lazy
val force_val : 'a my_lazy -> 'a
val lazy_from_fun : (unit -> 'a) -> 'a my_lazy
val lazy_from_val : 'a -> 'a my_lazy
val force : 'a my_lazy -> 'a my_lazy
type node_dir = { lazy_node : a_node; dir : direction; code : int; }
type node_data = { unadjusted : node_dir list; adjusted : node_dir list; }
val to_n : 'a -> 'a my_lazy
val has_code : int -> node_dir -> bool
val get_code : node_dir list -> int
val print_pairs : node_dir -> unit
val yes_with : int -> node_dir list -> node_dir * node_dir
val not_with : int -> node_dir list -> node_dir
module OneDirF :
    NodeSig.S with type e = exclude with type n = a_node with type other_n =
        Node.Standard.n with type nad8 = Node.Standard.nad8 
module AllDirF :
    NodeSig.S with type e = exclude with type n = node_data with
    type other_n = Node.Standard.n with
    type nad8 = Node.Standard.nad8 

type 'a node_hybrid = { st : Node.Standard.n option; dy : 'a; }
module HybridF : sig val get_dynamic : 'a node_hybrid -> 'a end
