module Ps : GraphTree.GRAPHICS_TYPE
module GraphTreePs :
  sig
    val draw_edges : int * int -> int * int -> unit
    val average : (int * int) list -> int ref * int
    val calc_depth_leaves :
      string Parser.Tree.t ->
      int ref -> int ref -> int ref -> int ref -> unit
    val draw :
      ?size:string -> ?leafColor:Ps.color -> string Parser.Tree.t -> unit
    val disp_tree : string -> string Parser.Tree.t -> unit
  end
val d : int ref
val max_depth : int ref
val num_leaves : int ref
val longest_name : int ref
val reset : unit -> unit
val display :
  string -> string -> (float * string Parser.Tree.t) array -> unit
