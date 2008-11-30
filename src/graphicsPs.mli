module Ps : GraphTree.GRAPHICS_TYPE with type display = Graphicpdf.display
module GraphTreePs :
  sig
    val draw_edges : Ps.display -> int * int -> int * int -> Ps.display
    val average : (int * int) list -> int ref * int
    val calc_depth_leaves :
      string Parser.Tree.t ->
      int ref -> int ref -> int ref -> int ref -> unit
    val draw :
      ?size:string -> ?leafColor:Ps.color -> 
          Ps.display -> string Parser.Tree.t -> Ps.display
    val disp_tree : Ps.display -> string -> string Parser.Tree.t -> Ps.display
  end
val d : int ref
val max_depth : int ref
val num_leaves : int ref
val longest_name : int ref
val reset : unit -> unit
val display :
  string -> string -> (float * string Parser.Tree.t) array -> unit

val display_diagnosis :
  string -> string -> Tags.output Sexpr.t -> unit
