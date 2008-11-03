val leaf_distance : int
val depth_distance : int
module type GRAPHICS_TYPE =
  sig
    type color = int
    val black : color
    val close_graph : unit -> unit
    val draw_string : string -> unit
    val foreground : color
    val lineto : int -> int -> unit
    val moveto : int -> int -> unit
    val open_graph : string -> unit
    val plot : int -> int -> unit
    val red : color
    val set_color : color -> unit
    val size_x : unit -> int
    val size_y : unit -> int
    val text_size : string -> int * int
    val display : unit -> unit
  end
exception Wrong_format of string
val get_code : in_channel -> string
module Make :
  functor (G : GRAPHICS_TYPE) ->
    sig
      val draw_edges : int * int -> int * int -> unit
      val average : (int * int) list -> int ref * int
      val calc_depth_leaves :
        string Parser.Tree.t ->
        int ref -> int ref -> int ref -> int ref -> unit
      val draw :
        ?size:string -> ?leafColor:G.color -> string Parser.Tree.t -> unit
      val disp_tree : string -> string Parser.Tree.t -> unit
    end
