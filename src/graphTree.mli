val leaf_distance : int
val depth_distance : int
module type GRAPHICS_TYPE = sig
    type display
    type color = int
    val black : color
    val close_graph : display -> unit
    val draw_string : 
        ?tag:string -> ?link:string -> display -> string -> display
    val foreground : color 
    val lineto : ?tag:string -> ?link:string -> display -> int -> int -> display
    val moveto : display -> int -> int -> display
    val polyline : display -> (int * int * string option * string option) list -> 
        display
    val open_file : string -> display
    val open_graph : display -> string -> display
    val plot : display -> int -> int -> display
    val red : color
    val set_color : display -> color -> display
    val size_x : display -> int
    val size_y : display -> int
    val text_size : string -> int * int
    val display : display -> unit
    val add_page : display -> display
end
exception Wrong_format of string
val get_code : in_channel -> string
module Make :
  functor (G : GRAPHICS_TYPE) ->
    sig
      val draw_edges : G.display -> int * int -> int * int -> G.display
      val average : (int * int) list -> int ref * int
      val calc_depth_leaves :
        string Parser.Tree.t ->
        int ref -> int ref -> int ref -> int ref -> unit
      val calc_depth_leaves_diag :
        Xml.xml ->
        int ref -> int ref -> int ref -> int ref -> unit
      val draw :
        ?size:string -> ?leafColor:G.color -> G.display ->
             string Parser.Tree.t -> G.display
      val draw_diagnosis :
          ?prefix:string -> ?size:string -> ?leafColor:G.color -> G.display ->
             Xml.xml -> G.display * (Xml.xml list)
      val disp_tree : 
          G.display -> string -> string Parser.Tree.t -> G.display
    end
