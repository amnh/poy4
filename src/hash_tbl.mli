exception Double_Collision
type hash_tbl_record = { id : int; count : int; size : int; index : int; }
type hash_tbl = {
  records : hash_tbl_record list array;
  h1_params : int array;
  h2_params : int array;
  m1 : int;
  m2 : int;
}
val empty_record : hash_tbl_record
val print_record : hash_tbl_record -> unit
val make : int array -> int array -> int -> int -> hash_tbl
val populate :
  int ->
  Rtree.r_tree * (int, int) Hashtbl.t * (int, int * int * int) Hashtbl.t ->
  hash_tbl -> (int, int * int * int) Hashtbl.t * hash_tbl
val get_data : int * int -> hash_tbl -> int * int * int
module Interface :
  sig
    type fp = int * int
    type t = (int, int * int * int) Hashtbl.t * hash_tbl
    val def_safety : int
    val make_params :
      ?safety_factor:int ->
      int -> int -> int * int * int array * int array * int
    val make_wparams : int * int * int array * int array * int -> t
    val make : ?safety_factor:int -> int -> int -> t
    val populate : t -> Rtree.r_tree -> int -> t
    val query : t -> int -> fp
  end
