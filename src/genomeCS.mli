exception Illegal_Arguments
val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
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
type meds_t = Genome.meds_t
type t = {
  meds : meds_t IntMap.t;
  costs : float IntMap.t;
  recosts : float IntMap.t;
  total_cost : float;
  total_recost : float;
  subtree_recost : float;
  c2 : Cost_matrix.Two_D.m;
  c3 : Cost_matrix.Three_D.m;
  alph : Alphabet.a;
  code : int;
}
val cardinal : t -> int
val of_array :
    Data.dynamic_hom_spec -> 
        (Sequence.s Data.dyna_data * IntMap.key) array -> 
            int -> int -> int -> t
val of_list :
    Data.dynamic_hom_spec -> 
        (Sequence.s Data.dyna_data * IntMap.key) list -> 
            int -> int -> int -> t
val to_list : t -> (meds_t * IntMap.key) list
val same_codes : 'a IntMap.t -> 'b IntMap.t -> bool
val median2 : t -> t -> t
val median3 : t -> t -> t -> t -> t
val distance : t -> t -> float
val max_distance : t -> t -> float
val to_string : t -> string
val dist_2 : t -> t -> t -> float
val f_codes : t -> All_sets.Integers.t -> t
val f_codes_comp : t -> All_sets.Integers.t -> t
val compare_data : t -> t -> int
val to_formatter :
  Tags.attribute list -> t -> t option -> Data.d -> Tags.output list
