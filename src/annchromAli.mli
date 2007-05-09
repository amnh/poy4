val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
type seq_t = {
  seq : Sequence.s;
  seq_ref_code : int;
  seq_ord1 : int;
  seq_ord2 : int;
}
type annchrom_t = {
  seq_arr : seq_t array;
  ref_code : int;
  ref_code1 : int;
  ref_code2 : int;
  cost1 : int;
  recost1 : int;
  cost2 : int;
  recost2 : int;
}
type annchromPam_t = {
  re_meth : Data.re_meth_t;
  keep_median : int;
  circular : int;
  swap_med : int;
  locus_indel_cost : int * int;
}
val annchromPam_default : annchromPam_t
val init : seq_t array -> annchrom_t
val get_annchrom_pam : Data.dyna_pam_t -> annchromPam_t
val print : annchrom_t -> Alphabet.a -> unit
val split : annchrom_t -> Sequence.s array * int array
val create_pure_gen_cost_mat :
  Sequence.s array ->
  Sequence.s array ->
  Cost_matrix.Two_D.m ->
  annchromPam_t -> int array array * int array * int array * int
val cmp_cost :
  annchrom_t ->
  annchrom_t ->
  Cost_matrix.Two_D.m -> 'a * 'b -> 'c -> Data.dyna_pam_t -> int * int
val find_med2_ls :
  annchrom_t ->
  annchrom_t ->
  Cost_matrix.Two_D.m ->
  'a * 'b -> 'c -> Data.dyna_pam_t -> int * int * annchrom_t list
val compare : annchrom_t -> annchrom_t -> int
val assign_seq_ref : annchrom_t -> int -> annchrom_t * int
val create_map : annchrom_t -> int -> int * int * Tags.output
val to_formater : annchrom_t -> Alphabet.a -> string
