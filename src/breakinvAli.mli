val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
type breakinv_t = {
  seq : Sequence.s;
  ref_code : int;
  ref_code1 : int;
  ref_code2 : int;
  cost1 : int;
  cost2 : int;
  recost1 : int;
  recost2 : int;
}
type breakinvPam_t = {
  re_meth : Data.re_meth_t;
  keep_median : int;
  circular : int;
  swap_med : int;
}
val breakinvPam_default : breakinvPam_t
val init : Sequence.s -> breakinv_t
val get_breakinv_pam : Data.dyna_pam_t -> breakinvPam_t
val cmp_cost :
  breakinv_t ->
  breakinv_t ->
  Cost_matrix.Two_D.m ->
  int array array -> Alphabet.a -> Data.dyna_pam_t -> int * int
val find_med2_ls :
  breakinv_t ->
  breakinv_t ->
  Cost_matrix.Two_D.m ->
  int array array ->
  Alphabet.a -> Data.dyna_pam_t -> int * int * breakinv_t list
val get_costs : breakinv_t -> int -> int * int
