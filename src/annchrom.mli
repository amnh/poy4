val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
type annchrom_t = AnnchromAli.annchrom_t
type meds_t = {
  med_ls : annchrom_t list;
  num_med : int;
  total_cost : int;
  total_recost : int;
  annchrom_pam : Data.dyna_pam_t;
  cost_mat : Cost_matrix.Two_D.m;
  alpha : Alphabet.a;
}
val init_med :
  Sequence.s Data.seq_t array ->
  Cost_matrix.Two_D.m -> Alphabet.a -> Data.dyna_pam_t -> meds_t
val keep : Data.dyna_pam_t -> 'a list -> 'a list
val find_meds2 : ?keep_all_meds:bool -> meds_t -> meds_t -> meds_t
val find_meds3 : meds_t -> meds_t -> meds_t -> meds_t
val cmp_min_pair_cost : meds_t -> meds_t -> int * int
val cmp_max_pair_cost : meds_t -> meds_t -> int * int
val compare : meds_t -> meds_t -> int
val to_string : annchrom_t -> Alphabet.a -> string
val get_active_ref_code : meds_t -> int * int * int
