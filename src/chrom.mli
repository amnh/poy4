val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
type med_t = ChromAli.med_t
type meds_t = {
  med_ls : med_t list;
  total_cost : int;
  total_recost : int;
  c2 : Cost_matrix.Two_D.m;
  approx_cost_arr : int array;
  approx_recost_arr : int array;
  code : int;
  chrom_pam : Data.dyna_pam_t;
}
val init_med : Sequence.s -> Data.dyna_pam_t -> int -> int -> meds_t
val keep : Data.dyna_pam_t -> 'a list -> 'a list
val update_cost_mat : meds_t -> meds_t -> unit
val find_meds2 : ?keep_all_meds:bool -> meds_t -> meds_t -> meds_t
val find_meds3 : meds_t -> meds_t -> meds_t -> meds_t
val cmp_min_pair_cost : meds_t -> meds_t -> int * int
val cmp_max_pair_cost : meds_t -> meds_t -> int * int
val compare : meds_t -> meds_t -> int
val get_active_ref_code : meds_t -> int * int * int
