(* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    *)
(* Copyright (C) 2007  Andr�s Var�n, Le Sy Vinh, Illya Bomash, Ward Wheeler,  *)
(* and the American Museum of Natural History.                                *)
(*                                                                            *)
(* This program is free software; you can redistribute it and/or modify       *)
(* it under the terms of the GNU General Public License as published by       *)
(* the Free Software Foundation; either version 2 of the License, or          *)
(* (at your option) any later version.                                        *)
(*                                                                            *)
(* This program is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(* GNU General Public License for more details.                               *)
(*                                                                            *)
(* You should have received a copy of the GNU General Public License          *)
(* along with this program; if not, write to the Free Software                *)
(* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301   *)
(* USA                                                                        *)


val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
type annchrom_t = AnnchromAli.annchrom_t
type meds_t = {
  med_ls : annchrom_t list;
  num_med : int;
  total_cost : int;
  total_recost : int;
  cost_mat : Cost_matrix.Two_D.m;
  alpha : Alphabet.a;
  annchrom_pam : Data.dyna_pam_t;
  approx_med_arr : annchrom_t array;
  approx_cost_arr : int array;
  approx_recost_arr : int array;
  code : int;
}
val init_med :
  Sequence.s Data.seq_t array ->
  Cost_matrix.Two_D.m ->
  Alphabet.a -> Data.dyna_pam_t -> int -> int -> meds_t
val update_approx_mat : meds_t -> meds_t -> unit
val find_meds2 : meds_t -> meds_t -> meds_t
val find_meds3 : meds_t -> meds_t -> meds_t -> meds_t
val cmp_min_pair_cost : meds_t -> meds_t -> int * int
val cmp_max_pair_cost : meds_t -> meds_t -> int * int
val compare : meds_t -> meds_t -> int
val to_string : annchrom_t -> Alphabet.a -> string
val get_active_ref_code : meds_t -> int * int * int
val copy_chrom_map : meds_t -> meds_t -> meds_t
