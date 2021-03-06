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

val k_val : float ref
val lambda_val : float ref
val sta_sig_prob : float
val get_score : int -> int -> Cost_matrix.Two_D.m -> int
val get_float_score : int -> int -> Cost_matrix.Two_D.m -> float
val find_lambda : Cost_matrix.Two_D.m -> float array -> float
val find_K : float -> Cost_matrix.Two_D.m -> float array -> float
val init : Cost_matrix.Two_D.m -> float array -> unit
val create_default_score : unit -> Cost_matrix.Two_D.m
val cmp_code_frq : int -> Sequence.s -> Sequence.s -> float array
val init_default : Sequence.s -> Sequence.s -> unit
val cmp_min_score : int -> int -> int
