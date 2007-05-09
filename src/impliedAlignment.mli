(* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    *)
(* Copyright (C) 2007  Andrés Varón, Le Sy Vinh, Illya Bomash, Ward Wheeler,  *)
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

(** A module to build implied alignments of sequences over trees. *)

(** The implied alignments are multiple sequence alignments that follow a
* homology hypothesis implied by the best hypothesis available to explain their
* origin. *)

exception NotASequence of int

(** A sequence representation used in the generation of an implied alignment *)
type ias = {
    seq : Sequence.s;    (** The sequence contained *)
    codes : (int, int) Hashtbl.t;
    (** The code of each position in the sequence *)
    homologous: (int, int Sexpr.t) Hashtbl.t;
    (** The other assigned codes that are homologous to a particular code 
    * assigned to [seq] or to other sequences. *)
    cannonic_code : int; (* The code of the implied alignment, to keep left and
    right relationships.*)
    order : int list; (* The order of the codes stored in homologous *)
}

type dyna_state_t = Data.dyna_state_t

(* t is the presentation of a dynamic set (DynamicCS) in order to create implied
   alignments.
   Note: sequences : ias list Codes.t where (ias list) is a list of equally
   optimal medians in case of chromosomes
   
*)
type t = {
    sequences : ias list All_sets.IntegerMap.t;
    c2 : Cost_matrix.Two_D.m;
    chrom_pam : Data.dyna_pam_t; 
    state : dyna_state_t;
    code : int;
}
type pairs = int * t list

(** A unique code generation function *)
type cg = (unit -> int)

(** Creates a fresh code generation functions *)
val code_generator : unit -> cg

(** [create_ias s cg] creates a fresh sequence representation for implied
* alignments of sequence [s] using the code generation function [cg]. *)
val create_ias : Sequence.s -> int -> cg -> ias

(** [ancestor a b cm m] creates a common ancestor for sequences [a] and [b]
 * using the cost matrix [cm] and the alignment matrix [m] 
 * The resulting common ancestor holds the homology
 * relationships of the codes assigned in [a] and [b]. *)
val ancestor : int -> ias -> ias -> Cost_matrix.Two_D.m -> Matrix.m -> ias

module type S = sig
    type a 
    type b
    type tree = (a, b) Ptree.p_tree

    (** [of_tree t] generates the implied alignment of all the sequences in the tree
    * [t]. *)
    val of_tree : (int * tree) -> Methods.implied_alignment

    val concat_alignment :
          (int * int array list All_sets.IntegerMap.t list) list list ->
          (int * int array All_sets.IntegerMap.t list) list list


    val create : (tree -> int list -> tree) ->
        int list -> Data.d ->
        tree -> Methods.implied_alignment list
    

    val to_static_homologies : bool ->
        (tree -> int list -> tree) ->
            bool  -> Methods.characters -> Data.d -> tree -> Data.d


             
end

module Make (Node : NodeSig.S) (Edge : Edge.EdgeSig with type n = Node.n) : 
    S with type a = Node.n with type b = Edge.e
