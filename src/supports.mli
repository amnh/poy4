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

(* $Id: supports.mli 1803 2007-05-09 20:09:27Z andres $ *)
(* Created Thu Feb  2 16:04:01 2006 (Illya Bomash) *)

(** This module implements computing a support diagnosis of a tree. *)

module type S = sig
    type a 
    type b

    val support : 
        (a, b) Ptree.p_tree Sexpr.t ->
            a list -> 
        Methods.support_method -> Data.d -> 
         Sampler.ft_queue ->
             int Tree.CladeFPMap.t

    val bremer_support : 
        (a, b) Ptree.p_tree Sexpr.t ->
        int -> int ->
        a list -> (a, b) Ptree.p_tree Sexpr.t -> Methods.local_optimum
        -> Methods.build -> Data.d -> Sampler.ft_queue ->
            Methods.support_tree Sexpr.t

val support_to_string_tree : Data.d -> Methods.support_tree -> string Parser.Tree.t

(** [join_support_trees trees] takes a list of [(iterations, support_tree)]
    pairs and combines them into a single support tree *)
val join_support_trees : (int * Methods.support_tree) list -> Methods.support_tree


end

module Make (Node : NodeSig.S) (Edge : Edge.EdgeSig with type n = Node.n) 
    (TreeOps : functor (Exact : Ptree.Exact) ->
        Ptree.Tree_Operations 
        with type a = Node.n with type b = Edge.e) : S 
    with type a = Node.n
    with type b = Edge.e
