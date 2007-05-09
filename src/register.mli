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

(* $Id: register.mli 1644 2007-02-14 19:05:47Z andres $ *)


(** Tags for messages and parallel functions registration *)

(** {2 Exceptions} *)

(* [Func_Not_Found] is raised when a find operation yields no matching code,
* answer or function. *)
exception Func_Not_Found

(** {2 Types} *)
type all_parallel =
    | Two_on_array_ret_float of 
        ((int -> int) -> (int -> float) -> int array ref -> float)
    | One_on_array_ret_array of 
        ((int -> int) -> int array ref -> int array ref)
    | Two_on_array_ret_array of 
        ((int -> int) -> (int -> float) -> int array ref ->
            float -> int array ref)
    | One_bool_on_array_ret_array of 
        ((int -> bool) -> int array ref -> int array ref)
    | Int_ret_int of (int -> int)
    | Int_ret_float of (int -> float)
    | Int_ret_bool of (int -> bool)
    | Nil
;;

(** {2 Functions} *)

(** [process_failed] is the tag for reporting a failing process to the leader *)
val process_failed : int

(** {1 gather_minimum_tag}
 *
 * The tag for executing a gather_minimum in parallel *)
val gather_minimum_tag : int
val gather_minimum_resp : int

(** {1 gather_maximum_tag}
 *
 * The tag for executing a gather_maximum in parallel *)
val gather_maximum_tag : int
val gather_maximum_resp : int

(** {1 all_minimize_tag} 
 *
 * The tag for executing an all_minimize in parallel *)
val all_minimize_tag : int
val all_minimize_resp :int

(** {1 all_maximize_tag} 
 *
 *  The tag for executing an all_maximize in parallel *)
val all_maximize_tag : int
val all_maximize_resp : int

val gather_within_threshold_tag : int
val gather_within_threshold_resp  : int

val set_pair_sum_tag : int
val set_pair_sum_resp : int

val set_pair_fold_tag : int
val set_pair_fold_resp : int

val map_tag : int
val map_resp : int

val scan_tag : int
val scan_resp : int

(** [register_init ()] registers the function tags. it is the first function
 * that has to be called before calling any other functions in the Register
 * library. *)
(*
val register_init : unit -> unit
*)
(** [register_asynch f] registers the function f in the initialization phase only
* *)

val register_init : unit -> unit

val register_asynch : all_parallel -> int * int


(** [register f] registers a function f to be performed in parallel and returns
* the tuple a, b where a is the tag code assigned to f and b is the expected tag
* in the answer of the executing process.*)

val register : all_parallel -> int * int

val response : int -> all_parallel * int * int

val funct : all_parallel -> all_parallel * int * int

val tag : int -> all_parallel * int * int


(*
(* [code f] search for an already registered function and returns the code
* assigned to it. If a function is registered more than once, only the first
* code will be retrieved, but this practice will decrease the matching speed in
* the internal registry. 
* If the function is not found, raises a Func_Not_Found exception. *)
val code : ('a -> 'b) -> int

(* [funct c] returns the function associated with the internal code c. 
* If the code is not found, raises a Func_Not_Found exception. *)
val funct : int -> ('a -> 'b)

(* [response c] returns the expected response tag in the message of a process
* executing the function with tag c *)
val response : int -> int

*)
(* vim:sw=4 et tw=80
 *)
