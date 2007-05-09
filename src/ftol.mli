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

(** Fault tolerant communications using the MPI library. *)

(* $Id: ftol.mli 1644 2007-02-14 19:05:47Z andres $ *)

(** {2 Exceptions} *)

exception Cannot_send_message

(** {2 Types} *)

type role = 
    | Slave
    | Master
    | Serial

type rank =
    | Nil
    | Rank of int;;

(** {2 Initialization} *)

(** [init ()] initializes the Ftol module. Should always be called before any
* other function, otherwise the behavior is unpredictable. *)
val init : unit -> role

(** {2 Environment Information} *)

(** [my_rank ()] gets the rank of the calling process *)
val my_rank :  unit -> rank

(** [int_of_rank x] gets the integer representation of the rank x *)
val int_of_rank : rank -> int

(** [rank_of_int x] gets the rank representation of the integer x *)
val rank_of_int : int -> rank 

(** [numberofprocesses ()] gets the total number of processes running in the
* world communicator the calling process belongs to *)
val number_of_processes : unit -> int

(** The list of slaves available for the calling process. *)
val list_of_slaves : rank list ref

(** [any_tag] returns the generic tag mark for testing messages of any tag *)
val any_tag :  Mpi.tag

(** [any_source] returns the generic rank for testing message from anyone *)
val any_source :  rank

(** {2 Setting Information} *)

(** [dead x] marks the process with rank x as a dead process. *)
val dead : rank -> unit

(** {2 Sending and Receiving} *)

(** [prob tag proc] tests if there is a message comming from proc marked with tag
* The operation is non blocking and returns None if
* there is no message avilable, otherwise returns Some of  tag, source. *)
val prob : Mpi.tag -> rank -> (Mpi.tag * rank) option

(** [send tag msg proc] sends a msg marked with tag to proc. An Mpi.Error error
* can be raised if there is a problem with the communications. *)
val send : Mpi.tag -> 'a -> rank -> unit

(** [recv tag proc] recieves a message marked with tag from proc. An Mpi.Error
* error can be raised if there is a problem with the communications *)
val recv : Mpi.tag -> rank -> 'a

val initialize_communications : rank -> unit

val set_workers : rank list -> unit

val set_master : rank -> unit

val my_master : unit -> rank
