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

(** Parallel communications handler. *)

(* $Id: comm.mli 1644 2007-02-14 19:05:47Z andres $ *)

(** {2 Exceptions} *)

exception Error of string;;

(** {2 Types} *)

(** Internal representation of the process name. *)
type proc_uid = int;;

(** A process conditions and uid in the set of processes being supported. The
* Alive and Dead conditions are ignored unless fault tolerance is supported and
* active. *)
type process = 
    | Alive of proc_uid
    | Dead of proc_uid;;

(** The status of the communication library, according to the communications
* protocol established in the introductory part of the library. *)
type status = 
    | Non_Initialized
    | Initialized 
    | Finalized;;

(** The mode of execution of the parallel environment. In synchronous mode, each
* executable is performing the same operations at same speed in an homogeneus
* environment, or in a master slave mode, better for heterogeneus environments.
* *)
type exec_mode = 
    | Synch
    | Asynch;;

(** The possible fault tolerant modes in the communication library. *)
type ft_mode =
    | Fault_Tolerant
    | Non_Fault_Tolerant;;


(** {2 Functions} *)

(** [supp_exec_mode] returns the list of exec_mode supported by the
* implementation. *)
val supp_exec_mode: unit -> exec_mode list

(** [is_ft] returns true if the library provides fault tolerance, false if not.
* *)
val is_ft: unit -> ft_mode

val get_exec_mode : unit -> ft_mode * exec_mode

(** [comm_status] returns the status of the communications library. *)
val comm_status: unit -> status


(** [run_slave ()] enters in the main loop of the slaves of the program. While
 the leader will follow the normal program execution, the slaves enter this loop
 where they receive messages and execute the function returned by the Parfunc
 module. 
val run_slave : unit -> unit
*)



(* [init do_inform a ft m] initializes the communications environment. This function 
 * should
* be called before any parallel operation is to be performed, and no calls can
* occur after the first one. The program arguments a are evaluated by the
* communications environment and modified if necessary, if ft is true then the
* communications should support fault tolerance (communications and processing
* overhead), and the operation mode m is employed for the processing of the
* data. 
* Before being called comm_status () returns Non_Initialized, after init is
* called, comm_status () returns Initialized.
* init returns the array of program arguments a with the necessary modifications
* according to the parallel environment, ready for POY's parsing; for this
* reason, POY should not parse the program arguments before init has been
* called.
* 
* The function will raise Error s in the following cases:
* {ul {If the function is called more than once.}
* {If ft is true but the library does not support fault tolerance}
* {If the mode m is not supported by the library}
* {If the Combination of ft and m is not supported by the library}
* {If one of the communication initialization functions of the specific}}
* environment outputs an error (ie. if MPI cannot initialize properly).
* All the previous cases should cause the program to *abort*. s should always
* be a human readable error message. 
* *)
val init: (unit -> unit) -> string array -> ft_mode -> exec_mode -> string array

(** [final f_do_inform ()] closes all the communication libraries. After final () is executed, no
* further parallel execution can be completed, and the serial operation
* continues. Before final () is called comm_status () returns either Initialized
* or Non_Initialized, deppending on [init]. After final (), comm_status ()
* returns Finalized, and any further call to final or init raises an error. *)
val final: (unit -> unit) -> unit -> unit

val dont_inform_dead_to_anyone : unit -> unit

(* vim:sw=4 et tw=80
 *)
