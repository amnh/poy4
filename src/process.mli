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

(** Parallel process handling. *)

(* $Id: process.mli 1644 2007-02-14 19:05:47Z andres $ *)

(** {2 Exceptions} *)

exception All_Children_Dead;;

module type S = sig
(** {2 Types} *)

type a 
type b
type c
type d
type checkpoint = (Tree.u_tree, Data.d, d, c) Methods.checkpoints

(** {2 Functions} *)

(* Tell [Process] about all running processes *)
val init_proc : Ftol.rank list -> int -> unit 

val kill_proc : Ftol.rank list -> unit

(** [no_tags] is the internal tag employeed to represent any tag in a message. It
 * also represents a the condition of no responses expected as a result of sending
 * a message. *)
val no_tags : int 

(** [any_alive ()] tests weather or not there are still alive processes available
 * for computation or not *)
val any_alive : unit -> bool 

(** [num_avail_proc ()] returns the number of processes in Idle condition
 * currently waiting for jobs to be sent. *)
val num_avail_proc : unit -> int

(** [get_proc tag checkpt] gets a tuple (a, b) where a is the rank of an idle
 * process in the communicator b. tag and checkpt are stored for fault tolerance
 * with the functions that their names mean. The returned process is set in Busy
 * condition, and a release_proc call is necessary to make it available again for
 * computation. 
 * *)
val get_proc : Mpi.tag -> [< checkpoint] -> Ftol.rank option

(** [dead_proc p] updates the information in the module, setting the process p as
 * a dead process and returns the checkpoint and job information of that process in
 * a tuple (a, b) respectively. *)
val dead_proc : Ftol.rank -> Mpi.tag * checkpoint option

(** [get_response p] returns the tag of the expected response from process p *)
val get_response : Ftol.rank -> int

(** [release_proc p] sets the process with rank p in Idle condition, and
* available for jobs execution. *)
val release_proc : Ftol.rank -> float

(** [max_num_proc ()] is the maximum possible number of processes available at a
 * given time unit for parallel execution. *)
val max_num_proc : unit -> int

(** [get_checkpt p] returns the stored checkpoint of process p *)
val get_checkpt : Ftol.rank -> checkpoint option

(** [clean_all p] cleans the stored tag and checkpoint of process p *)
val clean_all : Ftol.rank -> unit

val earliest_busy_job : unit -> Ftol.rank * checkpoint option  * float

end

module Make (Node : NodeSig.S) (Edge : Edge.EdgeSig with type n = Node.n) 
    (CScrp : CharacterScripting.S with type n = Node.n) :
        S with type a = Node.n with type b = Edge.e 
        with type c = CScrp.cs with type d = float
