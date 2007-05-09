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

(* $Id: process.ml 1644 2007-02-14 19:05:47Z andres $ *)
let () = SadmanOutput.register "Process" "$Revision: 1644 $"

let debug = false

(* Book keeping of the current state of the processes *)

exception All_Children_Dead

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
module Make 
    (Node : NodeSig.S) 
    (Edge : Edge.EdgeSig with type n = Node.n)
    (CScrp : CharacterScripting.S with type n = Node.n) = struct

    type a = Node.n
    type b = Edge.e
    type c = CScrp.cs
    type d = float

type checkpoint = (Tree.u_tree, Data.d, d, c) Methods.checkpoints

exception Array_Problem

type condition =
    | Busy
    | Idle
    | Hangover ;;

type status =
    | Alive
    | Dead;;

let no_tags = -1
and no_checkpt : checkpoint option = None

and _waiting_parameter = 1;;

let avail_proc = ref (Queue.create ())
and all_proc = ref [||];;

let time () = 
    let t = Unix.time () in 
    t;;
             
let rec init_proc c it =
    match c with 
    | [] -> ();
    | rank :: t ->
            Queue.push rank !avail_proc;
            begin match it with
            | 0 ->
                    all_proc := Array.create (List.length c) (rank, 
                     Idle, Alive, no_tags, no_tags, no_checkpt, (time ()));
                    !all_proc.(0) <- (rank, Idle, Alive, no_tags, no_tags, 
                    no_checkpt, (time ()));
            | _ ->                    
                    !all_proc.(it) <- (rank, Idle, Alive, no_tags,
                    no_tags, no_checkpt, (time ()));
            end;
            init_proc t (it + 1);;

let i = ref 0;;
let return_val = ref 0;;

let get_position r =    
    while  !i <= ((Array.length !all_proc) - 1) do
        let rank, _, _, _, _, _, _ = !all_proc.(!i) in
        if r = rank then begin
                return_val := !i ;
                i := (Array.length !all_proc);
        end else begin
            i := !i + 1;
        end;            
    done;
    i:=0;
    !return_val;;

let get_response p =
    let true_rank = get_position p in
    try
        let _, _, _, _, er, _, _ = !all_proc.(true_rank) in 
        er;
    with
    |Invalid_argument arg -> 
            let error_message = ("wrong response from process: " ^ string_of_int
            (true_rank) ^ 
            " there is a problem with array values \n") in
            Status.user_message Status.Error error_message;
            raise (Invalid_argument arg);;

let rec get_proc tag (chekpt : [< checkpoint ]) =
    if not (Queue.is_empty !avail_proc) then begin
        if debug then 
            Status.user_message Status.Information "Processes available";
        let rank = Queue.pop !avail_proc in
        let true_rank = get_position rank in
        let _, _, exp_tag = Register.response tag in
        !all_proc.(true_rank) <- (rank, Busy, Alive, tag, exp_tag, Some (chekpt
        :> checkpoint), time ());
        Some rank
     end else begin
         if debug then
             Status.user_message Status.Information "No processes available";
         None
     end

let hang_proc p =
    let true_rank = get_position p in
    try
        let rank, condition, status, tag, resptag, checkpoint, time =
            !all_proc.(true_rank) in      
        !all_proc.(true_rank) <- (rank, Hangover, status, tag, resptag,
        checkpoint, time);
    with
    | Invalid_argument _ ->
            let error_message = ("an error occured while hanging the process ") in
            Status.user_message Status.Error error_message;
            raise (Array_Problem);;

let release_proc p =
    let true_rank = get_position p in
    try 
        let rank, _, _, _, _, _, t = !all_proc.(true_rank) 
        and c_t = (time ()) in
        !all_proc.(true_rank) <- (rank, Idle, Alive, no_tags, no_tags,
        no_checkpt, c_t);
        Queue.push (p) !avail_proc;
        let returnval = c_t -. t in
        returnval;
    with 
    |Invalid_argument _ ->
            let error_message = ("an error occurred while releasing the process ") in 
            Status.user_message Status.Error error_message;
            raise (Array_Problem);;


let get_checkpt p =
    let true_rank = get_position p in
    let _, _, _, _, _, c, _ = !all_proc.(true_rank) in
    c;;

let clean_chekpt p =
    let true_rank = get_position p in
    let r, s, l, t, tr, _, tim = !all_proc.(true_rank) in
    !all_proc.(true_rank) <- (r, s, l, t, tr,  no_checkpt, tim);;

let clean_tag p =
    let true_rank = get_position p in
    let r, s, l, _, _, cp, tim = !all_proc.(true_rank) in
    !all_proc.(true_rank) <- r, s, l, no_tags, no_tags, cp,tim;;

let clean_all p =
    clean_tag p;
    clean_chekpt p;;

let rem_proc rank =    
    let copy_proc = Queue.create () in
    while (not (Queue.is_empty !avail_proc)) do
        let rank_t = Queue.pop !avail_proc in
        if rank_t != rank 
        then begin
            Queue.push (rank_t) copy_proc;
        end;
    done;
   Queue.transfer copy_proc !avail_proc;;
     
let dead_proc p =
    let true_rank = get_position p in
    let rank,  _, _, tag, tagr, cpt, _ = !all_proc.(true_rank) in
    !all_proc.(true_rank) <- rank, Idle, Dead, no_tags, no_tags, no_checkpt, (time ());
    rem_proc rank;
    Ftol.dead (p);
    tag, cpt;;

let max_num_proc () =
    let res = ref 0 in
    for i = 0 to ((Array.length !all_proc) - 1) do
        begin match !all_proc.(i) with
        | _, _, Alive, _, _, _, _ -> res := !res + 1;
        | _, _, Dead, _, _, _, _ -> res := !res ;
        end;
    done;
    !res ;;

let num_avail_proc () =
    Queue.length !avail_proc;;

let _test a =
    match a with
    | _, _, Alive, _, _, _, _ -> true;
    | _, _, Dead, _, _, _, _ -> false;;

let rec _any_alive n l =
    if n < l then begin
        if _test !all_proc.(n) then true
        else _any_alive (n + 1) l
    end else false;;

let any_alive () =
    _any_alive 0 (Array.length !all_proc);;

let earliest_busy_job () = 
    let _list = ref []
    and proc = ref !all_proc.(0) in
    let lim = ref Pervasives.max_float in

    for i = 0 to ((Array.length !all_proc) - 1) do
        begin match !all_proc.(i) with
        | _, Busy, _, _, _, _, st_time -> 
                if (st_time < !lim)
                then begin
                   proc := !all_proc.(i); 
                   lim := st_time
                end;
        | _, Idle, _, _, _, _, _ -> ()
        | _, Hangover, _, _, _, _, _ -> ()
        end;
    done;
    
    let _rank,_,_,_,_,_checkpoint, _time = !proc 
    and max_float = Pervasives.max_float in 
    let elapsed = (time ()) -. _time in 
    
    if !lim = max_float then
            _rank, no_checkpt, elapsed
    else 
            _rank, _checkpoint, elapsed;;
    

let rec kill_proc c = 
    match c with 
    | [] -> ();
    | rank :: t ->
            let message = Ftol.my_rank () in
            if rank <> (Ftol.my_rank ()) then 
                Ftol.send Methods.process_management (`CleanExit message) rank;
            kill_proc t
    
end
(* vim: set sw=4 et tw=80: *)
