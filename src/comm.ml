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

(* $Id: comm.ml 1644 2007-02-14 19:05:47Z andres $ *)
let () = SadmanOutput.register "Comm" "$Revision: 1644 $"


(* This is the MPI implementation of the Comm module for OCaml. This
 implementation relies on version 1.2.5 of MPICH for proper working, but it
 should work fine in any MPI 1.2 compilant implementation. *)

exception Error of string;;

(* Internal naming of a process in the parallel run *)
type proc_uid = int
and ft = bool;;

type process = 
    | Alive of proc_uid
    | Dead of proc_uid;;

type status = 
    | Non_Initialized
    | Initialized 
    | Finalized;;

type exec_mode = 
    | Synch
    | Asynch;;

type ft_mode =
    | Fault_Tolerant
    | Non_Fault_Tolerant;;

let _int_status = ref (Non_Initialized)
and _is_ft = Non_Fault_Tolerant
and _exec_mode_supp = [Synch;Asynch]
and _exec_mode = ref Synch
and _is_ft_mode = ref Non_Fault_Tolerant;;

let is_ft () =
    _is_ft;;

let get_exec_mode () =
    !_is_ft_mode, !_exec_mode;;

let comm_status () =
    !_int_status;;
   

let supp_exec_mode () =
    _exec_mode_supp;;

let do_inform = ref true

let dont_inform_dead_to_anyone () =
    do_inform := false

let final f_do_inform () =
    match !_int_status with
    | Initialized ->
            if !do_inform then begin
                f_do_inform ()
                (*
                Process.kill_proc !Ftol.list_of_slaves;
                Process.kill_proc [(Ftol.rank_of_int 0)];
                *)
            end else ();
            Mpi.finalize ();
            _int_status := Finalized;
    | Finalized -> 
            raise (Error "The parallel libraries where already \
            finalized, and final was called.");
    | Non_Initialized ->
            raise (Error "The parallel libraries where never \
            initialized, and final was called.");;




let init do_inform s ft em =
    match !_int_status with
    | Non_Initialized ->
            _int_status := Initialized;
            _exec_mode := em;
            _is_ft_mode := ft; 
            let result =             
            try
                 Mpi.init s
            with
                | Error m ->
                        raise (Error ("There was an mpi initialization error. \
                        The message is: " ^ m));
            in
            begin match (List.exists (function x -> x = em) (_exec_mode_supp)) 
                with
            | false ->             
                    raise (Error "The requested executions mode (Synchronous \
                    or Asynchronous), is not supported by the communications \
                    library.")
            | true ->
                    begin match ft, em with
                    | Fault_Tolerant, Asynch ->
                            Pervasives.at_exit (final do_inform);
                            Register.register_init ();
                    | Fault_Tolerant, Synch -> 
                            raise (Error "There is no fault tolerance if \
                            performing Synchronous processing. Switch to \
                            Asynchronous");
                    | _, _ -> ()
                    end;
            end;
            result
    | Initialized -> 
            raise (Error "The parallel library is already initialized when \
            init was called.");
    | Finalized -> 
            raise (Error "The parallel library is already finalized when init \
            was called");

(* vim:sw=4 et tw=80
 *)
