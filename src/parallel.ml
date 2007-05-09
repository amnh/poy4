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

let () = SadmanOutput.register "Parallel" "$Revision: 1644 $"

module type S = sig
    type a 
    type b 
    type c
    type d

    type inform_master = Do of (string * exn) | Dont
    type checkpoint = 
        (Tree.u_tree, Data.d, d, c) Methods.checkpoints

    type parallel_results = 
        (Tree.u_tree Sexpr.t, Data.d, c Sexpr.t, d Sexpr.t)
        Methods.parallel_input

    type parallel_results_data = parallel_results list

    val send_dead_signal_and_exit : inform_master -> unit
    val send_dead_signal : inform_master -> unit

    val n_tasks :
      string -> string ->
          [< checkpoint ] -> int -> parallel_results_data

    val distribute :
      string -> string -> [< checkpoint ] -> Tree.u_tree Sexpr.t list -> parallel_results_data 

    val broadcast : [< checkpoint] -> unit

    val broadcast_verbosity : Methods.verbosity -> unit

    val parallel_pipeline :  
        string ->
            string ->
                ((a, b, c) Scripting.run -> Methods.script -> (a, b, c)
                Scripting.run) ->
                    ((a, b, c) Scripting.run ->
                        (Tree.u_tree Sexpr.t, Data.d, c Sexpr.t,
                        d Sexpr.t)
                        Methods.parallel_input -> (a, b, c) Scripting.run) ->
                            (a, b, c) Scripting.run 
                            -> int * Methods.script list * Methods.script list *
                            Methods.script list -> (a, b, c) Scripting.run

    val answer : [< checkpoint] ->
        unit

    val all_down : unit -> unit

    val distribute_sexpr : string -> string ->
        checkpoint Sexpr.t -> checkpoint Sexpr.t

    val process_error_message : Methods.parallel_special_condition -> unit

    val report_exception : string -> exn -> [< `Caught | `Uncaught ] -> unit

    val initialize_random_seed : (string -> [`Random_Seed of int]) -> unit

end

module Make (Node : NodeSig.S) (Edge : Edge.EdgeSig with type n = Node.n) 
    (CScrp : CharacterScripting.S with type n = Node.n) 
    (P : Process.S with type a = Node.n with type b = Edge.e with type c =
        CScrp.cs with type d = float)
    = struct

    exception Unknown 

    type a = Node.n
    type b = Edge.e
    type c = CScrp.cs
    type d = float

    type checkpoint = P.checkpoint

    type parallel_results = 
        (Tree.u_tree Sexpr.t, Data.d, c Sexpr.t, d Sexpr.t)
        Methods.parallel_input

    type parallel_results_data = 
        (Tree.u_tree Sexpr.t, Data.d, c Sexpr.t, 
        d Sexpr.t) Methods.parallel_input list

    let debug = false

    let _waiting_parameter = 1 

    let test_for_alive_and_wait ?(time=_waiting_parameter) () =
        ()
        (*
        if Process.any_alive () then Unix.sleep time
        else begin
            Status.user_message Status.Error "No processes are left alive.";
            raise Process.All_Children_Dead
        end
        *)

    type inform_master = Do of (string * exn) | Dont

    let send_dead_signal inform_master = 
        match inform_master with
        | Do (msg, err) ->
            let msg = `Uncaught (Ftol.my_rank, msg, err) in
            Ftol.send Methods.process_management msg (Ftol.rank_of_int 0)
        | Dont -> Comm.dont_inform_dead_to_anyone ()

    let send_dead_signal_and_exit inform_master =
        send_dead_signal inform_master;
        Pervasives.exit 0

    let process_io_message (msg : Methods.io) =
        let t, msg, rank =
            match msg with
            | `Status (msg, rank) -> Status.Status, msg, rank
            | `Error (msg, rank) -> Status.Error, msg, rank
            | `Information (msg, rank) -> Status.Information, msg, rank
            | `Output (msg, rank) -> (Status.Output (None, false, [])), msg, rank
        in
        let msg = "Message@ from@ " ^ string_of_int rank ^ "@ @[" ^ msg 
            ^ "@]" in
        Status.user_message t msg 

    let process_error_message (msg : Methods.parallel_special_condition) =
        let printer rank msg ex = 
            "@ :@ @[" ^ string_of_int rank ^ "@ :@ " ^ msg ^ 
            "@ with@ exception@ @[" ^ Printexc.to_string ex ^ "@]@]"
        in
        match msg with
        | `Caught (rank, msg, ex) -> 
                Status.user_message Status.Error (printer rank msg ex);
        | `Uncaught (rank, msg, ex) ->
                Status.user_message Status.Error (printer rank msg ex);
        | `CleanExit rank -> 
                ignore (P.dead_proc (Ftol.rank_of_int rank))
        | `Unknown rank ->
                let msg = "Unknown@ crash@ in@ process." in
                Status.user_message Status.Error (printer rank msg Unknown);
                ignore (P.dead_proc (Ftol.rank_of_int rank))
        | `Cleanup -> ()

    let n_tasks name message msg n =
        let status = Status.create name (Some n) message in
        let jobs_to_finish = ref n
        and jobs_running = ref 0 
        and res = ref [] in
        let report () = 
            Status.full_report ~adv:(n - !jobs_to_finish) status
        in
        let send_job_to_slave () =
            if debug then
                Status.user_message Status.Information "I'm@ going@ to@ \
                send@ a@ parallel@ job.";
            match P.get_proc Methods.do_job msg with
            | Some rank ->
                    if debug then
                        Status.user_message Status.Information 
                        "Sending@ task";
                    Ftol.send Methods.do_job msg rank;
                    incr jobs_running;
                    decr jobs_to_finish;
            | None -> ()
        in
        let check_if_results_arrived () =
            match Ftol.prob Ftol.any_tag Ftol.any_source with
            | Some (tag, rank) when tag = Methods.do_job ->
                    res := (Ftol.recv tag rank) :: !res;
                    decr jobs_running;
                    let _ = P.release_proc rank in
                    report ();
                    ()
            | Some (tag, rank) when tag = Methods.process_management ->
                    (* TODO: Potential error if the dead process was doing nothing
                    * *)
                    process_error_message (Ftol.recv tag rank);
                    incr jobs_to_finish;
                    decr jobs_running;
                    report ();
                    ()
            | Some (tag, rank) when tag = Methods.io ->
                    process_io_message (Ftol.recv tag rank);
            | Some (tag, _) when tag = Methods.debugging ->
                    ()
            | Some _ -> ()
            | None -> Timer.nanosleep 0 250000000.;
        in
        while !jobs_to_finish > 0 || !jobs_running > 0 do
            if debug then 
                Status.user_message Status.Information ("jobs_to_finish@ =@  " ^
                string_of_int !jobs_to_finish ^ "@ jobs_running@ =@ " ^
                string_of_int !jobs_running);
            match P.num_avail_proc () with
            | 0 -> 
                    Timer.nanosleep 0 250000000.;
                    test_for_alive_and_wait ();
                    check_if_results_arrived ();
            | _ ->
                    if !jobs_to_finish > 0 then
                        send_job_to_slave ();
                    check_if_results_arrived ();
        done;
        if debug then
            Status.user_message Status.Information 
            ("Total@ number@ of items@ that@ I@ received@ " ^ 
            string_of_int (List.length !res));
        !res

    let distribute name message (meth : [< checkpoint]) (items : Tree.u_tree Sexpr.t list) 
        : parallel_results_data = 
        let jobs = Array.mapi (fun p x -> (p, x)) (Array.of_list items) in
        let queue = Queue.create () in
        let jobs_to_finish = Array.length jobs
        and jobs_running = ref 0 in
        let status = Status.create name (Some jobs_to_finish) message in
        let report () = 
            Status.full_report ~adv:(jobs_to_finish - (Queue.length queue)) status
        in
        let res = Array.make jobs_to_finish None in
        for i = 0 to (jobs_to_finish - 1) do
            Queue.add i queue;
        done;
        let send_job_to_slave () =
            let item = Queue.pop queue in
            let msg = (`Item (item, (meth :> checkpoint))) in
            match P.get_proc Methods.do_job msg with
            | Some rank -> 
                    Ftol.send Methods.do_job msg rank;
                    incr jobs_running;
            | None -> ()
        in
        let check_if_results_arrived () =
            match Ftol.prob Ftol.any_tag Ftol.any_source with
            | Some (tag, rank) when tag = Methods.do_job ->
                    begin match P.get_checkpt rank with
                    | Some (`Item (pos, _)) -> 
                            decr jobs_running;
                            res.(pos) <- Some (Ftol.recv tag rank);
                            let _ = P.release_proc rank in
                            report ();
                            ()
                    | _ -> failwith "Unexpected message"
                    end;
            | Some (tag, rank) when tag = Methods.process_management ->
                    process_error_message (Ftol.recv tag rank);
                    begin match P.get_checkpt rank with
                    | Some (`Item (pos, _)) -> 
                            Queue.push pos queue;
                            decr jobs_running;
                            report ();
                    | _ -> ()
                    end;
                    ()
            | Some (tag, rank) when tag = Methods.io ->
                    process_io_message (Ftol.recv tag rank);
            | Some (tag, _) when tag = Methods.debugging ->
                    ()
            | Some _ | None -> Timer.nanosleep 0 250000000. 
        in
        while (not (Queue.is_empty queue)) || !jobs_running > 0 do
            match P.num_avail_proc () with
            | 0 -> 
                    Timer.nanosleep 0 250000000.;
                    test_for_alive_and_wait ();
                    check_if_results_arrived ();
            | _ ->
                    if (not (Queue.is_empty queue)) then
                        send_job_to_slave ();
                    check_if_results_arrived ();
        done;
        let cleaner = function
            | Some x -> x
            | None -> failwith "Missing job";
        in
        Array.to_list (Array.map cleaner res)

    let parallel_pipeline name message f proc acc (times, todo, composer, continue) =
        let acc = ref acc in
        let results = Stack.create () in
        let jobs_to_finish = ref times in
        if debug then
            Status.user_message Status.Information ("I@ have@ " ^ string_of_int
            times ^ "@ jobs@ to@ finish.");
        let status = Status.create name (Some !jobs_to_finish) message in
        let report () =
            Status.full_report ~adv:(times - !jobs_to_finish) status
        in
        let jobs_running = ref 0 in
        let item = (`ParallelPipeline (1, todo, composer, [])) in
        let send_job_to_slave () =
            match P.get_proc Methods.do_job item with
            | Some rank -> 
                    Ftol.send Methods.do_job item rank;
                    incr jobs_running;
            | None -> ()
        in
        let process_results () =
            let did_something = not (Stack.is_empty results) in
            while not (Stack.is_empty results) do
                report ();
                let nacc = proc !acc (Stack.pop results) in
                let nacc = List.fold_left f nacc composer in 
                acc := nacc;
            done;
            did_something
        in
        let check_if_results_arrived () =
            match Ftol.prob Ftol.any_tag Ftol.any_source with
            | Some (tag, rank) when tag = Methods.do_job ->
                    begin match P.get_checkpt rank with
                    | Some (`ParallelPipeline  _) -> 
                            decr jobs_to_finish;
                            decr jobs_running;
                            Stack.push (Ftol.recv tag rank) results;
                            let _ = P.release_proc rank in
                            report ();
                            ()
                    | _ -> failwith "Unexpected message"
                    end;
            | Some (tag, rank) when tag = Methods.process_management ->
                    process_error_message (Ftol.recv tag rank);
                    begin match P.get_checkpt rank with
                    | Some (`ParallelPipeline _) -> 
                            decr jobs_running;
                            report ()
                    | Some _ -> failwith "Unexpected checkpoint"
                    | _ -> ()
                    end;
                    ()
            | Some (tag, rank) when tag = Methods.io ->
                    process_io_message (Ftol.recv tag rank);
            | Some (tag, _) when tag = Methods.debugging ->
                    ()
            | Some _ -> failwith "What is this message?"
            | None -> 
                    if not (process_results ()) then
                        Timer.nanosleep 0 250000000.
                    else ()
        in
        while (!jobs_to_finish > 0) || (!jobs_running > 0) do
            check_if_results_arrived ();
            match P.num_avail_proc () with
            | 0 -> 
                    if not (process_results ()) then
                        Timer.nanosleep 0 250000000.
                    else ();
                    test_for_alive_and_wait ();
            | _ ->
                    if !jobs_to_finish > 0 then
                        send_job_to_slave ()
        done;
        let _ = process_results () in
        Status.finished status;
        !acc

    let distribute_items_list name message 
        (items : [ `Item of (int * (Tree.u_tree, Data.d, d, c) Methods.checkpoints)] list) = 
        let queue = Stack.create () in
        List.iter (fun x -> Stack.push x queue) items;
        let jobs_to_finish = Stack.length queue in
        if debug then
            Status.user_message Status.Information ("I@ have@ " ^ string_of_int jobs_to_finish
            ^ "@ jobs@ to@ finish.");
        let res = Array.make jobs_to_finish None in
        let status = Status.create name (Some jobs_to_finish) message in
        let report () =
            Status.full_report ~adv:(jobs_to_finish - (Stack.length queue)) status
        in
        let jobs_running = ref 0 in
        let send_job_to_slave () =
            let (`Item (w, m)) as item = Stack.pop queue in
            match P.get_proc Methods.do_job item with
            | Some rank -> 
                    Ftol.send Methods.do_job m rank;
                    incr jobs_running;
            | None -> ()
        in
        let check_if_results_arrived () =
            match Ftol.prob Ftol.any_tag Ftol.any_source with
            | Some (tag, rank) when tag = Methods.do_job ->
                    begin match P.get_checkpt rank with
                    | Some (`Item (pos, _)) -> 
                            decr jobs_running;
                            if debug then 
                                Status.user_message Status.Information ("Storing@ \
                                position@ " ^ string_of_int pos);
                            res.(pos) <- Some (`Item (pos, (Ftol.recv tag rank)));
                            let _ = P.release_proc rank in
                            report ();
                            ()
                    | _ -> failwith "Unexpected message"
                    end;
            | Some (tag, rank) when tag = Methods.process_management ->
                    process_error_message (Ftol.recv tag rank);
                    begin match P.get_checkpt rank with
                    | Some ((`Item _) as x) -> 
                            Stack.push x queue;
                            decr jobs_running;
                            report ()
                    | Some _ -> failwith "Unexpected checkpoint"
                    | _ -> ()
                    end;
                    ()
            | Some (tag, rank) when tag = Methods.io ->
                    process_io_message (Ftol.recv tag rank);
            | Some (tag, _) when tag = Methods.debugging ->
                    ()
            | Some _ -> failwith "What is this message?"
            | None -> Timer.nanosleep 0 250000000.;
        in
        while (not (Stack.is_empty queue)) || !jobs_running > 0 do
            check_if_results_arrived ();
            match P.num_avail_proc () with
            | 0 -> 
                    Timer.nanosleep 0 250000000.;
                    test_for_alive_and_wait ();
            | _ ->
                    if (not (Stack.is_empty queue)) then
                        send_job_to_slave ()
        done;
        Array.to_list (Array.map (function Some x -> x | None -> failwith "Missing
        job") res)

    let broadcast msg =
        let all_slaves = !(Ftol.list_of_slaves) in
        List.iter (Ftol.send Methods.do_job (msg :> checkpoint)) all_slaves;
        if debug then
            Status.user_message Status.Information "Finished@ broadcasting."

    let broadcast_verbosity msg =
        let all_slaves = !(Ftol.list_of_slaves) in
        List.iter (Ftol.send Methods.io msg) all_slaves;
        if debug then
            Status.user_message Status.Status "Finished@ broadcasting."


    let answer msg =
        Ftol.send Methods.do_job msg (Ftol.rank_of_int 0)

    let report_exception msg exp kind =
        let msg =
            match kind with
            | `Caught -> `Caught (Ftol.int_of_rank (Ftol.my_rank ()), msg, exp)
            | `Uncaught -> `Uncaught (Ftol.int_of_rank (Ftol.my_rank ()), msg, exp)
        in
        Ftol.send Methods.process_management msg (Ftol.rank_of_int 0)

    let report_normal_exit () =
        Ftol.send Methods.process_management `CleanExit (Ftol.rank_of_int 0)

    let all_down () =
        let all_slaves = !(Ftol.list_of_slaves) in
        List.iter (Ftol.send Methods.process_management `CleanExit) all_slaves

    let distribute_sexpr name message (sexpr : checkpoint Sexpr.t) = 
        if debug then 
            Status.user_message Status.Information "I@ will@ distribute@ a@ sexpr.";
        let counter = 
            let c = ref (-1) in
            fun v -> 
                incr c;
                `Item (!c, v)
        in
        let new_sexpr = Sexpr.map counter sexpr in
        let todo = Sexpr.to_list new_sexpr in
        let did = distribute_items_list name message todo in
        if debug then
            Status.user_message Status.Information "I@ finished@ distributing@ \
            the@ sexpr.";
        let did_stack = Queue.create () in
        List.iter (fun x -> Queue.add x did_stack) did;
        Sexpr.map (fun _ -> 
            match Queue.pop did_stack with
            | `Item (_, meth) -> meth | _ -> failwith "Unexpected") new_sexpr

    let initialize_random_seed (f : string -> [`Random_Seed of int]) =
        let all_slaves = !(Ftol.list_of_slaves) in
        let send v =
            let vi = Ftol.int_of_rank v in
            let vi = string_of_int vi in
            let s = f vi in
            Ftol.send Methods.do_job s v
        in
        List.iter send all_slaves;
        if debug then
            Status.user_message Status.Status "Finished@ broadcasting."
end
