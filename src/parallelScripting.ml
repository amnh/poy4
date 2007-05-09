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

(* TODO
* Add proper support for Bremer and Ratchet in the Scripting interpreter of
* Parallel *)

exception Sigabort
exception Sigint
exception Sigkill

module Make (Node : NodeSig.S) (Edge : Edge.EdgeSig with type n = Node.n) (TreeOps : 
    functor (Exact : Ptree.Exact) ->
        Ptree.Tree_Operations with type a = Node.n with type b = Edge.e) 
    (CScrp : CharacterScripting.S with type n = Node.n)
    (P : Parallel.S with type a = Node.n with type b = Edge.e 
            with type c = CScrp.cs with type d = float)

    = struct

    type a = Node.n
    type b = Edge.e
    type c = CScrp.cs

    module S = Scripting.Make (Node) (Edge) (TreeOps) (CScrp)
    module Sup = Supports.Make (Node) (Edge) (TreeOps)
    module BuildExact = 
        Build.Make (Node) (Edge) (TreeOps (struct let exact = true end))
    module BuildInexact = 
        Build.Make (Node) (Edge) (TreeOps (struct let exact = false end))
    module TS = TreeSearch.Make (Node) (Edge) (TreeOps)
    module CT = CharTransform.Make (Node) (Edge) (TreeOps)

let _ =
    (* Signal handlers *)
    let sigabort = fun _ -> raise Sigabort
    and sigint = fun _ -> raise Sigint in
    Sys.set_signal Sys.sigabrt (Sys.Signal_handle sigabort);
    Sys.set_signal Sys.sigint (Sys.Signal_handle sigint)

type trees = Tree.u_tree Sexpr.t

let debug = true

module Debugging = struct
    type state = S.r * trees * int option *
    c Sexpr.t * int option * Scripting.support_class * Scripting.support_class * Methods.support_tree Sexpr.t *
    int

    (* Not intended for the casual user *)
    let get_current_state slave : state =
        let rank = Ftol.rank_of_int slave in
        Ftol.send Methods.debugging `Report_State rank;
        Ftol.recv Methods.debugging rank
end

module Master = struct

    (* There are three basic cases that we will handle, if we will just load
    * trees that have been built already, or if we are going to build a single
    * wagner tree, then there is no need to parallelize. Otherwise, if we have
    * to make n wagner trees then we simply divide the job among the available
    * slaves *)
    let build_initial_trees d (meth : Methods.build) = 
        match meth with
        | `Build (n, meth, _) ->
                Status.user_message Status.Information "Building@ in@ parallel.";
                let filter = function
                    | `Trees x -> x
                    | _ -> failwith "Illegal slave answer"
                in
                let tmp = P.n_tasks "Build" "initial trees" meth n in
                let res = List.map filter tmp in
                `Set res
        | `Prebuilt _  | `Build_Random _ -> failwith "Unsupported"

    let extract_trees run = 
        Sexpr.map (fun x -> x.Ptree.tree) run.Scripting.trees 

    let get_list_for_distribution trees =
        let trees = Sexpr.to_list trees in
        List.map (fun x -> `Single x) trees

    let filter_trees = function
        | `Trees x -> x
        | _ -> failwith "Illegal slave answer"


    (* If we will actually run just one tree local optimum, this will not be
    * parallelized, otherwise, we will perform the operation in parallel *)
    let find_local_optimum run (meth : Methods.local_optimum) : trees = 
        let todo = get_list_for_distribution (extract_trees run) in
        match List.map filter_trees (P.distribute "Optimum" "" meth todo) with
        | [] -> `Empty
        | [tree] -> tree
        | trees -> `Set trees

    let perturbe run (meth : Methods.perturb_method) : trees =
        let todo = get_list_for_distribution (extract_trees run) in
        let res =
            List.map filter_trees
            (P.distribute "Perturbing" "" meth todo)
        in
        `Set res

    let escape_local run (meth : Methods.escape_local) : trees = 
        let lst = get_list_for_distribution (extract_trees run) in
        let res =
            List.map filter_trees
            (P.distribute "Escape local" "" meth lst)
        in
        `Set res

    let run_subscript f run (meth : Methods.script list) (merger : Methods.script list) = 
        let lst = get_list_for_distribution (extract_trees run) in
        let res =
            List.map filter_trees
            (P.distribute "Running Pipeline" "" (`RunForTrees meth) lst)
        in
        let res : Tree.u_tree Sexpr.t = `Set res in
        Sexpr.fold_left (fun acc tree ->
            let run = { run with Scripting.trees = `Single { Ptree.empty
                    with Ptree.tree = tree } } in
            let run = S.update_trees_to_data run in
            List.fold_left f run merger) run res

    let character_operations run meth : (CScrp.cs Sexpr.t, float Sexpr.t) 
            Methods.character_input_output = 
        match meth with
        | `Distance (tx, codes) ->
                (*
                let charsa, charsb = 
                    CharacterScripting.filter_char_operations
                    run.Scripting.nodes run.Scripting.data tx codes
                in
                let shallow_function a b = `Distance (a, b) in
                let task = 
                    Sexpr.shallow_all_to_all shallow_function charsa charsb 
                in
                let filter x =
                    match x with
                    | `Floats r -> r
                    | _ -> 
                            failwith "Unexpected contents in message. \
                            ParallelScripting.character_operations."
                in
                let res = Parallel.distribute_sexpr "Distances" "" task in
                `Floats (Sexpr.flatten (Sexpr.map filter res))
                *)
                `Floats `Empty
        | `Median (tx, codes) ->
                (*
                let charsa, charsb = 
                    CharacterScripting.filter_char_operations 
                    run.Scripting.nodes run.Scripting.data tx codes
                in
                let task = 
                    Sexpr.shallow_all_to_all (fun a b -> `Distance (a, b)) 
                    charsa charsb 
                in
                print_endline "Testing the marshal of every leaf";
                flush stdout;
                Sexpr.leaf_iter (fun x -> ignore (Marshal.to_string x [])) task;
                print_endline "Passed the test, continue run";
                flush stdout;
                let filter x =
                    match x with
                    | `Characters x -> x
                    | _ -> failwith "Unexpected contents in message"
                in
                let res = Parallel.distribute_sexpr "Medians" "" task in
                `Characters (Sexpr.flatten (Sexpr.map filter res))
                *)
                `Characters `Empty

                (*
    let support_values run meth =
        match run.Scripting.trees with
        | `Set lst -> 
                let res = P.distribute "Support" "" meth lst in
                `Set (List.map (function `Support x -> x | _ -> failwith
                "Unexpected slave answer") res)
        | `Empty -> `Empty
        | (`Single _) as t -> 
                match meth with
                | `Bootstrap (1, _, _, _) | `Jackknife (_, 1, _, _, _)  ->
                      Sup.support run.Scripting.nodes meth
                      run.Scripting.data run.Scripting.queue
                | `Bootstrap (it, x, y, z) ->
                        let meth = `Bootstrap (1, x, y, z) in
                        let res = 
                            (P.n_tasks "Support" "iteration" meth it)
                        in
                        let mapper = function
                            | `Support (`Single x) -> 1, x
                            | _ -> failwith "Unexpected slave answer"
                        in
                        let res = List.map mapper res in
                        `Single (Sup.join_support_trees res)
                | `Jackknife (x, it, y, z, w) -> 
                        let meth = `Jackknife (x, 1, y, z, w) in
                        let res = 
                            (P.n_tasks "Support" "iteration" meth it)
                        in
                        let mapper = function
                            | `Support (`Single x) -> 1, x
                            | _ -> failwith "Unexpected slave answer"
                        in
                        let res = List.map mapper res in
                        `Single (Sup.join_support_trees res)
                | `Bremer _ -> 
                        Status.user_message Status.Information "Bremer is \
                        still unsupported in parallel, I will calculate this
                        values sequentially.";
                      Sup.support run.Scripting.nodes t meth run.Scripting.data
                      run.Scripting.queue
                *)

    let process_random_seed_set run v =
        let run = S.process_random_seed_set run v in
        let int = Random.int 1073741823 in
        let f = 
            let c = ref (int + 1) in
            fun p -> 
                incr c;
                let features = [("process", p); ("value", string_of_int !c)] in
                Sadman.start "random_seed" features;
                Sadman.finish [];
                `Random_Seed !c
            in
        P.initialize_random_seed f;
        run

    exception Error_in_Script of (exn * (a, b, c) Scripting.run)

    let update_trees run trees = 
        S.update_trees_to_data { run with Scripting.trees = 
            Sexpr.map (fun x -> { Ptree.empty with Ptree.tree = x }) trees }

    let rec folder run (meth : S.script) = 
        match meth with
        | `Bremer _ | `Jackknife _ | `Bootstrap _ -> failwith "Unsupported"
        | `DataStore _
        | `DataLoad _
        | `DataDiscard _
        | `Entry 
        | `StoreTrees
        | `UnionStored
        | `GetStored -> S.run ~start:run [meth]
        | `ParallelPipeline ((times, todo, composer, continue) as arg) ->
                let run = 
                    P.parallel_pipeline "Running Pipeline" "cool, isn't it?"
                    folder (fun run tree ->
                        let trees = filter_trees tree in
                        let run = { run with Scripting.trees = 
                            (Sexpr.map (fun x -> { Ptree.empty with Ptree.tree = x })
                            trees) } in
                        S.update_trees_to_data run)
                        run arg
                in
                List.fold_left folder run continue
        | `OnEachTree (dosomething, mergingscript) ->
                run_subscript folder run dosomething mergingscript
        | `Repeat (n, comm) ->
                if n < 1 then run 
                else
                    let res = List.fold_left folder run comm in 
                    folder res (`Repeat (n - 1, comm))
        | `SetSeed v -> process_random_seed_set run v 
        | #Methods.tree_handling
        | #Methods.transform
        | #Methods.taxa_handling
        | #Methods.report
        | #Methods.runtime_store
        | #Methods.characters_handling 
        | #Methods.application as meth (* SetSeed was already processed *) ->
                let run = S.run ~start:run [meth] in
                run
        | `ReadScript files ->
                let file_folder run item = 
                    try folder run item with
                    | err -> raise (Error_in_Script (err, run))
                in
                Sexpr.fold_status "Running commands" ~eta:false file_folder run
                (S.read_script_files true files)
        | #Methods.input as meth ->
                Status.user_message Status.Information "I@ will@ read@ input.";
                let run = S.process_input run meth in
                run
        | #Methods.build as meth ->
                Status.user_message Status.Information "Will@ start@ building";
                let trees = build_initial_trees run meth in
                update_trees run trees
        | #Methods.perturb_method as meth ->
                Status.user_message Status.Information "I@ shall@ perturb.";
                let trees = perturbe run meth in
                update_trees run trees
        | `Fusing params ->
              (* TODO: real parallelization in fusing *)
              Status.user_message Status.Information
                  "No@ fusing@ in@ parallel@ currently.@ Fusing@ in@ sequence.";
              let trees = TS.fusing run.Scripting.data
              run.Scripting.queue run.Scripting.trees params in
              { run with Scripting.trees = trees }
        | #Methods.local_optimum as meth ->
                Status.user_message Status.Information "I@ will@ find@ local@ \
                optimum.";
                let trees = find_local_optimum run meth in
                update_trees run trees
        | #Methods.char_operations as meth -> 
                Status.user_message Status.Information "Calculating character \
                operations";
                let characters = character_operations run meth in
                { run with Scripting.characters = characters }
                (* TODO
        | #Methods.support_method as meth ->
                Status.user_message Status.Information "Calculating@ tree@ \
                support";
                let support = support_values run meth in
                { run with Scripting.support = support }
                *)
        | #Methods.escape_local as meth ->
                Status.user_message Status.Information "I@ shall@ escape@ \
                local.";
                let trees = escape_local run meth in
                update_trees run trees 
        | `Synchronyze ->
                let trees = extract_trees run in
                P.broadcast (`DataNTrees (run.Scripting.data,
                !Data.median_code_count, trees));
                run

    let run ?(start=S.empty) (script : S.script list) = 
        let script = Analyzer.parallel_analysis true script in
        S.run ~folder:folder ~start:start script

end

module Slave = struct

    let extract_topologies trees = Sexpr.map (fun x -> x.Ptree.tree) trees

    let trees : (a, b) Ptree.p_tree Sexpr.t ref = ref `Empty
    let data = ref (Data.empty ())
    let nodes : Node.n list ref = ref []
    let characters : CScrp.cs Sexpr.t ref = ref `Empty
    let active_tree : (a, b) Ptree.p_tree Sexpr.t ref = ref `Empty
    let active_tree_pos : int option ref = ref None
    let active_character : CScrp.cs Sexpr.t ref = ref `Empty
    let active_character_pos : int option ref = ref None
    let bremer_support : Methods.support_tree Sexpr.t ref = ref `Empty
    let jackknife_support : Scripting.support_class ref = ref None
    let bootstrap_support : Scripting.support_class ref = ref None
    let runtime_store : (a, b, c) Scripting.run All_sets.StringMap.t ref = 
        ref All_sets.StringMap.empty
    let data_store : Data.d All_sets.StringMap.t ref = ref
        All_sets.StringMap.empty
    let queue : (a, b) Sampler.ft_queue = Sampler.create ()
    let seed : int ref = ref 0

    let make_run () : S.r =
        { 
            Scripting.description = None;
            trees = !active_tree;
            queue = queue;
            data = !data;
            nodes = !nodes;
            characters = `Floats `Empty;
            bremer_support = !bremer_support;
            runtime_store = !runtime_store;
            bootstrap_support = !bootstrap_support;
            jackknife_support = !jackknife_support;
            stored_trees = `Empty;
            data_store = !data_store;

        }

    let get_run r =
        trees := r.Scripting.trees;
        data := r.Scripting.data;
        nodes := r.Scripting.nodes;
        characters := `Empty;
        bremer_support := r.Scripting.bremer_support;
        jackknife_support := r.Scripting.jackknife_support;
        bootstrap_support := r.Scripting.bootstrap_support;
        runtime_store := r.Scripting.runtime_store;
        data_store := r.Scripting.data_store

    let ( --> ) a b = b a

    let set_internal_data meth =
        match meth with
        | `DataNTrees (d, internal_code, t) ->
                trees := Sexpr.map (fun x -> { Ptree.empty with Ptree.tree = x }) t;
                active_tree := !trees;
                if debug then 
                    print_endline ("I have in total " ^ string_of_int
                    (Sexpr.length !trees));
                Data.median_code_count := internal_code;
                let data', nodes' = Node.load_data d in
                data := data';
                nodes := nodes';
                let run = S.update_trees_to_data (make_run ()) in
                nodes := run.Scripting.nodes;
                trees := run.Scripting.trees;
                active_tree := !trees;
        | `Data (d, internal_code) ->
                if debug then 
                    print_endline "Received data";
                Data.median_code_count := internal_code;
                let data', nodes' = Node.load_data d in
                data := data';
                nodes := nodes';
                let run = S.update_trees_to_data (make_run ()) in
                trees := run.Scripting.trees;
                active_tree := !trees;
        | `Trees t ->
                if debug then 
                    print_endline "Received trees";
                trees := Sexpr.map (fun x -> { Ptree.empty with Ptree.tree = x }) t;
                active_tree := !trees;
                active_tree_pos := None;
                let res = S.update_trees_to_data (make_run ()) in
                trees := res.Scripting.trees;
                active_tree := !trees;
        | `Support v -> 
                failwith "Unsupported"
(*                bremer_support := v;*)
        | `Characters _ | `Floats _ -> 
                (* TODO *)
                failwith "Slaves do not expect this input from the master yet."
        | `Random_Seed s ->
                seed := s;
                Random.init s

    let build_trees (meth : Methods.parallelizable_build) : unit = 
        let cc = 
            match meth with
            | `Wagner_Rnd (_, _, cc) 
            | `Wagner_Ordered (_, _, cc)  -> cc
            | `Prebuilt _ 
            | `Build_Random _ -> failwith "Unsupported"
        in
        let meth = `Build (1, meth, cc) in
        let trees = 
            if Scripting.build_has_exact meth then
                BuildExact.build_initial_trees !data !nodes meth 
            else 
                BuildInexact.build_initial_trees !data !nodes meth 
        in
        P.answer (`Trees (extract_topologies trees))

    let set_active_tree_in_trees () =
        match !active_tree_pos with
        | None 
        | Some 0 -> trees := !active_tree
        | Some n -> 
                match !trees with
                | `Set lst ->
                        let rec replacer it v lst =
                            if it > 0 then begin
                                match lst with
                                | h :: t ->
                                        h :: (replacer (it - 1) v t)
                                | [] -> 
                                        Status.user_message Status.Error
                                        "set_active_tree_in_trees 1";
                                        failwith "Unexpected"
                            end else begin
                                match lst with
                                | _ :: t -> v :: t
                                | [] -> 
                                        Status.user_message Status.Error
                                        "set_active_tree_in_trees 2";
                                        failwith "Unexpected"
                            end
                        in
                        trees := `Set (replacer n !active_tree lst);
                | `Empty | `Single _ -> 
                        Status.user_message Status.Error
                        "set_active_tree_in_trees 3";
                        failwith "Unexpected"

    let apply_method_on_active_trees f meth =
        let tmpd = 
            { S.empty with Scripting.trees = !active_tree; 
            data = !data; nodes = !nodes }
        in
        let res = f tmpd meth in
        active_tree := res;
        P.answer (`Trees (extract_topologies res))

    let extract_character = function
        | `OfNodes c ->
                let c = List.map (fun x -> `Single (CScrp.extract_character c x)) !nodes in
                characters := `Set c;
                active_character := !characters;
                active_character_pos := None

    let rec process_message (meth : P.checkpoint) = 
        match meth with
        | (`ParallelPipeline _) as meth ->
                let run = make_run () in
                let run = S.run ~start:run [meth] in
                P.answer (`Trees (extract_topologies run.Scripting.trees))
        | `RunForTrees meth ->
                let run = make_run () in
                let run = S.run ~start:run meth in
                P.answer (`Trees (extract_topologies run.Scripting.trees))
        | #Methods.parallel_input as meth -> 
                if debug then begin
                    print_endline "Receiving parallel input ";
                    flush stdout;
                end;
                set_internal_data meth;
        | #Methods.parallelizable_build as meth -> 
                if debug then begin
                    print_endline "Receiving parallel build ";
                    flush stdout;
                end;
                build_trees meth
        | #Methods.local_optimum as meth -> 
                if debug then begin
                    print_endline "Receiving parallel optimum ";
                    flush stdout;
                end;
                let run = make_run () in
                let run = S.run ~start:run [meth] in
                let res = run.Scripting.trees in
                active_tree := res;
                P.answer (`Trees (extract_topologies res))
        | `Bootstrap (it, _, _, _) as meth -> 
              let sup = Sup.support !nodes meth !data queue in
              bootstrap_support := Some (it, sup);
              (* TODO  all support values
              P.answer (`Support sup)
              *)
        | `Jackknife (_, it, _, _, _) as meth -> 
              let sup = Sup.support !nodes meth !data queue in
              jackknife_support := Some (it, sup);
        | #Methods.perturb_method as meth ->
                if debug then begin
                    print_endline "Receiving parallel perturbation ";
                    flush stdout;
                end;
                let res = CT.perturbe !data !active_tree meth in
                active_tree := res;
                P.answer (`Trees (extract_topologies res))
        | #Methods.escape_local as meth ->
                if debug then begin
                    print_endline "Receiving parallel perturbation ";
                    flush stdout;
                end;
                let res = CT.escape_local !data queue !active_tree meth in
                active_tree := res;
                P.answer (`Trees (extract_topologies res))
        | #Methods.build as meth -> 
                begin try
                    let res =
                        if Scripting.build_has_exact meth then
                            BuildExact.build_initial_trees !data !nodes meth 
                        else 
                            BuildInexact.build_initial_trees !data !nodes meth
                    in
                    active_tree := res;
                    trees := res;
                    active_tree_pos := None;
                    P.answer (`Trees (extract_topologies res));
                with
                | err -> 
                        Status.user_message Status.Error
                        "Uncatchet 4";
                        raise err
                end;
        | #Methods.extract_characters as meth -> 
                if debug then begin
                    print_endline "Extracting a character";
                    flush stdout;
                end;
                extract_character meth
        | #Methods.character_operations as meth ->
                if debug then begin
                    print_endline "Calculating character operation";
                    flush stdout;
                end;
                P.answer (CScrp.character_operations meth)
        | `Item (it, meth) -> 
                if debug then begin
                    print_endline ("Received item " ^ string_of_int it ^ " to process");
                    print_endline ("I currently have " ^ string_of_int
                    (Sexpr.length !trees) ^ " trees in memory");
                    flush stdout;
                end;
                begin try
                    active_tree := `Single (Sexpr.nth it !trees);
                    active_tree_pos := Some it;
                    process_message meth
                with
                | Failure msg -> 
                        if debug then
                            print_endline msg;
                        Status.user_message Status.Error
                        "process_message 3";
                        Status.user_message Status.Error
                        (msg ^ string_of_int it);
                        failwith  "I don't have such tree."
                | err ->
                        P.report_exception "I had an error \
                        while attempting to process a tree." err
                        `Caught;
                end;
        | #Methods.input -> ()

    let waiting_time = 1

    exception Testing

    let process_io_message (msg : Methods.io) =
        match msg with
        | `Verbosity x -> Status.set_verbosity x
        | _ -> failwith "Unexpected message"

    let process_management_message (msg : Methods.parallel_special_condition) =
        match msg with
        | `Cleanup -> 
                trees := `Empty;
                data := Data.empty ();
                nodes := [];
                characters := `Empty;
                active_tree := `Empty;
                active_tree_pos := None;
                active_character := `Empty;
                active_character_pos := None;
                Status.user_message Status.Status "Deallocated all possible
                memory";
        | _ -> P.send_dead_signal_and_exit P.Dont

    let process_debugging_message = function
        | `Report_State ->
                let run : S.r =
                    { S.empty with Scripting.data = !data; 
                    nodes = !nodes }
                in
                let res : Debugging.state = 
                    run, extract_topologies !active_tree, !active_tree_pos, !characters,
                    !active_character_pos, !bootstrap_support,
                    !jackknife_support, !bremer_support, !seed
                in
                Ftol.send Methods.debugging res (Ftol.rank_of_int 0)

    let run_slave ?(waiting_timeiting_time) () : unit = 
        let rank = Ftol.rank_of_int 0 in
        try
            while true do
                try
                    Timer.nanosleep 0 250000000.;
                    match Ftol.prob Ftol.any_tag rank with
                    | Some (tag, rank) when tag = Methods.do_job -> 
                            if debug then begin
                                print_endline "I received a message to do something";
                                flush stdout;
                            end;
                            process_message (Ftol.recv tag rank);
                            if debug then begin
                                print_endline "I finished doing it";
                                flush stdout;
                            end;
                    | Some (tag, rank) when tag = Methods.process_management -> 
                            process_management_message (Ftol.recv tag rank);
                    | Some (tag, rank) when tag = Methods.io ->
                            process_io_message (Ftol.recv tag rank);
                    | Some (tag, rank) when tag = Methods.debugging ->
                            process_debugging_message (Ftol.recv tag rank)
                    | Some (tag, rank) -> 
                            if debug then begin
                                Status.user_message Status.Error 
                                ("Received a message with unkown tag " ^
                                string_of_int tag);
                                flush stdout;
                            end
                    | None -> ()
                with
                | Sys.Break -> ()
                | err ->
                        P.report_exception (Printexc.to_string err) err 
                        `Caught
            done;
        with
        | Sys.Break -> 
                P.send_dead_signal (P.Do ("Interrupt",
                Sys.Break));
                while true do
                    Timer.nanosleep 0 250000000.;
                    match Ftol.prob Methods.process_management rank with
                    | Some (tag, rank) ->
                            process_management_message (Ftol.recv tag rank);
                    | None -> ()
                done
        | Sigabort | Sigint as err ->
                P.send_dead_signal (P.Do ("Received a signal",
                err));
                while true do
                    Timer.nanosleep 0 250000000.;
                    match Ftol.prob Methods.process_management rank with
                    | Some (tag, rank) ->
                            process_management_message (Ftol.recv tag rank);
                    | None -> ()
                done

end
    end

