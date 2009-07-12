let (-->) a b = b a

let run_script start lst = 
    let lst = PoyCommand.of_parsed false lst in
    Phylo.run ~start lst

let merge_trees a b = 
    { a with Scripting.trees = Sexpr.union a.Scripting.trees b.Scripting.trees }

let get_cost run =
    Sexpr.fold_left (fun acc tree -> 
        min acc (Phylo.PhyloTree.get_cost tree)) (float_of_int max_int)
        run.Scripting.trees

let rec run_with_static_approx_until_no_better run =
    let initial_cost = get_cost run in
    let new_run = run_script run (CPOY swap (transform (static_approx))) in
    let final_cost = get_cost new_run in
    if initial_cost > final_cost then 
        run_with_static_approx_until_no_better new_run
    else run

let build_initial_tree run =
    (run_script run (CPOY build (1))) 
    --> run_with_static_approx_until_no_better

let build_more_trees trees run =
    let n = run_script run (CPOY build ([trees], transform (static_approx))) in
    (n , run)

let improve_trees (to_improve, already_good) =
    let to_improve = Sexpr.map (fun x -> { to_improve with Scripting.trees =
        `Single x }) to_improve.Scripting.trees 
    in
    let to_improve = Sexpr.map run_with_static_approx_until_no_better to_improve in
    let improved = Sexpr.fold_left merge_trees already_good to_improve in
    run_script improved (CPOY swap (constraint_p))

let do_fuse_round run =
    run_script run (CPOY fuse ())

let search_function args run =
    match args with
    | `Empty -> 
            (* We assume that we are using a simple affine gap cost, so we will
            * use this technique *)
            (* 5 rounds of the following *)
            let rec do_iteration iteration run acc =
                if iteration = 0 then acc
                else
                    run
                    --> build_initial_tree 
                    --> build_more_trees 10
                    --> improve_trees
                    --> merge_trees acc
                    --> do_iteration (iteration - 1) run
            in
            let run = do_iteration 30 run run in
            do_fuse_round run
    | _ -> failwith "Usage: search2 (), no more, no less."


let () = Phylo.register_function "search2" search_function
