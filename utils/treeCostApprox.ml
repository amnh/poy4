(* A module to evaluate the goodness of our tree cost calculation *)

type 'a tree = Tree of ('a option * 'a tree * 'a tree) | Leaf of 'a


let balance_and_complete tree =
    let rec tree_depth tree =
        match tree with
        | Leaf _ -> 1
        | Tree (_, a, b) ->
                1 + (max (tree_depth a) (tree_depth b))
    in
    let rec extend_to_at_least depth height tree =
        if depth = height then tree
        else 
            match tree with
            | Leaf x -> 
                    let nt = extend_to_at_least depth (height + 1) tree in
                    Tree (None, nt, nt)
            | Tree (x, y, z) ->
                    Tree (None, extend_to_at_least depth (height + 1) y,
                    extend_to_at_least depth (height + 1) z)
    in
    extend_to_at_least (tree_depth tree) 1 tree

let rec parser_to_tree name_to_sequence parser_tree =
    match parser_tree with
    | Parser.Tree.Leaf x -> Leaf (name_to_sequence x)
    | Parser.Tree.Node ([a; b], _) ->
            Tree (None, parser_to_tree name_to_sequence a, parser_to_tree
            name_to_sequence b)
    | _ -> failwith "What happened with this tree?"

let make_array_of_tree tree = 
    let rec aux_make_array_of_tree acc tree =
        match tree with
        | Leaf x -> x :: acc
        | Tree (_, a, b) ->
                aux_make_array_of_tree (aux_make_array_of_tree acc b) a
    in
    Array.of_list (aux_make_array_of_tree [] tree)

let rec log2 acc v =
    if v = 1 then acc
    else log2 (acc + 1) (v lsr 1)

let generate_optimal_sequence_of_assignments matrix =
    let len = Array.length matrix in 
    if len > 0 then begin
        let minimum = ref max_int 
        and position = ref 0 in
        Array.iteri (fun pos cost -> 
            if cost < !minimum then begin
                minimum := cost;
                position := pos;
            end) matrix.(len - 1);
        Cost_matrix.Two_D.list_of_bits !position (len + 1)
    end else  []

(* A function that generates all the possible assignments in a matrix *)
let generate_assignments df array = 
    Array_ops.randomize array;
    let total_len = Array.length array in
    let log_len = 1 + log2 0 total_len in
    let lifted len pos res prev =
        let d = prev.(pos) + prev.(pos + len) + df array.(pos) array.(pos + len) in
        res.(pos) <- d;
        res.(pos + len) <- d
    in
    let all_lifted len prev res =
        let rec all_lifted len start = 
            if start >= total_len then ()
            else begin
                for i = start to start + len - 1 do
                    lifted len i res prev
                done;
                all_lifted len (start + (2 * len))
            end
        in
        all_lifted len 0
    in
    let res = Array.init log_len (fun _ -> Array.create total_len 0) in
    for i = 0 to log_len - 2 do
        all_lifted (1 lsl i) res.(i) res.(i + 1)
    done;
    (*
    Array.iter (fun x ->
        Array.iter (Printf.printf "%d\t") x;
        print_newline ()) res;
    *)
    res

let calculate_average_assignment df array =
    let matrix = generate_assignments df array in
    let len = Array.length matrix in
    if len > 0 then
        (float_of_int (Array.fold_left ( + ) 0 matrix.(len - 1))) /. 
        (float_of_int (Array.length array))
    else 0.

(* A function that generated a balanced tree *)
let generate_balanced_tree find array = 
    let queue = Queue.create () in
    Array.iter (fun x -> Queue.add x queue) array;
    let rec generate_balanced size =
        if size = 1 then
            Leaf (find (Queue.pop queue))
        else 
            let ns = size / 2 in
            let left = generate_balanced ns
            and right = generate_balanced ns in
            Tree (None, left, right)
    in
    generate_balanced (Array.length array)

type side = Left | Right 

let rec generate_all_sequences len =
    if len = 1 then [[Left]; [Right]]
    else 
        let str = generate_all_sequences (len - 1) in
        List.rev_map (fun x -> Left :: x) str @ List.map (fun x -> Right :: x) str

let rec assign_sequence seq tree df =
    match tree, seq with
    | Leaf a, _ -> a, 0
    | Tree (_, left, right), h :: t ->
            let lefta, leftc = assign_sequence t left df in
            let righta, rightc = assign_sequence t right df in
            let cost = leftc + rightc + df lefta righta in
            let assign =
                match h with
                | Left -> lefta
                | Right -> righta
            in
            assign, cost
    | _ -> failwith "This is possible?"

let rec do_assignment tree df =
    match tree with
    | Leaf a -> tree, a, 0
    | Tree (_, l, r) ->
            let ltree, la, lc = do_assignment l df
            and rtree, ra, rc = do_assignment r df in
            let res_assgn, res_cost = df la ra lc rc in
            Tree (Some res_assgn, ltree, rtree), res_assgn, res_cost


let rec do_aff_assignment tree parent to_single df =
    match tree with
    | Leaf a -> df parent a
    | Tree (Some asgn, lt, rt) ->
            let asgn = to_single parent asgn in
            (do_aff_assignment lt asgn to_single df) +
            (do_aff_assignment rt asgn to_single df) +
            df asgn parent
    | Tree _ -> failwith "you must have assigned already all interior vertices"

let do_aff_assignment tree to_single df =
    match tree with
    | Leaf a -> 0
    | Tree (Some asgn, lt, rt) ->
            let new_asgn = to_single asgn asgn in
            do_aff_assignment (Tree (Some new_asgn, lt, rt)) new_asgn to_single
            df
    | Tree _ -> failwith "you must have assigned already all interior vertices"

let approximation a b =
    (float_of_int (2 * a)) /. b

let print_approx = ref false
let print_do_distr = ref 0

let rec randomize_tree tree =
    match tree with
    | Leaf _ -> tree
    | Tree (_, l, r) ->
            if Random.bool () then
                Tree (None, randomize_tree l, randomize_tree r)
            else Tree (None, randomize_tree r, randomize_tree l)

let gap_opening = ref 0

let sample_for_worst_average to_single pairwise_distance find_sequence do_f 
lifted samples randomize arr =
    let rec find_worst (approx, bestapprox, current_lifted, prev_tree) samples =
        if samples = 0 then (approx, bestapprox, current_lifted, prev_tree)
        else 
            let () = 
                if randomize then Array_ops.randomize arr 
                else ()
            in
            let tree = generate_balanced_tree find_sequence arr in
            let new_current_lifted = calculate_average_assignment lifted arr in
            let new_approx = 
                let rec new_current_do tree iter =
                    let res_tree, _, next_cost = do_assignment tree do_f in
                    let next_cost =
                        if !gap_opening = 0 then next_cost
                        else do_aff_assignment res_tree to_single pairwise_distance
                    in
                    if iter = 0 then next_cost 
                    else
                        let _ = 
                            Printf.printf "%f\t%d\n" new_current_lifted
                            (let c = 10 * (next_cost / 10) in
                            if next_cost - c < 5 then c else c + 5)
                        in
                        new_current_do (randomize_tree tree) (iter - 1)
                in
                approximation (new_current_do tree !print_do_distr) new_current_lifted
            in
            let () =
                if !print_approx then Printf.printf "%f\n" new_approx
            in
            let new_approx, new_lifted_worst_cost, new_lifted_worst_tree =
                if approx < new_approx then
                    new_approx, new_current_lifted, Some tree
                else approx, current_lifted, prev_tree
            in
            find_worst (new_approx, min bestapprox new_approx,
            new_lifted_worst_cost, new_lifted_worst_tree) (samples - 1)
    in
    find_worst (0., max_float, 0., None) samples 

let rec dig_with_do to_single pairwise_distance df times best_cost tree = 
    if times = 0 then best_cost
    else 
        let new_tree = randomize_tree tree in
        let tree, _, new_cost = do_assignment tree df in
        let new_cost =
            if !gap_opening = 0 then new_cost
            else do_aff_assignment tree to_single pairwise_distance
        in
        dig_with_do to_single pairwise_distance df (times - 1) (min (float_of_int new_cost) best_cost) new_tree

(* We have all the functions we need, time to create the actual program *)
let input_trees = ref None
let indel = ref 1
let subst = ref 1
let samples = ref 100
let input = ref []
let digs = ref 100
let print_all_results = ref false
let num_seqs = ref 0
let cost_function = ref "do"

let not_using_input_trees () = 
    match !input_trees with
    | None -> true
    | Some _ -> false

let process_input_trees filename = 
    let trees = Parser.Tree.of_file (`Local filename) in
    input_trees := Some (List.flatten trees)

let define_cost_function f =
    match f with
    | "do" | "single" -> cost_function := f
    | _ -> failwith ("Unknown cost function " ^ f)

let assign r x = r := x

let () = 
    (* Upon initialization, we parse the command line *)
    Random.self_init ();
    let parse_list = [
        ("-cost-function", Arg.String define_cost_function, 
        "The kind of cost function to be used: do, single");
        ("-seqs", Arg.Int (assign num_seqs), 
        "The number of sequences to be selected uniformly at random from the \
        input dataset");
        ("-stats", Arg.Unit (fun () -> print_all_results := true), 
            "Output the worst, best, and dig result");
        ("-print", Arg.Unit (fun () -> print_approx := true),
            "Output all the approximation values");
        ("-indel", Arg.Int (assign indel), "Indel cost, 1 by default");
        ("-subst", Arg.Int (assign subst), "Substitution cost, 1 by default");
        ("-open", Arg.Int (assign gap_opening), "Gap opening cost, 0 by default");
        ("-samples", Arg.Int (assign samples), 
        "Number of samples to be performed, 100 by default");
        ("-digs", Arg.Int (assign digs), 
        "Number of randomized left-right assignments to be performed to check \
        the best performance of DO");
        ("-do-distr", Arg.Int (assign print_do_distr), 
        "Ouptut the do estimation for each random tree for as many times as \
        requested in the argument value, by randomly shuffling the left-right \
        assignment of the vertices in the tree.");
        ("-use-trees", Arg.String process_input_trees,
        "Do not produce random trees, but evaluate the goodness of our \
        estimations using those trees found in the input file.");
    ] in
    let annon_fun (str : string) = 
        input := str :: !input
    in
    Arg.parse parse_list annon_fun "test_do [OPTIONS]* filename [filename]*" 

let () =
    (* First a function that generates a pair of functions that use
    * precalculated matrices to calculate distances *)
    let cm = 
        if 0 = !gap_opening then
            Scripting.DNA.CM.of_sub_indel !subst !indel
        else Scripting.DNA.CM.of_sub_indel_affine !subst !indel !gap_opening
    in
    let generate_functions seqs =
        let initial_array = Array.copy seqs in
        let len = Array.length initial_array in
        let mtx = 
            Array.init len (fun a ->
                let a = seqs.(a) in
                Array.init len (fun b ->
                    let b = seqs.(b) in
                    let deltaw = 
                        let tmp =
                            (max (Sequence.length a) (Sequence.length b)) - (min
                            (Sequence.length a) (Sequence.length b)) 
                        in
                        if tmp > 8 then tmp 
                        else 8
                    in
                    Lazy.lazy_from_fun (fun () -> Sequence.Align.cost_2
                    ~deltaw:deltaw a b cm Matrix.default)))
        in
        (fun a b -> Lazy.force_val mtx.(a).(b)),
        (fun x -> initial_array.(x)),
        (Array.mapi (fun pos _ -> pos) seqs)
    in
    (* Now we are ready to proceed accoring to the parameters, we create the
    * necessary distance functions, but let us first parse the input files *)
    let seqs_n_functions = 
        if not_using_input_trees () then begin
            let seqs = 
                Array.of_list 
                (List.flatten 
                (List.map 
                (fun x -> 
                    let res = Scripting.DNA.Generic.molecular x in
                    List.map (fun (_, seq) -> seq) res) !input))
            in
            Array_ops.randomize seqs;
            let seqs = 
                if 0 >= !num_seqs || !num_seqs >= Array.length seqs then seqs
                else Array.init !num_seqs (fun x -> seqs.(x)) 
            in
            [generate_functions seqs]
        end else begin
            match !input_trees with
            | Some trees ->
                    let table = 
                        let res = Hashtbl.create 1667 in
                        List.iter 
                        (fun x -> 
                            List.iter (fun (name, seq) ->
                            Hashtbl.add res name seq)
                            (Scripting.DNA.Generic.molecular x))
                        !input;
                        res
                    in
                    let seqs = 
                        List.rev_map (fun tree ->
                            make_array_of_tree
                            (balance_and_complete
                            (parser_to_tree (Hashtbl.find table) tree)))
                        trees
                    in
                    List.map generate_functions seqs
            | None -> assert false
        end
    in
    (* Now we generate the required cost matrices and the pair of distance
    * functions for do and sampling the worst average *)
    let do_function a b ca cb =
        let tmpa, tmpb, tmpcost = 
            Sequence.Align.align_2 a b cm Matrix.default
        in
        let seq = Sequence.Align.ancestor_2 tmpa tmpb cm in
        seq, ca + cb + tmpcost
    in
    let to_single a b =
        let s, _ = Sequence.Align.closest a b cm Matrix.default in
        s
    in
    let distance_function a b =
        Sequence.Align.cost_2 a b cm Matrix.default
    in
    let single_function a b ca cb = 
        let seq, cost = do_function a b ca cb in
        let s, _ = Sequence.Align.closest seq seq cm Matrix.default in
        s, cost
    in
    let do_tree_messaging = 1 < List.length seqs_n_functions in
    let process (lifted_function, find, seqs) =
        let approx, best, worst_case_cost, tree =
            let dof =
                if "do" = !cost_function then do_function else single_function
            in
            sample_for_worst_average to_single distance_function find dof 
            lifted_function !samples (not_using_input_trees ()) seqs 
        in
        let tree = 
            match tree with
            | Some t -> t
            | None -> assert false
        in
        let new_cost = 
            dig_with_do to_single distance_function do_function !digs max_float tree 
        in
        let best_attempt = approximation (int_of_float new_cost) worst_case_cost in
        if !print_all_results then
            Printf.printf "%f\t%f\t%f\n" approx best best_attempt
        else ()
    in
    let tree_counter = ref 0 in
    List.iter (fun x ->
        if do_tree_messaging then
            Printf.printf "Processing tree %d\n%!" !tree_counter
        else ();
        incr tree_counter;
        process x) seqs_n_functions
