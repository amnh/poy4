let (-->) a b = b a
let fasta_file = ref ""
let substitution = ref 8
let indel = ref 6
let gap_opening = ref 13
let input_tree = ref ""
let fasta_pattern = ref ""
let tree_pattern = ref ""
let min_counter_pattern = ref 0
let max_counter_pattern = ref 0

let () = 
    let parse_list = [
        ("-fasta", Arg.Set_string fasta_file, "Fasta file");
        ("-tree", Arg.Set_string input_tree, "Input tree");
        ("-substitution", Arg.Set_int substitution, "Substitution cost");
        ("-indel", Arg.Set_int indel, "Indel cost");
        ("-gap_opening", Arg.Set_int gap_opening, "Gap opening");
        ("-fasta_pattern", Arg.Set_string fasta_pattern, 
        "Pattern of fasta files. Every occurrence of NUMBER will be replaced \
        with a number between -min_counter and -max_counter");
        ("-tree_pattern", Arg.Set_string tree_pattern,
        "Same as -fasta_pattern but for the input tree");
        ("-min_counter", Arg.Set_int min_counter_pattern, 
        "The minimum integer to be replaced in the -fasta_pattern \
        and the -tree_pattern");
        ("-max_counter", Arg.Set_int max_counter_pattern, 
        "The maximum integer to be replaced in the -fasta_pattern \
        and the -tree_pattern");
    ]
    in
    Arg.parse parse_list (fun _ -> ()) "masm [OPTIONS]"


module Heap = Heap.Make (struct 
    type t = (int * int) 
    let compare (acnt, acode) (bcnt, bcode) = 
        let cmp = compare acnt bcnt in
        if cmp = 0 then compare acode bcode
        else cmp
end) 

let pick_neighbors tree node a b c =
    let rec recursive_visit parent node counter heap =
        if (Tree.is_leaf node tree) || (Tree.is_single node tree) then
            Heap.insert (counter, node) heap
        else 
            let x, y = Tree.other_two_nbrs parent (Tree.get_node node tree) in
            let heap = recursive_visit node x (counter + 1) heap in
            recursive_visit node y (counter + 1) heap 
    in
    let select_from_heap heap =
        let rec getem lst heap =
            match lst with
            | [a; b; c] -> (a, b, c)
            | x -> getem ((Heap.findMin heap) :: x) (Heap.deleteMin heap)
        in
        getem [] heap
    in
    Heap.empty 
    --> recursive_visit node a 1 
    --> recursive_visit node b 1
    --> recursive_visit node c 1 
    --> select_from_heap

let force_node tree code = 
    let node = Ptree.get_node_data code tree in
    match node.AllDirNode.adjusted with
    | [x] -> AllDirNode.force_val x.AllDirNode.lazy_node
    | _ -> assert false

let compute_center tree code neighbor1 neighbor2 neighbor3 a b c =
    let force_node = force_node tree in
    let nodea = force_node a
    and nodeb = force_node b 
    and nodec = force_node c in
    (* Time to compute the center of the three vertices *)
    let a_node, _ = Node.readjust (`ThreeD None) None nodea nodeb nodec nodea in
    let a_node = AllDirNode.lazy_from_val a_node 
    and fst_dir = Some (a, b)
    and snd_dir = Some (b, c)
    and thr_dir = Some (a, c) in
    let fst_dir = { AllDirNode.lazy_node = a_node; dir = fst_dir; code = code } in
    let snd_dir = { fst_dir with  AllDirNode.dir = snd_dir }
    and thr_dir = { fst_dir with AllDirNode.dir = thr_dir } in
    let unadjusted = [fst_dir; snd_dir; thr_dir] 
    and adjusted = [ { fst_dir with AllDirNode.dir = None } ] in
    { AllDirNode.unadjusted = unadjusted; adjusted = adjusted }


let calculate_distance tree vertex a b c = 
    let force_node = force_node tree in
    let vertex = force_node vertex
    and a = force_node a
    and b = force_node b
    and c = force_node c in
    let distance = Node.Standard.distance 0. in
    (distance vertex a) +. (distance vertex b) +. (distance vertex c)

let rec readjust initial_cost tree = 
    let adjuster _ vertex tree =
        match Ptree.get_node vertex tree with
        | Tree.Interior (_, x, y, z) ->
                let original_distance = calculate_distance tree vertex x y z in
                let center = compute_center tree vertex x y z x y z in
                let new_tree = Ptree.add_node_data vertex center tree in
                let final_distance = calculate_distance new_tree vertex x y z in
                Tree.Continue,
                    if final_distance < original_distance then new_tree
                    else tree
        | _ -> Tree.Continue, tree
    in
    let handles = All_sets.Integers.elements (Ptree.get_handles tree) in
    match handles with
    | [handle] ->
            let fst = Ptree.post_order_node_visit adjuster handle tree tree in
            let snd = Ptree.pre_order_node_visit adjuster handle fst fst in
            let cost = AllDirChar.M.check_cost_all_handles snd in
            if cost < initial_cost then readjust cost snd
            else initial_cost, tree
    | _ -> assert false

let msam_algorithm tree =
    (* We begin by assigning to each interior vertex the median between the
    * closest leaves, if there are many closest, we pick them at random *)
    let initial_of_node node node_data tree = 
        match Ptree.get_node node tree with
        | Tree.Single _
        | Tree.Leaf _ -> tree
        | Tree.Interior (_, n1, n2, n3) ->
                let a, b, c = pick_neighbors tree.Ptree.tree node n1 n2 n3 in
                let ad = snd a
                and bd = snd b 
                and cd = snd c in
                let center = compute_center tree node n1 n2 n3 ad bd cd in
                Ptree.add_node_data node center tree
    in
    let tree = 
        All_sets.IntegerMap.fold initial_of_node tree.Ptree.node_data tree
    in
    fst (readjust (AllDirChar.M.check_cost_all_handles tree) tree)


let process_file ?counter fasta_file input_tree = 
    (* Read the input data and the input tree, we will discard the diagnosis
    * anyway *)
    POY read ([fasta_file])
    transform (tcm:([!substitution], [!indel]), gap_opening:[!gap_opening])
    read ([input_tree]);
    let trees = Phylo.Runtime.trees () in
    match List.map msam_algorithm trees with
    | [] -> ()
    | costs -> 
            let cost = List.fold_left min (max_float) costs in
            match counter with
            | None -> Printf.printf "%f\n%!" cost
            | Some counter -> Printf.printf "%d\t%f\n%!" counter cost

let () = 
    match !fasta_pattern, !tree_pattern with
    | "", _
    | _, "" -> process_file !fasta_file !input_tree
    | fasta_pattern, tree_pattern ->
            let regex = Str.regexp "NUMBER" in
            for counter = !min_counter_pattern to !max_counter_pattern do
                let cnt = string_of_int counter in
                let fasta_file = Str.global_replace regex cnt fasta_pattern
                and tree_file = Str.global_replace regex cnt tree_pattern in
                process_file ~counter fasta_file tree_file;
                POY wipe ();
            done

