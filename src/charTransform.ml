(* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    *)
(* Copyright (C) 2007  Andr�s Var�n, Le Sy Vinh, Illya Bomash, Ward Wheeler,  *)
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

(* $Id: charTransform.ml 2556 2008-01-17 22:51:53Z andres $ *)
(* Created Fri Jan 13 11:22:18 2006 (Illya Bomash) *)

(** CharTransform implements functions for transforming the set of OTU
    characters, including keeping a stack of transformations, reverting
    transformations, and applying a transformation or reverse-transformation to
    a tree. *)

let () = SadmanOutput.register "CharTransform" "$Revision: 2556 $"

let check_assertion_two_nbrs a b c =
    if a <> Tree.get_id b then true
    else 
        let _ = Status.user_message Status.Error c in
        false

module type S = sig
    type a 
    type b
    type tree = (a, b) Ptree.p_tree

    val escape_local : 
        Data.d -> Sampler.ft_queue ->
            tree Sexpr.t -> Methods.escape_local -> tree Sexpr.t

    val filter_characters : tree -> int list -> tree

    val perturbate_in_tree : Methods.perturb_method -> Data.d ->
        tree -> Data.d * tree

    val perturbe : 
        Data.d -> tree Sexpr.t -> Methods.perturb_method -> tree Sexpr.t

    val replace_nodes : a list -> tree -> tree

    (** [substitute_nodes nodes tree] replaces the nodes in tree with the nodes with
        the same taxon code from the list *)
    val substitute_nodes : a list -> tree -> tree

    (** [transform_tree f tree] applies a transformation function [f] on all the
        leaves of [tree] and returns the updated tree *)
    val transform_tree :
        (a -> a) -> tree -> tree

    val transform_nodes :
      tree Sexpr.t ->
      Data.d -> a list -> Methods.char_transform list -> Data.d * a list

end

exception No_trees

let select_shortest trees = 
    let choose acc tree = 
        let nc = Ptree.get_cost `Adjusted tree in
        match acc with
        | Some (cost, _) ->
                if nc < cost then Some (nc, tree)
                else acc
        | None -> Some (nc, tree)
    in
    match Sexpr.fold_left choose None trees with
    | Some (_, t) -> t
    | None -> raise No_trees

module Make (Node : NodeSig.S) (Edge : Edge.EdgeSig with type n = Node.n) 
    (TreeOps : 
        Ptree.Tree_Operations with type a = Node.n with type b = Edge.e)
    : S with type a = Node.n with type b = Edge.e = struct

    module IA = ImpliedAlignment.Make (Node) (Edge)

    type a = Node.n
    type b = Edge.e
    type tree = (a, b) Ptree.p_tree
    type ts = Node.n list list

    let empty = []
    let is_empty a = a = []
    let load_nodes n ts = n :: ts
    let get_nodes = List.hd

    let reroot_in_component tree = 
        let st = Status.create "Rerooting"  None 
        "Selecting the best alignment found per tree" in
        Status.report st;
        let res = 
            All_sets.IntegerMap.fold (fun handle root new_tree ->
            match root.Ptree.root_median with
            | Some ((`Edge (a, b)), _) ->
                    let new_tree, _ = 
                        TreeOps.reroot_fn false (Tree.Edge (a, b)) new_tree 
                    in
                    new_tree
            | _ -> new_tree)
            tree.Ptree.component_root tree
        in
        Status.finished st;
        res

    let transform func tq =
        let nodes = get_nodes tq in
        let nodes' =
            List.map func nodes in
        if nodes' <> nodes                  (* If no transformations were performed,
                                               nodes will not have changed *)
        then nodes' :: tq
        else tq

    let untransform = List.tl

    let ratchet data probability severity = 
        let severity = float_of_int severity in
        let d = Data.get_weights data in
        let counter = ref 0 in
        let perturbator (x, y) =
            incr counter;
            if Random.float 1.0 < probability then
                (x, y *. severity)
            else (x, y)
        in
        let d = List.map perturbator d in
        List.fold_left (fun a (x, w) -> 
            Data.transform_weight (`ReWeight ((`Some (false, [x])), w)) a) 
            data d, !counter

    let filter_characters tree codes = 
        let filter_codes node = Node.f_codes codes node in
        let new_node_data = 
            All_sets.IntegerMap.map filter_codes tree.Ptree.node_data 
        in
        let component_root = 
            All_sets.IntegerMap.map (fun x ->
                match x.Ptree.root_median with
                | None -> x
                | Some (x, y) -> 
                        let y = filter_codes y in
                        { 
                            Ptree.component_cost = Node.root_cost y;
                            Ptree.adjusted_component_cost = Node.root_cost y;
                            Ptree.root_median = Some (x, y) })
            tree.Ptree.component_root
        in
        { tree with
              Ptree.node_data = new_node_data;
              Ptree.component_root = component_root }

    let substitute_nodes nodes tree =
        let adder acc x = 
            All_sets.IntegerMap.add (Node.taxon_code x) x acc
        in
        let node_data = 
            List.fold_left adder All_sets.IntegerMap.empty nodes 
        in
        let tree =
            { tree with
                  Ptree.node_data = node_data } in

        let st = Status.create "Diagnosis"  None "Recalculating original tree" in
        Status.report st;
        let res = TreeOps.uppass (TreeOps.downpass tree) in
        Status.finished st;
        res


    let transform_nodes nodes tree =
        let node_data : Node.n All_sets.IntegerMap.t
                = tree.Ptree.node_data in
        let map = List.fold_right
            (fun n map ->
                 All_sets.IntegerMap.add (Node.taxon_code n) n map)
            nodes All_sets.IntegerMap.empty in
        let node_data = All_sets.IntegerMap.mapi
            (fun i n ->
                 try All_sets.IntegerMap.find i map
                 with Not_found -> n)
            node_data in
        let tree =
            { tree with
                  Ptree.node_data = node_data } in
        TreeOps.uppass (TreeOps.downpass tree)

    let resample_characters n data =
        let d = Data.get_weights data in
        let d = Array.of_list d in
        let max = Array.length d in
        let res = Array.map (fun (x, y) -> (x, 0.)) d in
        for i = 1 to n do
            let element = Random.int max in
            let (_, b) = d.(element) in
            let (x, y) = res.(element) in
            res.(element) <- (x, y +. b);
        done;
        Array.fold_left 
        (fun a (b, c) -> Data.transform_weight 
            (`ReWeight ((`Some (false, [b])), c)) a) data res

    let resample_characters_in_tree n data tree = 
        let new_data = resample_characters n data in
        let new_data, nodes = Node.load_data new_data in
        let new_tree = substitute_nodes nodes tree in
        new_data, new_tree

    let ratchet_tree data probability severity tree =
        let new_data, changed = ratchet data probability severity in
        let new_data, nodes = Node.load_data new_data in
        new_data, substitute_nodes nodes tree, changed

    let fix_implied_alignments remove_non_informative chars data tree = 
        let st = 
            Status.create "Fixing Implied Alignments" None 
            "Calculating Static Approximation" 
        in
        Status.report st;
        let new_data = 
            IA.to_static_homologies true filter_characters
            remove_non_informative chars data tree 
        in
        Status.full_report ~msg:"Regenerating the nodes" st;
        let new_data, nodes = Node.load_data new_data in
        Status.finished st;
        new_data, substitute_nodes nodes tree
        

        module TS = TreeSearch.Make (Node) (Edge) (TreeOps)

    let unratchet_tree data tree = 
        let new_data, nodes = Node.load_data data in
        new_data, TS.diagnose (substitute_nodes nodes tree)

    (** End user visible functionality *)
    let perturbation_features = function
        | `Ratchet (probability, severity) ->
                [("type", "ratchet"); ("probability", string_of_float probability); 
                ("severity", string_of_int severity)]
        | `Resample (`Characters n) ->
                [("type", "characters"); ("sample size", string_of_int n)]
        | `Resample (`Taxa n) ->
                [("type", "taxa"); ("sample size", string_of_int n)]
        | `UnRatchet ->
                [("type", "unratchet")]
        | `UnResample (`Characters _) ->
                [("type", "unresample characters")]
        | `UnResample (`Taxa _) ->
                [("type", "unresample taxa")]
        | `UnFixImpliedAlignments ->
                [("type", "unia")]
        | `FixImpliedAlignments _ ->
                [("type", "fix implied alignment")]

    let perturbate_in_tree (meth : Methods.perturb_method) data tree =
        match meth with
        | `Resample kind ->
                Sadman.start "sampling" (perturbation_features meth);
                let data, tree = 
                    match kind with
                    | `Characters n -> resample_characters_in_tree n data tree
                    | `Taxa n -> 
                            (* TODO *)
                            failwith "Not supported"
                in
                Sadman.finish [];
                data, TS.diagnose tree
        | `Ratchet (probability, severity) -> 
                Sadman.start "perturbation" (perturbation_features meth);
                let a, b, c = ratchet_tree data probability severity tree in
                Sadman.finish [("reweighted", string_of_int c)];
                a, TS.diagnose b
        | `UnRatchet 
        | `UnFixImpliedAlignments 
        | `UnResample (`Characters _)
        | `UnResample (`Taxa _) ->
                unratchet_tree data tree
        | `FixImpliedAlignments (chars, remove_non_informative) ->
                Sadman.start "perturbation" (perturbation_features meth);
                let data, res = 
                    fix_implied_alignments remove_non_informative chars data tree 
                in
                Sadman.finish [];
                data, TS.diagnose res


    let perturbe data trees meth = 
        Sexpr.map_status "Perburbing"
            (fun x -> let _, t = perturbate_in_tree meth
                 data x in t) trees

    let transform_tree f t =
        let leaves = Ptree.get_all_leaves t in
        let leaves = List.map f leaves in
        substitute_nodes leaves t

    let undo = function 
        | `Ratchet _ -> `UnRatchet
        | `FixImpliedAlignments _ -> `UnFixImpliedAlignments
        | `Resample ((`Characters _) as x) -> `UnResample x
        | _ -> failwith "Unsupported undo"

    let get_title = function
        | `Ratchet _ -> "Ratcheting"
        | `FixImpliedAlignments _ -> "Fixed Implied Alignment"
        | `Resample (`Characters _) -> "Resampling Characters"
        | `Resample (`Taxa _) -> "Resampling Characters"
        | `UnRatchet -> "UnRatcheting"
        | `UnFixImpliedAlignments -> "Rolling Implied Alignment back to DO"
        | `UnResample (`Characters _) -> 
                "Rolling Character Weights back to original"
        | `UnResample _ -> 
                "Rolling Taxon Weights back to original"

    let escape_local data queue trees = function
        | `PerturbateNSearch (_, pert_method, search_method, iterations) ->
                let st = 
                    Status.create "Perturb Iteration" (Some iterations) ""
                in
                let trees = ref trees in
                for i = 1 to iterations do
                    Status.full_report ~adv:i st;
                    let todo = Sexpr.length !trees in
                    let title = get_title pert_method in
                    let status = Status.create title (Some todo) "trees in \
                    the current optimum" in
                    Status.report status;
                    let mapper = fun tree ->
                        let _, x = perturbate_in_tree pert_method data tree in
                        let x = TS.diagnose x in
                        let did = Status.get_achieved status in
                        Status.full_report ~adv:(did + 1) status;
                        x
                    in
                    let prepared_trees = Sexpr.map mapper !trees in
                    let set = TreeSearch.sets search_method data prepared_trees in
                    let new_optimal = 
                        let f = TS.find_local_optimum in
                        f data queue prepared_trees set search_method
                    in
                    let new_trees = perturbe data new_optimal (undo pert_method) in
                    let merged = Sexpr.combine (!trees, new_trees) in
                    let choose_best (tree, others) =
                        match
                            Sexpr.fold_left (fun ((c, _) as acc) cur_tree ->
                                let nc = Ptree.get_cost `Adjusted cur_tree in
                                if nc < c then (nc, Some cur_tree)
                                else acc) (max_float, None) others
                        with
                        | _, Some x -> x
                        | _, None -> failwith "No trees?"
                    in
                    trees := Sexpr.map choose_best merged;
                    Status.finished status;
                done;
                Status.finished st;
                !trees

    let replace_nodes nodes tree =
        let filter node = 
            List.find (fun x -> Node.taxon_code x = Node.taxon_code node) nodes 
        in
        transform_tree filter tree

    let unsupported_character_messages = function
        | `Assign_Transformation_Cost_Matrix (_, _) ->
                Status.user_message Status.Error "Assigning a new transformation \
                cost matrix  is not supported \
                yet. I will not modify the characters requested with \
                this transformation.";
        | `Prioritize ->
                Status.user_message Status.Error "Prioritize  is not supported \
                yet. I will not modify the characters requested with \
                this transformation.";
        | `Automatic_Sequence_Partition _ ->
                Status.user_message Status.Error "Automatic Sequence Partition  \
                is not supported yet. I will not modify the characters requested with \
                this transformation.";
        | `Automatic_Static_Aprox ->
                Status.user_message Status.Error "Automatic  is not supported \
                yet. I will not modify the characters requested with \
                this transformation.";
        | `Exact _ ->
                Status.user_message Status.Error "Exact is not supported \
                yet. I will not modify the characters requested with \
                this transformation.";
        | `Approximate _ ->
                Status.user_message Status.Error "Approximate is not supported \
                yet. I will not modify the characters requested with \
                this transformation.";
        | `Search_Based _ -> 
                Status.user_message Status.Error "Search Based is not supported \
                yet. I will not modify the characters requested with \
                this transformation."


    let rec all_pairs f acc lst =
        match lst with
        | h :: t -> 
                let acc = List.fold_left (f h) acc t in
                all_pairs f acc t
        | [] -> acc

    let count_all_pairs (_, a, m) (ones, mores) (_, b, _) =
        let _ = Sequence.Align.align_2 a b m Matrix.default in
        let bt = Sequence.Align.make_backtrack a b Matrix.default in
        match Sequence.Align.count_paths bt with
        | 1 -> (ones + 1, mores)
        | _ -> (ones, mores + 1)

    let get_roots tree = 
        let get_handle ptree = All_sets.Integers.choose (Ptree.get_handles ptree) in
        let get_component_of_handle ptree = 
            let handle = get_handle ptree in
            let root = Ptree.get_component_root handle ptree in
            let leaves = Ptree.get_leaves handle ptree in
            match root.Ptree.root_median with
            | Some (_, node) -> node, leaves
            | None -> failwith "No roots in trees?"
        in
        get_component_of_handle tree

    let (-->) a b = b a 

    let post_order_in_every_handle f ptree acc = 
        let process_handle x acc =
            match (Ptree.get_component_root x ptree).Ptree.root_median with
            | Some ((`Edge (a, b)), _) ->
                    let edge = Tree.Edge (a, b) in
                    Tree.post_order_node_with_edge_visit_simple f edge
                    acc.Ptree.tree acc
            | _ -> acc
        in
        All_sets.Integers.fold process_handle ptree.Ptree.tree.Tree.handles acc

    (* A function that maps a tree that has only nodes to a tree that has tuples
    * in each node, being each tuple the node and the union of the nodes it
    * roots *)
    let to_tupled_tree ptree =
        let get_union code ptree =
            let _, u = Ptree.get_node_data code ptree in 
            u
        in
        let convert_vertex_post_order original_tree prev code final_tree = 
            match Ptree.get_node code ptree with
            | Tree.Single _ 
            | Tree.Leaf (_, _) -> 
                    Ptree.get_node_data code original_tree 
                    --> fun x -> (x, Node.Union.leaf None None x)
                    --> fun x -> Ptree.add_node_data code x final_tree
            | (Tree.Interior (_, par, a, b)) as node ->
                    assert (check_assertion_two_nbrs prev node "5");
                    let a, b = Tree.other_two_nbrs prev node in
                    Ptree.get_node_data code original_tree 
                    --> fun x -> 
                        (x, Node.Union.union (Some prev) x 
                        (get_union a final_tree) (get_union b final_tree))
                    --> fun x -> Ptree.add_node_data code x final_tree
        in
        let convert_roots component_root final_tree = 
            let convert_a_root root = 
                match root with
                | None -> None
                | Some ((`Edge (a, b)) as v, d) ->
                        let _, aunion = 
                            try Ptree.get_node_data a final_tree with
                            | e -> 
                                    Status.user_message Status.Error
                                    ("Failed with " ^ string_of_int a);
                                    raise e
                        and _, bunion = 
                            try Ptree.get_node_data b final_tree with
                            | e ->
                                    Status.user_message Status.Error
                                    ("Failed with " ^ string_of_int b);
                                    raise e
                        in
                        Some (v, (d, Node.Union.union None d aunion bunion))
                | Some ((`Single _) as v, d) ->
                        Some (v, (d, Node.Union.leaf None None d))
            in
            let convert_a_root tree prevhandle root roots =
                let true_root = Tree.handle_of prevhandle tree in
                let nmed = convert_a_root root.Ptree.root_median in
                let root_med = 
                    { Ptree.root_median = nmed; 
                    component_cost = root.Ptree.component_cost; 
                    adjusted_component_cost = root.Ptree.component_cost } 
                in
                All_sets.IntegerMap.add true_root root_med roots
            in
            let roots = 
                All_sets.IntegerMap.fold (convert_a_root final_tree.Ptree.tree) component_root 
                All_sets.IntegerMap.empty
            in
            { final_tree with Ptree.component_root = roots }
        in
        { Ptree.empty with 
        Ptree.edge_data = ptree.Ptree.edge_data; 
        Ptree.tree = ptree.Ptree.tree}
        --> 
            post_order_in_every_handle 
            (convert_vertex_post_order ptree) ptree
        --> convert_roots ptree.Ptree.component_root


    let insert_union parent union_node (a, b, c, d, e) =
        (a, b, Node.Union.get_sequence parent a union_node, c, d, e)

    let get_sequence parent code (normal_node, union_node) = 
        let tmp = Node.get_sequences parent normal_node in
        let tmp = List.find (fun (c, _, _, _, _) -> (code = c)) tmp in
        insert_union parent union_node tmp

    let ever_increasing lst =
        let res, _ = 
            List.fold_left (fun (acc, prev) (x, _) ->
                (acc && (prev <= x)), x) (true, -1) lst
        in
        res

    let partition_sequences mode sensible codes tree data =
        let (root, root_union), _ = get_roots tree in
        let produce_partitions data tree (positions, union, code) = 
            assert (ever_increasing positions);
            let rec traverse_tree parent1 parent2 ch1 ch2 positions1 positions2 acc =
                let node1, node1u = Ptree.get_node_data ch1 tree 
                and node2, node2u = Ptree.get_node_data ch2 tree in
                let node1, node2, ch1, ch2, parent1, parent2 =
                    if 
                        Node.min_child_code parent1 node1 < 
                        Node.min_child_code parent2 node2 
                    then
                        (node1, node1u), (node2, node2u), ch1, ch2, parent1,
                        parent2
                    else (node2, node2u), (node1, node1u), ch2, ch1, parent2,
                    parent1
                in
                let process_one parent node_data ch positions acc =
                    assert (ch <> parent);
                    if Tree.is_leaf ch tree.Ptree.tree then
                        let _, the_sequence, the_union, _, _, alph =
                            get_sequence None code node_data in
                        assert (ever_increasing positions);
                        if not (Sequence.is_empty the_sequence 16) then
                            (Sequence.split positions the_sequence alph, ch) :: acc
                        else acc
                    else
                        match Ptree.get_node ch tree with
                        | (Tree.Interior (a, b, c, d)) as n' ->
                                assert (check_assertion_two_nbrs parent n' "6");
                                let c, d = Tree.other_two_nbrs parent n' in
                                let (_, ts, the_union, _, _, _) = 
                                    get_sequence (Some parent) code node_data 
                                in
                                if Sequence.is_empty ts 16 then acc
                                else 
                                    let p1, p2 = 
                                        Sequence.Unions.get_positions the_union positions
                                    in
                                    if (ever_increasing p1) && (ever_increasing
                                    p2) then begin
                                        assert (a <> c); 
                                        assert (a <> d);
                                        traverse_tree (Some a) (Some a) c d p1 p2 acc
                                    end else begin
                                        let printer x = 
                                            List.iter (fun (x, _) -> print_int x;
                                            print_string " ") x;
                                            print_newline ()
                                        in
                                        printer p1;
                                        printer p2; 
                                        printer positions;
                                        let union_len = 
                                            Sequence.length
                                            the_union.Sequence.Unions.seq
                                        in
                                        let cap = 
                                            Bigarray.Array1.dim 
                                            the_union.Sequence.Unions.union_c1
                                        in
                                        print_newline ();
                                        for i = cap - union_len to cap - 1 do
                                            Printf.printf "%d, %d\n" 
                                            (Sequence.Unions.to_int the_union.Sequence.Unions.union_c1.{i})
                                            (Sequence.Unions.to_int the_union.Sequence.Unions.union_c2.{i});
                                        done;
                                        failwith "Assertion broken in 631";
                                    end
                        | _ -> failwith "Impossible"
                in
                let p1, p2 = 
                    match parent1, parent2 with
                    | Some x, Some y -> 
                            assert (x <> ch1);
                            assert (y <> ch2);
                            x, y
                    | _ -> assert false
                in
                let acc = process_one p1 node1 ch1 positions1 acc in
                process_one p2 node2 ch2 positions2 acc
            in
            let handle = All_sets.Integers.choose (Ptree.get_handles tree) in
            let root = Ptree.get_component_root handle tree in
            match root.Ptree.root_median with
            | Some ((`Edge (a, b)), _) ->
                    assert (a <> b);
                    let p1, p2 = Sequence.Unions.get_positions union positions in
                    traverse_tree (Some b) (Some a) a b p1 p2 [], code
            | _ -> failwith "Impossible"
        in
        let get_positions partition_mode (code, _, union, cm2, cm3, alph) =
            let equal_length_processor size (pos_list, cur_len, prev) pos base =
                if cur_len = size then (((pos, pos) :: pos_list), 0, pos)
                else (pos_list, cur_len + 1, prev)
            in
            let automatic_processor (positions_list, constant_length, ungapped) pos base =
                let count = Sequence.count_bits base in
                 if (1 = count) && (0 = base land 16) then
                     positions_list, constant_length + 1, ungapped
                 else if sensible && (count < 4) && (0 = base land 16) then
                     positions_list, constant_length, ungapped + 1
                else 
                    if constant_length > 10 || ungapped > 20 then
                        let pos = ((2 * pos) + constant_length + 1) / 2 in
                        ((pos, pos) :: positions_list), 0, 0
                    else (positions_list, 0, 0)
            in
            let positions, _, _ = 
                let processor = 
                    match partition_mode with
                    | None -> automatic_processor
                    | Some n ->
                            let len = Sequence.length union.Sequence.Unions.seq in
                            Printf.printf "The union length is %d\n%!" len;
                            let n = 
                                if n > len then n
                                else if n < 1 then 1
                                else n
                            in
                            equal_length_processor (len / n)
                in
                Sequence.fold_righti processor ([], 0, 0) 
                union.Sequence.Unions.seq 
            in
            assert (ever_increasing positions);
            positions, union, code
        in
        let process_partitions data (sequences, character) =
            let name = Data.code_character character data in
            let tcm = Data.get_sequence_tcm character data in
            let treed = Data.get_tcm3d data character 
            and tcmfile = Data.get_tcmfile data character in
            let alph = Data.get_alphabet data character in
            let data = Data.process_ignore_characters false data (`Some [character]) in
            let new_data = 
                List.map (fun (seqs, taxon) ->
                    ([[seqs]], Data.code_taxon taxon data)) sequences
            in
            Data.process_parsed_sequences tcmfile tcm treed 
            false alph name `Seq data new_data 
        in
        root 
        --> Node.get_sequences None
        --> List.filter (fun (c, _, _, _, _) -> All_sets.Integers.mem c codes)
        --> List.map (insert_union None root_union)
        --> List.map (get_positions mode)
        --> List.map (produce_partitions data tree)
        --> List.fold_left process_partitions data

    let analyze_sequences sensible acc ((node, node_union), leafs) =
        try 
            let leaf_sequences = 
                leafs
                --> List.map (fun (x, un) -> (Node.get_sequences None x), un) 
                --> List.map (fun (x, un) -> List.map (fun x -> (x, un)) x) 
                --> List.flatten
                --> List.map (fun (x, un) -> insert_union None un x) 
            and sequences = 
                node 
                --> Node.get_sequences None
                --> List.map (insert_union None node_union)
            in
            List.fold_left (fun acc (code, _, s, _, _, alph) ->
                if 0.85 < Sequence.poly_saturation s.Sequence.Unions.seq 1 
                    || 0.05 > Sequence.gap_saturation s.Sequence.Unions.seq alph then 
                    All_sets.Integers.add code acc
                else if not sensible then acc 
                else
                    (let sum, minlen, maxlen, cnt = 
                        List.fold_left 
                        (fun ((sum, minlen, maxlen, cnt) as acc) (c, s, _, _, _, _) ->
                            if c <> code then acc
                            else if Sequence.is_empty s 16 then acc
                            else 
                                let len = Sequence.length s in
                                (sum + len, min minlen len, max maxlen len, cnt + 1)) 
                        (0, max_int, min_int, 0) leaf_sequences 
                    in
                    let union_len = Sequence.length s.Sequence.Unions.seq in
                    if minlen > maxlen - (maxlen / 10) then
                        if maxlen > (union_len  - (union_len / 10)) then
                            All_sets.Integers.add code acc
                        else acc
                    else acc)) acc sequences
        with 
        | Failure "Node.get_sequence: could not find code" -> acc

    let do_analyze (c, (ones, mores)) =
        let ones = float_of_int ones 
        and mores = float_of_int mores in
        if (ones /.  (ones +. mores)) > 0.80 then c, true
        else c, false
        
    let extract_sequence_code (_, v) = v

    let analyze_sequences sensible data trees =
        if 0 <> Sexpr.length trees then
            trees 
            --> Sexpr.map to_tupled_tree
            --> Sexpr.map get_roots
            --> Sexpr.fold_left (analyze_sequences sensible) All_sets.Integers.empty
            --> All_sets.Integers.elements
        else []

    let prioritize tree =
        match All_sets.Integers.elements (Ptree.get_handles tree) with
        | h :: _ ->
                let root = Ptree.get_component_root h tree in
                begin match root.Ptree.root_median with
                | Some (_, root) ->
                        let prio = Node.prioritize root in
                        transform_tree (Node.reprioritize prio) tree
                | None -> tree
                end
        | [] -> tree

    let process_static_approx remove chars remove_non_informative data filter tree =
        tree
        --> 
            IA.to_static_homologies remove filter_characters remove_non_informative 
            chars data 
        --> Data.categorize 


    let get_char_codes (chars : Methods.characters)  data =
        let codes = 
            let codes = 
                match chars with
                | `Some (dont_complement, codes) ->
                        let codes = Data.get_chars_codes data (`Some codes) in
                        if dont_complement then `Some codes
                        else Data.complement_characters data (`Some codes)
                | `Names (dont_complement, names) ->
                        let codes = Data.get_chars_codes data (`Names names) in
                        if dont_complement then `Some codes
                        else Data.complement_characters data (`Some codes)
                | `Missing _ | `All | `AllStatic | `AllDynamic as x ->
                        `Some (Data.get_chars_codes data x)
                | `Random fraction ->
                        let lst = Data.get_chars_codes data `All in
                        let arr = Array.of_list lst in
                        Array_ops.randomize arr;
                        let n =
                            let len = float_of_int (Array.length arr) in
                            truncate ((fraction *. len) /. 100.)
                        in
                        let rec collect n acc =
                            if n = 0 then `Some acc
                            else collect (n - 1) (arr.(n) :: acc)
                        in
                        collect n []
            in
            let codes = 
                match codes with
                | `Some x -> x
                | _ -> failwith "Impossible?"
            in
            (* Ensure we are not removing anything that is not affected by this
            * transformation *)
            let dyn = 
                (Data.get_code_from_characters_restricted `Dynamic data 
                (`Some codes))
            in
            List.filter (fun x -> List.exists (fun y -> y = x) dyn) codes 
        in
        codes

    let rec transform_node_characters trees (data, nodes) (meth : Methods.char_transform)  =
        let nc = List.map Node.taxon_code nodes in
        let load_transformed_data new_data = 
            let data, nodes =
                new_data 
                --> Data.categorize
                --> Node.load_data ~taxa:nc 
            in
           data, (List.rev nodes)
        in 
        let cleanup_extra_dynamics chars = 
            List.filter (fun x -> 
                let a = Data.get_alphabet data x in
                let a = Alphabet.to_sequential a in
                Alphabet.distinct_size a < 6 ) chars
        in
        match meth with
        | `Automatic_Sequence_Partition (chars, sensible, mode) ->
                (match 
                    cleanup_extra_dynamics 
                    (Data.get_code_from_characters_restricted_comp 
                    `Dynamic data chars)
                with
                | [] -> data, nodes
                | chars ->
                        (* One problem we have is that auto sequence partition
                        * can only work with nucleotide sequences, so we have to
                        * filter those out. Actually, we will make it work with
                        * those *)
                        let chars = 
                            List.fold_left 
                            (fun acc x -> 
                                Printf.printf "Adding %d\n%!" x;
                                All_sets.Integers.add x acc)
                            All_sets.Integers.empty
                            chars
                        in
                        try
                            trees
                            --> select_shortest
                            --> to_tupled_tree
                            --> fun x -> partition_sequences mode sensible chars x data
                            --> Data.categorize 
                            --> Node.load_data ~taxa:nc
                        with
                        | No_trees ->
                            Status.user_message Status.Error
                            ("An@ error@ has@ occured@ while@ attempting@ to@ "
                            ^ "do@ an@ automatic@ sequence@ partition.@ You@ "
                            ^ "have@ no@ trees@ in@ memory!@ In@ order@ to@ " ^
                            "produce@ an@ implied@ alignment@ POY@ needs@ a@ " ^
                            "loaded@ tree,@ you@ could@ simply@ build@ one@ " ^
                            "with@ build (1)");
                            failwith "Illegal transform command")
        | `Automatic_Static_Aprox sensible ->
                (match analyze_sequences sensible data trees with
                | [] -> data, nodes
                | chars ->
                      Status.user_message (Status.Output (None, false, []))
                    "@.@[I@ will@ make@ static@ the@ characters@[<v 2>@,";
                    List.iter (fun x -> 
                        let name = Data.code_character x data in
                        let name = StatusCommon.escape name in
                        Status.user_message Status.Information ("@[" ^ name ^ "@]@,")) chars;
                    Status.user_message (Status.Output (None, false, [])) "@]@]@.";
                    transform_node_characters trees (data, nodes) (`Static_Aprox (`Some
                    (true, chars), true)))
        | `Prealigned_Transform chars ->
                Node.load_data ~taxa:nc (Data.prealigned_characters
                ImpliedAlignment.analyze_tcm data chars)
        | `MultiStatic_Aprox (chars, remove_non_informative) ->
                (try
                    let len = Sexpr.length trees in
                    let _, data = 
                        Sexpr.fold_left (fun (cnt, acc) x ->
                            cnt + 1,
                            process_static_approx (cnt = len) chars remove_non_informative 
                            acc filter_characters x) (1, data) trees
                    in
                    Node.load_data ~taxa:nc data
                with
                | No_trees ->
                        Status.user_message Status.Error
                        ("An@ error@ has@ occured@ while@ attempting@ to@ fix@ "
                        ^ "an@ static@ approximation.@ You@ have@ no@ trees@ " ^
                        "in@ memory!@ In@ order@ to@ " ^
                        "produce@ an@ implied@ alignment@ POY@ needs@ a@ " ^
                        "loaded@ tree,@ you@ could@ simply@ build@ one@ " ^
                        "with@ build (1)");
                        failwith "Illegal transform command")
        | `Static_Aprox (chars, remove_non_informative) ->
                if 1 < Sexpr.length trees then
                    Status.user_message Status.Information
                    ("I@ will@ use@ the@ shortest@ tree@ to@ fix@ the@ sequence@ "
                    ^ "alignments.")
                else ();
                (try
                    (select_shortest trees)
                    --> process_static_approx true chars remove_non_informative data
                    filter_characters 
                    --> Node.load_data ~taxa:nc
                with
                | No_trees ->
                        Status.user_message Status.Error
                        ("An@ error@ has@ occured@ while@ attempting@ to@ fix@ "
                        ^ "an@ static@ approximation.@ You@ have@ no@ trees@ " ^
                        "in@ memory!@ In@ order@ to@ " ^
                        "produce@ an@ implied@ alignment@ POY@ needs@ a@ " ^
                        "loaded@ tree,@ you@ could@ simply@ build@ one@ " ^
                        "with@ build (1)");
                        failwith "Illegal transform command")
        | (`ReWeight _)
        | (`WeightFactor _) as m ->
                data 
                --> Data.transform_weight m 
                --> Data.categorize 
                --> Node.load_data ~taxa:nc
        | `Assign_Prep_Cost (filit, chars) ->
                filit
                --> Data.assign_prepend data chars
                --> Data.categorize
                --> Node.load_data ~taxa:nc
        | `Assign_Tail_Cost (filit, chars) ->
                filit
                --> Data.assign_tail data chars
                --> Data.categorize
                --> Node.load_data ~taxa:nc
        | `Assign_Transformation_Cost_Matrix (file, chars) ->
                file 
                --> Data.assign_tcm_to_characters_from_file data chars 
                --> Data.categorize 
                --> Node.load_data ~taxa:nc 
        | `Assign_Affine_Gap_Cost (cost, chars) ->
                (if cost = 0 then (Cost_matrix.Linnear) 
                else (Cost_matrix.Affine cost))
                --> Data.assign_affine_gap_cost data chars
                --> Data.categorize
                --> Node.load_data ~taxa:nc
        | `Create_Transformation_Cost_Matrix (trans, gaps, chars) ->
                gaps 
                --> Data.assign_transformation_gaps data chars trans 
                --> Data.categorize 
                --> Node.load_data ~taxa:nc
        | `Prioritize ->
                let new_nodes = 
                    trees 
                    --> Sexpr.first 
                    --> prioritize
                in
                data,
                List.map 
                (fun x -> Ptree.get_node_data (Node.taxon_code x) new_nodes) 
                nodes
        | `Search_Based _ as meth ->
                unsupported_character_messages meth;
                data, nodes
        | `Fixed_States chars ->
                data 
                --> Data.make_fixed_states chars
                --> Data.categorize
                --> Node.load_data ~taxa:nc
             
        | #Methods.dynamic_char_transform as meth -> begin
              let status = 
                  Status.create "Tranform" None 
                  "Transforming chromosome to rearranged sequences." 
              in
              let new_data = 
                  match meth with  
                  | `Chrom_to_Seq (chars, _ ) ->
                        let tree = select_shortest trees in               
                        let tree = reroot_in_component tree in
                        let char_codes = get_char_codes chars data in 
                        let ia_ls = IA.create filter_characters char_codes data tree in
                        let new_data = 
                            Data.transform_chrom_to_rearranged_seq data meth
                                char_codes ia_ls 
                        in                  
                        new_data                           
                  | _ -> 
                        Data.transform_dynamic meth data 
              in  
              Status.finished status;              
              load_transformed_data new_data 
          end 


    let transform_nodes trees data nodes (trans : Methods.char_transform list) = 
        let len = List.length trans in 
        let st = 
            Status.create "Transforming" (Some len) " of transformations applied"
        in
        let apply_transformation trees acc res =

            let res = transform_node_characters trees acc res in
            let _ =
                let ach = Status.get_achieved st in
                Status.full_report ~adv:(ach +1) st
            in
            res
        in
        let res = List.fold_left (apply_transformation trees) (data, nodes) trans in
        Status.finished st;
        res
end 
