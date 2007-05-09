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

let () = SadmanOutput.register "AllDirChar" "$Revision: 1616 $"

module IntSet = All_sets.Integers

module F (Exact : Ptree.Exact) : Ptree.Tree_Operations with 
type a = AllDirNode.AllDirF.n
with type b = AllDirNode.OneDirF.n = struct

    type a = AllDirNode.AllDirF.n
    type b = AllDirNode.OneDirF.n
    type phylogeny = (a, b) Ptree.p_tree

    let (-->) a b = b a

    let force_node x = Lazy.force_val x.AllDirNode.lazy_node

    let get_one ptree code x =
        ptree 
        --> Ptree.get_node_data x 
        --> fun x -> x.AllDirNode.unadjusted
        --> AllDirNode.not_with code
        --> force_node

    (* Creates a lazy node with code [code] which is the median between the 
    * vertices with codes [a] and [b]. *)
    let create_lazy_node ptree code a b =
        let dataa = get_one ptree code a 
        and datab = get_one ptree code b 
        and ncode = Some code in
        let node = 
            Lazy.lazy_from_fun 
            (fun () -> Node.Standard.median ncode ncode None dataa datab)
        in
        { AllDirNode.lazy_node = node; dir = Some (a, b); code = code }

    (* Creates a lazy edge which is the median between the data of the vertices
    * with codes [a] and [b] in the tree [ptree]. *)
    let create_lazy_edge ptree a b = 
        let get_one a b =
            ptree --> Ptree.get_node_data a 
            --> fun x -> x.AllDirNode.unadjusted 
            --> AllDirNode.not_with b --> force_node
        in
        let dataa = get_one a b 
        and datab = get_one b a in
        Lazy.lazy_from_fun 
        (fun () -> Node.Standard.median None None None dataa datab)

    (* Creates a valid vertex that only has the downpass information *)
    let create_lazy_interior_down ptree code a b =
        let res = [create_lazy_node ptree code a b] in
        { AllDirNode.unadjusted = res; adjusted = res }

    (* Creates a valid vertex that has the downpass and uppass information.
    * [a] and [b] have to be the component of the currently calculated subtree
    * that is still valid. *)
    let create_lazy_interior_up ptree code a b c =
        let cur_data = 
            try Ptree.get_node_data code ptree with
            | Not_found -> create_lazy_interior_down ptree code a b
        in
        let still_good = AllDirNode.not_with c cur_data.AllDirNode.unadjusted in
        let ac = create_lazy_node ptree code a c 
        and bc = create_lazy_node ptree code b c in
        let cnt = [still_good; ac; bc] in
        { AllDirNode.unadjusted = cnt; adjusted = cnt }

    let create_root a b (ptree : phylogeny) =
        let make_internal () = 
            let norm = 
                Tree.normalize_edge (Tree.Edge (a, b)) ptree.Ptree.tree 
            in
            let node = Ptree.get_edge_data norm ptree in
            let nnode = Lazy.force_val node in
            let node = {AllDirNode.lazy_node = node; code = 0; dir = None} in
            let node = { AllDirNode.unadjusted = [node]; adjusted = [node] } in
            { 
                Ptree.root_median = Some ((`Edge (a, b)), node);
                component_cost = Node.Standard.root_cost nnode;
                adjusted_component_cost = Node.Standard.root_cost nnode;
            }
        in
        match Ptree.get_node a ptree with
        | Tree.Leaf (_, x) ->
                assert (x = b);
                make_internal ();
        | Tree.Interior (_, x, y, z) ->
                assert ((x = b) || (y = b) || (z = b));
                make_internal ();
        | Tree.Single _ ->
                let root = Ptree.get_node_data a ptree in
                { 
                    Ptree.root_median = Some ((`Single a), root);
                    component_cost = AllDirNode.AllDirF.root_cost root;
                    adjusted_component_cost = AllDirNode.AllDirF.root_cost root;
                }

    (* We first define a function to check the total cost of a tree for
    * sequences only! *)
    let check_cost new_tree handle =
        let edge_visitor (Tree.Edge (a, b)) acc =
            let nda = 
                (List.hd ((Ptree.get_node_data a
                new_tree).AllDirNode.adjusted)).AllDirNode.lazy_node
            and ndb = (List.hd ((Ptree.get_node_data b
                new_tree).AllDirNode.adjusted)).AllDirNode.lazy_node
            in
            let dist = AllDirNode.OneDirF.distance nda ndb in
            Tree.Continue, dist +. acc
        in
        let real_cost = 
            Tree.pre_order_edge_visit edge_visitor handle new_tree.Ptree.tree 
            0.0
        in
        let root_minus = 
            Ptree.get_component_root handle new_tree 
            --> (fun x -> 
                match x.Ptree.root_median with
                | Some (_, root) -> root, AllDirNode.AllDirF.root_cost root
                | None -> failwith "No root?")
            --> (fun (x, cost) -> 
                match x.AllDirNode.unadjusted with
                | [x] -> 
                        cost -.
                        (Node.total_cost_of_type `Seq
                        (Lazy.force_val x.AllDirNode.lazy_node))
                | _ -> failwith "Wha?")
        in
        real_cost +. root_minus

    (* A function to assign a unique sequence on each vertex of the ptree in the
    * [AllDirNode.adjusted] field of the node. *)
    let assign_single ptree = 
        (* We first define a function that can traverse the tree and assign
        * a single sequence to each vertex on it. *)
        let rec assign_single_subtree parentd parent current ptree = 
            let current_d, initial_d =
                let tmp = Ptree.get_node_data current ptree in
                AllDirNode.not_with parent tmp.AllDirNode.unadjusted, tmp
            in
            let nd, original = 
                current_d.AllDirNode.lazy_node
                --> Lazy.force_val 
                --> fun x -> Node.to_single parentd x, x
            in
            let nnd = 
                { current_d with AllDirNode.lazy_node = Lazy.lazy_from_val nd }
            in
            let final_d = { initial_d with AllDirNode.adjusted = [nnd] } in
            let ptree = Ptree.add_node_data current final_d ptree in
            try 
                let a, b = 
                    let currentn = Ptree.get_node current ptree in 
                    Tree.other_two_nbrs parent currentn
                in
                ptree
                --> assign_single_subtree nd current a
                --> assign_single_subtree nd current b 
            with
            | Invalid_argument _ -> ptree
        in
        (* Now we define a function that can assign single sequences to the
        * connected component of a handle *)
        let assign_single_handle handle ptree =
            let get_root_direction root = 
                match root.AllDirNode.unadjusted with
                | [x] -> Lazy.force_val (x.AllDirNode.lazy_node), x
                | _ -> 
                        failwith "more than one root? \
                        AllDirChar.assign_single_handle 2"
            in
            let generate_root_and_assign_it rootg edge ptree =
                let root, rooth = get_root_direction rootg in
                let root = Node.to_single root root in
                let rooti = [{ rooth with AllDirNode.lazy_node =
                    lazy (root) }]
                in
                let readjusted = { rootg with AllDirNode.adjusted = rooti} in
                Ptree.assign_root_to_connected_component 
                handle
                (Some (edge, readjusted))
                (Node.Standard.root_cost root)
                None
                ptree, root, readjusted
            in
            let comp = Ptree.get_component_root handle ptree in
            match comp.Ptree.root_median with
            | Some ((`Edge (a, b)) as edge, rootg) ->
                    let ptree, root, readjusted = 
                        generate_root_and_assign_it rootg edge ptree 
                    in
                    ptree
                    --> assign_single_subtree root b a 
                    --> assign_single_subtree root a b 
                    --> (fun ptree ->
                        Ptree.assign_root_to_connected_component 
                        handle 
                        (Some ((`Edge (a, b)), readjusted))
                        comp.Ptree.component_cost
                        (Some (check_cost ptree handle))
                        ptree)
            | Some ((`Single a) as edge, rootg) ->
                    let ptree, root, _ = 
                        generate_root_and_assign_it rootg edge ptree 
                    in
                    assign_single_subtree root (-1) a ptree
            | None -> failwith "no root? AllDirChar.assign_single_handle"
        in
        (* Finally, we are ready to proceed on all the handles available *)
        let res =
            All_sets.Integers.fold assign_single_handle
            ptree.Ptree.tree.Tree.handles ptree 
        in
        (*
        assert ((Ptree.get_cost `Adjusted res) = check_cost res handle);
        *)
        res

    let unadjust ptree = ptree
    (*
        (* We first define a function that takes a handle and assigns it the
        * unadjusted total cost *)
        let unadjust_handle handle ptree =
            let comp = Ptree.get_component_root handle ptree in
            match comp.Ptree.root_median with
            | Some ((`Edge (_, _)), root) -> 
                    Ptree.assign_root_to_connected_component
                    handle
                    comp.Ptree.root_median
                    (AllDirNode.AllDirF.total_cost None root)
                    ptree
            | _ -> ptree
        in
        All_sets.Integers.fold unadjust_handle ptree.Ptree.tree.Tree.handles
        ptree
    *)

    (* Now we define a function that can adjust all the vertices in the tree
    * to improve the overall cost of the tree, using only the
    * [AllDirNode.adjusted] field of each. *)
    let adjust_tree ptree =
        (* We start by defining a function to adjust one node *)
        let adjust_node ch1 ch2 parent minec mine ptree =
            match ch1.AllDirNode.adjusted, ch2.AllDirNode.adjusted, 
            parent.AllDirNode.adjusted, mine.AllDirNode.adjusted with
            | [ch1], [ch2], [parent], [mine'] ->
                    let mine = 
                        mine'
                        --> force_node 
                        --> (Node.readjust (force_node ch1) (force_node ch2) 
                            (force_node parent))
                        --> Lazy.lazy_from_val 
                        --> (fun x -> { mine' with AllDirNode.lazy_node = x })
                        --> (fun x -> { mine with AllDirNode.adjusted = [x] })
                    in
                    Ptree.add_node_data minec mine ptree
            | _ -> failwith "Huh? AllDirChar.aux_adjust_single"
        in
        (* Now we should be able to adjust all the vertices in the tree
        * recursively, in a post order traversal, on a given subtree. *)
        let rec adjust_subtree parent vertex ptree =
            match Ptree.get_node vertex ptree with
            | Tree.Leaf _ 
            | Tree.Single _ -> ptree
            | node ->
                    let a, b = Tree.other_two_nbrs parent node in
                    let ptree = 
                        ptree 
                        --> (adjust_subtree vertex a)
                        --> (adjust_subtree vertex b)
                    in
                    let gnd x = Ptree.get_node_data x ptree in
                    let ptree =
                        adjust_node (gnd a) (gnd b) (gnd parent) vertex (gnd vertex)
                        ptree
                    in
                    let newv = Ptree.get_node_data vertex ptree in
                    Printf.printf "The vertex %d has cost %f\n%!" vertex 
                    (AllDirNode.OneDirF.total_cost (Some parent) (List.hd
                    (newv.AllDirNode.adjusted)).AllDirNode.lazy_node);
                    ptree
        in
        (* Now we need to be able to adjust the root of the tree, and it's
        * cost, once we have finished adjusting every vertex in the tree *)
        let adjust_root_n_cost handle root a b ptree =
            let ad = Ptree.get_node_data a ptree
            and bd = Ptree.get_node_data b ptree in
            match ad.AllDirNode.adjusted, bd.AllDirNode.adjusted with
            | [ad], [bd] ->
                    let new_root = 
                        let ad = force_node ad
                        and bd = force_node bd in
                        Printf.printf "The distance here is %f\n%!"
                        (Node.Standard.distance ad bd);
                        let node = Node.Standard.median None None None ad bd in
                        Printf.printf "The median cost is %f with parents of
                        cost %f and %f\n%!"
                        (Node.Standard.node_cost None node)
                        (Node.Standard.total_cost None ad)
                        (Node.Standard.total_cost None bd);
                        node
                    in
                    Printf.printf "The total cost of this will be %f\n%!"
                    (Node.Standard.total_cost None new_root);
                    let new_root' = 
                        { new_root with 
                         Node.characters = (Node.to_single new_root
                         new_root).Node.characters }
                        --> fun x -> [{ 
                            AllDirNode.lazy_node = lazy x;
                            dir = Some (a, b);
                            code = (-1);}]
                    in
                    Ptree.assign_root_to_connected_component handle 
                    (Some ((`Edge (a, b)), {root with 
                    AllDirNode.adjusted = new_root'}))
                    (Node.Standard.root_cost new_root)
                    None
                    ptree
            | _ -> failwith "AllDirChar.adjust_root_n_cost"
        in
        (* Now we are ready for a function to adjust a handle in the tree *)
        let adjust_handle handle ptree =
            match (Ptree.get_component_root handle ptree).Ptree.root_median with
            | Some ((`Edge (a, b)), rootg) ->
                    let ptree = 
                        ptree 
                        --> adjust_subtree a b 
                        --> adjust_subtree b a
                    in
                    adjust_root_n_cost handle rootg a b ptree
            | Some _ -> ptree
            | None -> failwith "Huh? AllDirChar.adjust_handle"
        in
        (* Finally, we are ready to assign to all the handles in the tree
        * and adjusted cost *)
        All_sets.Integers.fold adjust_handle ptree.Ptree.tree.Tree.handles
        ptree

    let assign_single_and_readjust ptree = 
        adjust_tree (assign_single ptree)

    let refresh_all_edges ptree =
        let new_edges = 
            Tree.EdgeSet.fold (fun ((Tree.Edge (a, b)) as e) acc ->
                let data = 
                    try create_lazy_edge ptree a b with
                    | err -> 
                            let print_node = function
                                | Tree.Interior (u, v, w, x) ->
                                        Status.user_message Status.Error 
                                        ("with neighbors " ^ string_of_int v ^
                                        ", " ^ string_of_int w ^ ", " ^
                                        string_of_int x);
                                | Tree.Leaf (_, x) ->
                                        Status.user_message Status.Error
                                        ("with neighbor " ^ string_of_int x);
                                | Tree.Single _ -> ()
                            in
                            Status.user_message Status.Error
                            ("Failure while attempting lazy_edge between " ^
                            string_of_int a ^ " and " ^ string_of_int b);
                            print_node (Ptree.get_node a ptree);
                            print_node (Ptree.get_node b ptree);
                            raise err
                in
                Tree.EdgeMap.add e data acc)
            ptree.Ptree.tree.Tree.d_edges Tree.EdgeMap.empty
        in
        { ptree with Ptree.edge_data = new_edges }

    let refresh_roots ptree =
        let new_roots =
            All_sets.Integers.fold (fun x acc ->
                let root = 
                    match Ptree.get_node x ptree with
                    | Tree.Leaf (a, b)
                    | Tree.Interior (a, b, _, _) ->
                            create_root a b ptree
                    | Tree.Single _ ->
                            create_root x x ptree
                in
                All_sets.IntegerMap.add x root acc
            ) ptree.Ptree.tree.Tree.handles All_sets.IntegerMap.empty
        in
        {
            ptree with 
            Ptree.component_root = new_roots;
        }

    let internal_downpass do_roots (ptree : phylogeny) : phylogeny =
        (* Traverse every vertex in the tree and assign the downpass and uppass
        * information using the lazy all direction nodes *)
        let ptree = 
            (* A function to add  the vertices using a post order traversal 
            * from the Ptree library. *)
            let add_vertex_post_order prev code ptree =
                match Ptree.get_node code ptree with
                | Tree.Single _
                | Tree.Leaf (_, _) -> Tree.Continue, ptree
                | (Tree.Interior (_, par, a, b)) as v ->
                        let a, b = 
                            match prev with
                            | Some prev -> Tree.other_two_nbrs prev v
                            | None -> a, b
                        in
                        let interior = create_lazy_interior_down ptree code a b
                        in
                        Tree.Continue, Ptree.add_node_data code interior ptree
            in
            (* A function to add the vertices using a pre order traversal from
            * the Ptree library *)
            let add_vertex_pre_order prev code ptree =
                match Ptree.get_node code ptree with
                | Tree.Single _ 
                | Tree.Leaf (_, _) -> Tree.Continue, ptree
                | (Tree.Interior (_, par, a, b)) as v ->
                        let a, b, prev =
                            match prev with
                            | Some prev ->
                                    let a, b = Tree.other_two_nbrs prev v in
                                    a, b, prev
                            | None -> 
                                    a, b, par
                        in
                        let interior = 
                            create_lazy_interior_up ptree code a b prev 
                        in
                        Tree.Continue, 
                        Ptree.add_node_data code interior ptree
            in
            All_sets.Integers.fold 
            (fun x ptree -> 
                let ptree = 
                    Ptree.post_order_node_visit add_vertex_post_order x ptree 
                    ptree
                in
                Ptree.pre_order_node_visit add_vertex_pre_order x ptree ptree)
            ptree.Ptree.tree.Tree.handles
            ptree
        in
        if do_roots then
            ptree --> refresh_all_edges --> refresh_roots
        else 
            ptree --> refresh_all_edges

    let downpass = internal_downpass true

    let clear_internals = 
        internal_downpass false

    let uppass ptree = 
        let edgesnhandles = 
            All_sets.Integers.fold 
            (fun handle acc ->
                ((Ptree.get_pre_order_edges handle ptree), handle) :: acc)
            ptree.Ptree.tree.Tree.handles 
            []
        in
        let process ptree (edges, handle) =
            match
                List.fold_left (fun acc ((Tree.Edge (a, b)) as e) ->
                    let data = Ptree.get_edge_data e ptree in
                    let c = AllDirNode.OneDirF.root_cost data in
                    match acc with 
                    | Some (edge, cost, v) ->
                            if cost > c then Some ((a, b), c, data)
                            else acc
                    | None -> Some ((a, b), c, data)) None edges
            with
            | Some (a, b, c) -> 
                    let c = 
                        [{ AllDirNode.lazy_node = c; dir = None; code = -1 }] 
                    in
                    let c = { AllDirNode.unadjusted = c; adjusted = c } in
                    let comp = Some ((`Edge a), c) in
                    Ptree.set_component_cost b None comp handle ptree
            | None -> 
                    assert (Tree.is_single handle ptree.Ptree.tree);
                    let data = Ptree.get_node_data handle ptree in
                    let c = AllDirNode.AllDirF.root_cost data in
                    let comp = Some ((`Single handle), data) in
                    Ptree.set_component_cost c None comp handle ptree
        in 
        assign_single (List.fold_left process ptree edgesnhandles)

    let create_edge ptree a b =
        let edge1 = (Tree.Edge (a, b)) in
        let edge2 = (Tree.Edge (b, a))
        and edge = Tree.normalize_edge edge1 ptree.Ptree.tree in
        ptree --> Ptree.remove_edge_data edge1 
            --> Ptree.remove_edge_data edge2 
            --> Ptree.add_edge_data edge (create_lazy_edge ptree a b) 

    (* Remove the "uppass" data from the vertex v, where the parent is p in the
    * tree ptree *)
    let clear_vertex v p a b ptree = 
        let new_data = create_lazy_interior_up ptree v a b p in 
        Ptree.add_node_data v new_data ptree

    let debug_clear_subtree = false 

    let rec clear_subtree v p ptree = 
        if debug_clear_subtree then
            Status.user_message Status.Information 
            ("Clearing vertex " ^ string_of_int v ^ " with parent " ^
            string_of_int p)
        else ();
        match Ptree.get_node v ptree with
        | Tree.Leaf _ | Tree.Single _ -> ptree
        | Tree.Interior (_, a, b, c) ->
                let uno, dos = 
                    if a = p then b, c
                    else if b = p then a, c
                    else if c = p then a, b 
                    else failwith "No parent"
                in
                ptree --> clear_vertex v p uno dos 
                    --> clear_subtree uno v 
                    --> clear_subtree dos v 

    (* Remove all the "uppass" data from the tree, starting in the edge (a, b),
    * moving up *)
    let clear_up_over_edge (a, b) ptree =
        let fora, forb = 
            (* A function that will recreate a fresh vertex in a given tree
            * depending on it's role on it *)
            let create_interior_down node other =
                match Ptree.get_node node ptree with
                | Tree.Interior (a, u, v, w) ->
                        let unoa, dosa = 
                            if other = u then v, w
                            else if other = v then u, w
                            else if other = w then u, v
                            else failwith "AllDirChar.clear_up_over_edge"
                        in
                        create_lazy_interior_down ptree a unoa dosa
                | _ ->
                    Ptree.get_node_data node ptree
            in
            create_interior_down a b, create_interior_down b a
        in
        ptree --> Ptree.add_node_data a fora
            --> Ptree.add_node_data b forb 
            --> clear_subtree b a 
            --> clear_subtree a b

    let set_clade_root (ptree : phylogeny) ex_sister root handle = 
        match Ptree.get_node handle ptree with
        | Tree.Interior (handle, sister, _, _)
        | Tree.Leaf (handle, sister) ->
                let node, snode = 
                    let a = AllDirNode.not_with ex_sister root.AllDirNode.unadjusted in
                    { AllDirNode.unadjusted = [a]; adjusted = [a]}, a
                in
                let ptree = 
                    Ptree.add_edge_data (Tree.Edge (handle, sister))
                    snode.AllDirNode.lazy_node ptree
                in
                let snode = force_node snode in
                let cost = Node.Standard.root_cost snode in
                Ptree.set_component_cost cost None
                (Some (`Edge (handle, sister), node)) handle ptree
        | Tree.Single _ ->
                let cost = AllDirNode.AllDirF.root_cost root in
                Ptree.set_component_cost cost None (Some (`Single handle, root)) 
                handle ptree


    let clean_ex_neighbor a b ptree = 
        let data = Ptree.get_node_data a ptree in
        let notwith_un = AllDirNode.not_with b data.AllDirNode.unadjusted in
        let node = { AllDirNode.unadjusted = [notwith_un]; adjusted = [notwith_un] } 
        in
        Ptree.add_node_data a node ptree

    let get_edge_n_force a b ptree =
        let data = Ptree.get_edge_data (Tree.Edge (a, b)) ptree in
        Lazy.force_val data

    let replace_topology tree ptree = { ptree with Ptree.tree = tree } 

    let add_component_root ptree handle root = 
        { ptree with 
        Ptree.component_root = All_sets.IntegerMap.add handle root
        ptree.Ptree.component_root }

    (* break_fn must have type handle * node -> tree -> tree * delta * aux_data
    * *)
    let break_fn (tree_node, clade_node_id) (ptree : phylogeny) =
        let (Tree.Edge (tree_node, clade_node_id)) as edge = 
            Tree.normalize_edge (Tree.Edge (tree_node, clade_node_id)) 
            ptree.Ptree.tree 
        in
        (* We want to know the cost of the tree, so we force the calculation of
        * the downpass all the way down to the place of the breakage *)
        let prev_cost = 
            let edge_res = get_edge_n_force tree_node clade_node_id ptree in
            Node.Standard.root_cost edge_res
        in
        (* Figure out the cost of the broken tree *)
        let new_cost, clade_node = 
            let clade_node = Ptree.get_node_data clade_node_id ptree
            and tree_node_dir = 
                ptree --> Ptree.get_node_data tree_node 
                --> fun x -> x.AllDirNode.unadjusted
                    --> AllDirNode.not_with clade_node_id 
                    --> force_node
            in
            let clade_node_dir = 
                clade_node --> fun x -> x.AllDirNode.unadjusted
                    --> AllDirNode.not_with tree_node 
                    --> force_node
            in
            clade_node_dir.Node.total_cost +. tree_node_dir.Node.total_cost,
            let res = 
                [AllDirNode.not_with tree_node clade_node.AllDirNode.unadjusted]
            in
            { AllDirNode.unadjusted = res; adjusted = res }
        in
        (* Break the topology and update the data *)
        let ptree, tree_delta, clade_handle =
            (* A function that takes one side of a tree delta and updates the
            * tree's data using that information *)
            let update_break_delta delta ptree = 
                match delta with
                | `Edge (rem, l1, l2, _) ->
                        assert ((tree_node = rem) || (clade_node_id = rem));
                        ptree --> clean_ex_neighbor l1 rem
                            --> clean_ex_neighbor l2 rem
                            --> Ptree.remove_node_data rem
                            --> clear_up_over_edge (l1, l2) 
                | _ -> 
                        ptree
            in
            (* Perform the topology break *)
            let nbt, ((left_delta, right_delta) as tree_delta) = 
                Tree.break (tree_node, clade_node_id) ptree.Ptree.tree 
            in
            let tree_handle, clade_handle = 
                Tree.get_break_handles tree_delta nbt 
            in
            (* Update the actual contents of the tree *)
            let ptree = 
                ptree --> Ptree.remove_root_of_component tree_handle
                    --> Ptree.remove_root_of_component clade_handle
                    --> Ptree.remove_edge_data edge
                    --> replace_topology nbt
                    --> update_break_delta left_delta
                    --> update_break_delta right_delta
                    --> refresh_all_edges
                    --> refresh_roots
            in
            ptree, tree_delta, clade_handle
        in
        (* Compare costs, and calculate the break delta *)
        let b_delta =
            if prev_cost = infinity && new_cost = infinity then 0.
            else 
                let rc, tc = 
                    let clade_root = 
                        let c = Ptree.get_component_root clade_handle ptree in
                        match c.Ptree.root_median with
                        | Some (_, b) -> b
                        | None -> failwith "AllDirChar.break_fn Huh?"
                    in
                    AllDirNode.AllDirF.root_cost clade_root, 
                    AllDirNode.AllDirF.total_cost None clade_root
                in
                (prev_cost -. (new_cost -. (rc +. ptree.Ptree.origin_cost))) -. tc
        in
        ptree, tree_delta, b_delta, clade_node_id, clade_node, []

    let break_fn a b =
        if not Exact.exact then break_fn a b
        else 
            let c, d, e, f, g, _ = break_fn a b in
            let nt = uppass c in
            nt, d, 
            (Ptree.get_cost `Adjusted b) -. (Ptree.get_cost `Adjusted nt),
            f,
            g,
            []

    let debug_join_fn = false

    (* join_fn must have type join_1_jxn -> join_2_jxn -> delta -> tree -> tree
    * *)
    let join_fn _ jxn1 jxn2 ptree =
        if debug_join_fn then
            Status.user_message Status.Information "Time to join!"
        else ();
        let ret, tree_delta = Tree.join jxn1 jxn2 ptree.Ptree.tree in
        let v, h, ptree =
            match tree_delta with
            | (`Edge (v, a, b, _)), (`Single (h, true)), _ ->
                    let ptree = 
                        ptree --> Ptree.remove_node_data v 
                            --> clean_ex_neighbor a b
                            --> clean_ex_neighbor b a
                            --> Ptree.remove_root_of_component h
                    in
                    v, h, ptree
            | (`Single (v, _)), (`Single (h, true)), _ ->
                    v, h, Ptree.remove_root_of_component h ptree
            | (`Edge (v, c, d, _)), (`Edge (r, a, b, Some h)), _ ->
                    let ptree = 
                        ptree --> Ptree.remove_root_of_component h 
                            --> Ptree.remove_node_data r 
                            --> Ptree.remove_node_data v
                            --> clean_ex_neighbor c d
                            --> clean_ex_neighbor d c
                            --> clean_ex_neighbor a b
                            --> clean_ex_neighbor b a
                    in
                    r, v, ptree
            | (`Single (v, _)), (`Edge (r, a, b, Some h)), _ ->
                    let ptree = 
                        ptree --> Ptree.remove_root_of_component h 
                            --> Ptree.remove_node_data r 
                            --> clean_ex_neighbor a b
                            --> clean_ex_neighbor b a
                    in
                    r, h, ptree
            | _ -> failwith "Unexpected Chartree.join_fn"
        in
        let ptree = { ptree with Ptree.tree = ret } in
        let handle, parent = 
            let handle = Ptree.handle_of v ptree in 
            let parent = Ptree.get_parent handle ptree in
            handle, parent
        in
        let ptree = 
            ptree --> Ptree.remove_root_of_component handle 
                --> clear_up_over_edge (v, h) 
                --> refresh_all_edges 
        in
        let ptree = 
            add_component_root ptree handle (create_root handle parent ptree)
        in
        ptree, tree_delta

    let join_fn a b c d =
        if not Exact.exact then join_fn a b c d 
        else 
            let tree, delta = join_fn a b c d in
            uppass tree, delta

    type tmp = Edge of (int * int) | Clade of a 

    let cost_fn jxn1 jxn2 delta clade_data (tree : phylogeny) =
        let rec forcer edge =
            match edge with
            | Edge (a, b) ->
                    Lazy.force_val (Ptree.get_edge_data (Tree.Edge (a, b)) tree)
            | Clade x -> 
                    (match x.AllDirNode.unadjusted with
                    | [x] -> force_node x
                    | _ -> failwith "AllDirChar.cost_fn")
        in
        let clade_data = forcer (Clade clade_data) in
        match jxn1 with
        | Tree.Single_Jxn h ->
                let d = 
                    Node.Standard.distance 
                    (forcer (Clade (Ptree.get_node_data (Tree.int_of_id h)
                    tree)))
                    clade_data
                in
                Ptree.Cost d
        | Tree.Edge_Jxn (h, n) ->
              let (Tree.Edge (h, n)) = 
                  Tree.normalize_edge (Tree.Edge (h, n)) tree.Ptree.tree
              in
              let ndata = forcer (Edge (h, n)) in
              let d = Node.Standard.distance clade_data ndata in
              Ptree.Cost d

    let cost_fn a b c d e =
        if not Exact.exact then cost_fn a b c d e 
        else
            let pc = Ptree.get_cost `Adjusted e in
            let nt, _ = join_fn [] a b e in
            Ptree.Cost (pc -. (Ptree.get_cost `Adjusted nt))

    let reroot_fn edge ptree =
        let Tree.Edge (h, n) = edge in
        let my_handle = Ptree.handle_of h ptree in
        let root = Ptree.get_component_root my_handle ptree in
        let ptree, _ = 
            ptree 
            --> Ptree.remove_root_of_component my_handle 
            --> Ptree.move_handle h 
        in
        let ptree = Ptree.fix_handle_neighbor h n ptree in
        let root = 
            let new_roots = create_root h n ptree in
            if new_roots.Ptree.component_cost < 
                root.Ptree.component_cost then
                new_roots
            else root
        in
        add_component_root ptree h root, []

    let root_costs tree = 
        let collect_edge_data edge node acc =
            let cost = AllDirNode.OneDirF.root_cost node in
            (edge, cost) :: acc
        in
        Tree.EdgeMap.fold collect_edge_data tree.Ptree.edge_data []

    let string_of_node _ = ""

    let features meth lst = 
        Chartree.features meth (("all directions", "true") :: lst)

    let incremental_uppass tree _ = tree

    let convert_three_to_one_dir tree = 
        (* We will convert the tree in a one direction tree, then we will use
        * the standard Chartree.to_formatter function. *)
        let tree = 
            (* We convert the tree to a one direction one *)
            let simple_conversion prev curr acc = 
                let node =
                    let data = Ptree.get_node_data curr tree in
                    AllDirNode.not_with prev data.AllDirNode.unadjusted
                in
                Ptree.add_node_data curr (Lazy.force_val
                node.AllDirNode.lazy_node) acc
            in
            All_sets.IntegerMap.fold (fun _ root acc ->
                match root.Ptree.root_median with
                | Some ((`Edge (a, b)), r) ->
                        let r = 
                            match r.AllDirNode.unadjusted with
                            | [r] -> r
                            | _ -> failwith "No root?1"
                        in
                        let acc = 
                            let t = Tree.reroot (a, b) acc.Ptree.tree in
                            { acc with Ptree.tree = t } 
                        in
                        let acc = 
                            Tree.post_order_node_with_edge_visit_simple
                            simple_conversion (Tree.Edge (a, b))
                            acc.Ptree.tree acc
                        in
                        Ptree.assign_root_to_connected_component
                        a (Some ((`Edge (a, b)), 
                        Lazy.force_val r.AllDirNode.lazy_node))
                        root.Ptree.component_cost 
                        (Some root.Ptree.adjusted_component_cost)
                        acc
                | _ -> failwith "No root?")
            tree.Ptree.component_root 
            { Ptree.empty with Ptree.tree = tree.Ptree.tree }
        in 
        (*
        let tree = tree --> Chartree.downpass --> Chartree.uppass in
        *)
        tree


    let to_formatter ?(pre_ref_codes=IntSet.empty) ?(fi_ref_codes=IntSet.empty) 
    atr data tree = 
        let tree = convert_three_to_one_dir tree in
        let pre_ref_codes, fi_ref_codes = Chartree.get_active_ref_code tree in  
        Chartree.to_formatter ~pre_ref_codes:pre_ref_codes
          ~fi_ref_codes:fi_ref_codes atr data tree 

end

module CharScripting : CharacterScripting.S with type cs =
    CharacterScripting.Standard.cs with type n = AllDirNode.AllDirF.n = struct
        type cs = CharacterScripting.Standard.cs
        type n = AllDirNode.AllDirF.n

        type character_input_output = [ 
            | `Characters of cs Sexpr.t
            | `Floats of float Sexpr.t
        ]

        let distance = CharacterScripting.Standard.distance
        let median = CharacterScripting.Standard.median
        let convert =
            List.map 
            (fun x -> Lazy.force_val (List.hd x.AllDirNode.unadjusted).AllDirNode.lazy_node)

        let scriptchar_operations nodes = 
            CharacterScripting.Standard.scriptchar_operations (convert nodes)

        let filter_char_operations nodes =
            CharacterScripting.Standard.filter_char_operations (convert nodes)

        let extract_character x y =
            CharacterScripting.Standard.extract_character x 
            (List.hd (convert [y]))

        let character_operations = 
            CharacterScripting.Standard.character_operations
    end
