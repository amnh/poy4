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

let () = SadmanOutput.register "Mst" "$Revision: 1673 $"

type mst = int list All_sets.IntegerMap.t

let empty = All_sets.IntegerMap.empty

let connect_one_dir a b mst = 
    if All_sets.IntegerMap.mem a mst then
        let cr = All_sets.IntegerMap.find a mst in
        All_sets.IntegerMap.add a (b :: cr) mst
    else All_sets.IntegerMap.add a [b] mst

let connect a b mst = 
    connect_one_dir b a (connect_one_dir a b mst)

let kruskal distance_matrix list =
    let find_best did todo =
        List.fold_left (fun tmpacc in_tree ->
            List.fold_left (fun acc not_in_tree ->
                match acc with
                | None -> 
                        Some (distance_matrix in_tree not_in_tree, in_tree,
                        not_in_tree)
                | Some (cd, _, _) ->
                        let nd = distance_matrix in_tree not_in_tree in
                        if nd >= cd then acc
                        else Some (nd, in_tree, not_in_tree))
            tmpacc todo) None did
    in
    let rec aux_kruskal total mst did todo =
        match find_best did todo with
        | None -> mst
        | Some (tadd, a, b) -> 
                let mst = connect a b mst
                and did = b :: did 
                and todo = List.filter (fun x -> x <> b) todo in
                aux_kruskal (tadd +. total) mst did todo
    in
    match list with
    | h :: t ->
            let tree = All_sets.IntegerMap.add h [] empty in
            aux_kruskal 0. tree [h] t
    | [] -> failwith "An empty tree?2"

let rec collapser tree =
    match tree with
    | Parser.Tree.Leaf _ -> tree
    | Parser.Tree.Node (chld, cont) ->
            match chld with
            | [chld] -> 
                    collapser chld
            | [] -> failwith "empty?"
            | children ->
                    Parser.Tree.Node ((List.map collapser chld), cont)

let get_neighbors_not_visited visited vertex mst =
    let neighs = All_sets.IntegerMap.find vertex mst in
    List.filter (fun x -> not (All_sets.Integers.mem x visited)) neighs

let vertices_from_mst mst = 
    All_sets.IntegerMap.fold (fun a _ acc -> a :: acc) mst [] 

let get_random_vertex mst = 
    match vertices_from_mst mst with
    | [] -> failwith "An empty tree?"
    | lst ->
            let len = List.length lst in
            List.nth lst (Random.int len) 

let random_list lst = 
    let arr = Array.of_list lst in
    Array_ops.randomize arr;
    Array.to_list arr

let randomized_post_order to_name mst =
    let rec aux_dfs visited vertex =
        let leaf = 
            let name = to_name vertex in
            Parser.Tree.Leaf name
        and visited = All_sets.Integers.add vertex visited in
        let neighbors = 
            let tmp = get_neighbors_not_visited visited vertex mst in
            random_list tmp
        in
        let visited, trees = 
            List.fold_left (fun (visited, trees) vertex ->
                let visited, nt = aux_dfs visited vertex in
                (visited, nt :: trees)) (visited, []) neighbors 
        in
        let tree = Parser.Tree.Node (leaf :: trees, "") in
        visited, tree
    in
    let item = get_random_vertex mst in
    let _, tree = aux_dfs All_sets.Integers.empty item in 
    collapser tree

let simplified_order to_name mst = 
    let rec aux_dfs visited vertex = 
        let name = to_name vertex 
        and visited = All_sets.Integers.add vertex visited in
        match get_neighbors_not_visited visited vertex mst with
        | [] -> visited, (Parser.Tree.Leaf name)
        | neighbors ->
                let visited, trees =
                    List.fold_left (fun (visited, trees) vertex ->
                        let visited, nt = aux_dfs visited vertex in
                        (visited, nt :: trees)) (visited, []) neighbors
                in
                visited, Parser.Tree.Node (trees, name)
    in
    let start =  (* If we have choosen a leaf, we take it's neighbor *)
        let item = get_random_vertex mst in
        match get_neighbors_not_visited All_sets.Integers.empty item mst with
        | [one] -> one
        | _ -> item
    in
    let _, tree = aux_dfs All_sets.Integers.empty start in
    tree

let randomized_bfs_traversal mst =
    let queue = Queue.create () in
    let rec aux_bfs visited acc =
        if not (Queue.is_empty queue) then
            let item = Queue.pop queue in
            let acc = item :: acc
            and visited = All_sets.Integers.add item visited in
            let ngbs = 
                let t = get_neighbors_not_visited visited item mst in
                random_list t 
            in
            let _ = List.iter (fun x -> Queue.push x queue) ngbs in
            aux_bfs visited acc
        else List.rev acc
    in
    let item = get_random_vertex mst in
    Queue.add item queue;
    aux_bfs All_sets.Integers.empty []

let randomized_dfs_traversal mst =
    let rec aux_dfs (visited, acc) item = 
        let acc = item :: acc
        and visited = All_sets.Integers.add item visited in
        let ngbs =
            let t = get_neighbors_not_visited visited item mst in
            random_list t
        in
        List.fold_left aux_dfs (visited, acc) ngbs 
    in
    let item = get_random_vertex mst in
    let _, res = aux_dfs (All_sets.Integers.empty, []) item in
    List.rev res

let print_mst_tree tost mst filename =
    let t = simplified_order tost mst 
    and sep = 4
    and bd = 16 in
    Status.user_message 
    (Status.Output (filename, false, [])) 
    (AsciiTree.to_string ~sep ~bd true t);
    Status.user_message
    (Status.Output (filename, false, []))
    "%!"
