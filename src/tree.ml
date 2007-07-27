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

let () = SadmanOutput.register "Tree" "$Revision: 2013 $"

exception Invalid_Node_Id of int
exception Invalid_Handle_Id
exception Invalid_Edge
exception Invalid_End_Nodes
exception Node_Is_Handle

let debug_print_edge_removal = false
let debug_print_edge_addition = false
let debug_tests = false
let debug_fusing = false
let debug_handle_of = false
let odebug = Status.user_message Status.Information

type id = int

type 'p id_t = id

type node =
    | Single of id
    | Leaf of id * id
    | Interior of id * id * id * id


type edge =
    | Edge of (id * id)

type t_status = Continue | Skip | Break

module EdgeComparator = struct
    type t = edge
    let compare x y =
        match x, y with
        | Edge(e11, e12), Edge(e21, e22) ->
            match (e11 - e21) with
            | 0 -> (e12 - e22)
            | x -> x
end


module EdgeSet = Set.Make(EdgeComparator)

module EdgeMap =  Map.Make(EdgeComparator)

type edge_delta = {
    added : edge list;
    removed : edge list;
}

let empty_ed = { added = []; removed = [] }

let merge_edged a b =
    { added = b.added @ a.added; removed = b.removed @ a.removed }

type side_delta =
        [ `Single of int * bool
        | `Edge of int * int * int * int option
              (** l, l1, l2, handle added/removed? *)
        ]

let get_side_anchor = function
    | `Single (i, _) -> i
    | `Edge (i, _, _, _) -> i

type join_jxn =
    | Single_Jxn of id
    | Edge_Jxn of id * id

let string_of_jxn = function
    | Single_Jxn id -> string_of_int id
    | Edge_Jxn (a, b) -> string_of_int a ^ " -> " ^ string_of_int b

let side_to_jxn = function
    | `Single (i, _) -> Single_Jxn i
    | `Edge (_, l1, l2, _) -> Edge_Jxn (l1, l2)

let jxn_choose_node = function
    | Single_Jxn id -> id
    | Edge_Jxn (a, b) -> a

type reroot_delta = id list
type break_delta = side_delta * side_delta
type join_delta = side_delta * side_delta * reroot_delta

let join_to_edge_delta (sd1, sd2, rrd) =
    let a, r, n1 = match sd1 with
    | `Single (id, _) -> [], [], id
    | `Edge (l, l1, l2, _) ->
          [Edge(l, l1);
           Edge(l, l2);],
          [Edge(l1, l2);],
          l in
    let a, r, n2 = match sd2 with
    | `Single (id, _) -> a, r, id
    | `Edge (l, l1, l2, _) ->
          Edge(l, l1) :: Edge(l, l2) :: a,
          Edge(l1, l2) :: r,
          l in
    let a = Edge(n1, n2) :: a in
    { added = a; removed = r; }

let break_to_edge_delta (sd1, sd2) =
    let { added = a; removed = r; } = join_to_edge_delta (sd1, sd2, []) in
    { added = r; removed = a; }

(** [int_of_id id]
    @return the corresponding int of the id. *)
let int_of_id id =
    id

(** [print_break_jxn jxn]
    @return prints the break jxn. *)
let print_break_jxn (h1, h2) =
    let h1 = (int_of_id h1) and
        h2 = (int_of_id h2) in
    let h1_s = (string_of_int h1) and
        h2_s = (string_of_int h2) in
    print_string ("(" ^ h1_s ^ "," ^ h2_s ^ ")")

(** [print_join_1_jxn jxn]
    @return prints join_1_jxn. *)
let print_join_1_jxn jxn =
    match jxn with
    | Single_Jxn(s) ->
          print_string ("Single_1_Jxn " ^ (string_of_int s))
    | Edge_Jxn(e1, e2) ->
          let e1 = (int_of_id e1) and
                  e2 = (int_of_id e2) in
          let e1_s = (string_of_int e1) and
                  e2_s = (string_of_int e2) in
          print_string ("Edge_1_Jxn (" ^ e1_s ^ "," ^ e2_s ^ ")")

(** [print_join_2_jxn jxn]
    @return prints join_2_jxn. *)
let print_join_2_jxn jxn =
    match jxn with
    | Single_Jxn(s) ->
          print_string ("Single_2_Jxn " ^ (string_of_int s))
    | Edge_Jxn(e1, e2) ->
          let e1 = (int_of_id e1) and
                  e2 = (int_of_id e2) in
          let e1_s = (string_of_int e1) and
                  e2_s = (string_of_int e2) in
          print_string ("Edge_2_Jxn (" ^ e1_s ^ "," ^ e2_s ^ ")")


type u_tree = {
    u_topo  : node All_sets.IntegerMap.t;
    d_edges : EdgeSet.t;
    handles : All_sets.Integers.t;
    avail_ids : id list;
    new_ids : id;
}

let replace_codes f tree =
    let topo = 
        let fix_node = function
            | Single x -> Single (f x)
            | Leaf (x, y) -> Leaf ((f x), (f y))
            | Interior (a, b, c, d) -> Interior ((f a), (f b), (f c), (f d))
        in
        All_sets.IntegerMap.fold 
        (fun code node acc -> 
            All_sets.IntegerMap.add (f code) (fix_node node) acc)
        tree.u_topo
        All_sets.IntegerMap.empty
    and edges =
        let fix_edge (Edge (a, b)) = Edge ((f a), (f b)) in
        EdgeSet.fold 
        (fun edge acc -> EdgeSet.add (fix_edge edge) acc)
        tree.d_edges
        EdgeSet.empty
    and handles = 
        All_sets.Integers.fold
        (fun code acc -> All_sets.Integers.add (f code) acc) 
        tree.handles
        All_sets.Integers.empty
    and avail_ids = List.map f tree.avail_ids in
    { tree with u_topo = topo; d_edges = edges; handles = handles; avail_ids =
        avail_ids }

let set_avail_start tree id =
    { tree with new_ids = id }

let get_available tree =
    match tree.avail_ids with
    | id :: ids ->
          assert (false = All_sets.IntegerMap.mem id tree.u_topo);
          assert (false = List.mem id ids);
          id, { tree with avail_ids = ids }
    | [] ->
          assert (false = All_sets.IntegerMap.mem tree.new_ids tree.u_topo);
          tree.new_ids, { tree with new_ids = succ tree.new_ids }

type break_jxn = id * id

(*****************************************************************************)

(** [get_id node]
    @param node the node whose id is desired
    @return id of the node. *)
let get_id node =
    match node with
    | Single(id) 
    | Leaf(id, _)
    | Interior(id, _, _, _) -> id


(*****************************************************************************)

(** The empty unrooted tree. *)
let empty = {
    u_topo = All_sets.IntegerMap.empty;
    d_edges = EdgeSet.empty;
    handles = All_sets.Integers.empty;
    avail_ids = [];
    new_ids = 12345;
}

(** [is_handle id tree]
    @param id node id.
    @param tree tree in which the node exists.
    @return true, when the node corresponding to id is a handle.
            false, otherwise. *)
let is_handle id tree =
    (All_sets.Integers.mem id tree.handles)

(** [is_edge e tree]
    @param e=Edge (x, y) tuple indicating end vertices x, y.
    @param tree tree in which Edge(x,y) exists or not.
    @return true if Edge(x, y) exists, else false. *)
let is_edge e tree =
    (EdgeSet.mem e tree.d_edges)

(**************** GET FUNCTIONS **********************************)

(** [get_node_ids tree]
    @return all the node ids of the tree in a list. *)
let get_node_ids tree =
    let first x y z = x::z in
        (All_sets.IntegerMap.fold first tree.u_topo [])

(** [get_nodes tree]
    @return all the nodes of the tree in a list. *)
let get_nodes tree =
    let second x y z = y::z in
        (All_sets.IntegerMap.fold second tree.u_topo [])

(** [get_handles tree]
    @return the handles of the tree. *)
let get_handles tree =
    tree.handles

(** [handle_list tree] returns a list of the handles *)
let handle_list tree =
    All_sets.Integers.elements tree.handles

(** [get_node_id id btree]
    @param id the id being verified
    @param tree the tree in which id is a valid node.
    @return returns the id with a [`Node] tag
    after verifying it.
    @raise Invalid_Node_Id when the id does not have a node associated
    with it. *)
let get_node_id id tree =
    match (All_sets.IntegerMap.mem id tree.u_topo) with
    | true -> id
    | false -> raise (Invalid_Node_Id id)

(** [get_handle_id id btree]
    @param id the id being verified
    @param tree the tree in which id is a valid handle.
    @return returns the id with a [`Handle] tag
    after verifying it.
    @raise Invalid_Handle_Id when the id does not have a node associated
    with it. *)
let get_handle_id id tree =
    match (All_sets.Integers.mem id tree.handles) with
    | true -> id
    | false -> raise Invalid_Handle_Id

(** [get_node id btree]
    @param id id of the node desired.
    @param tree tree to which the node belongs
    @return returns the node associated with the id.
    @raise Invalid_Node_Id, if the id is not a valid id. *)
let get_node id tree =
    try
        (All_sets.IntegerMap.find id tree.u_topo)
    with
        | Not_found -> 
                raise (Invalid_Node_Id id)

(** [get_leaves ?acc tree handle] returns a list of the leaves in [tree]
    below node [handle] *)
let rec get_leaves ?(acc=[]) tree handle =
    if is_handle handle tree
    then begin match get_node handle tree with
    | Leaf (id, par) -> get_leaves ~acc:(id :: acc) tree par
    | Single id -> id :: acc
    | Interior (id, p, a, b) ->
          let acc = get_leaves ~acc tree p in
          let acc = get_leaves ~acc tree a in
          let acc = get_leaves ~acc tree b in
          acc
    end else begin match get_node handle tree with
    | Leaf (id, _) -> id :: acc
    | Single id -> failwith "Tree.get_leaves"
    | Interior (id, _, a, b) ->
          let acc = get_leaves ~acc tree a in
          let acc = get_leaves ~acc tree b in
          acc
    end

(** [get_all_leaves tree handle] returns a list of all the leaves in the tree *)
let get_all_leaves tree handle =
    let fn handle acc = get_leaves ~acc tree handle in
    All_sets.Integers.fold fn tree.handles []

(** [is_leaf id tree]
    @param id the id of the node.
    @param tree the tree in which the id corresponds to  the leaf or not.
    @return whether the node corresponding to the id is a leaf or not. *)
let is_leaf id tree =
    match get_node id tree with
    | Leaf(_, _) -> true
    | _ -> false

let is_single id tree =
    match get_node id tree with
    | Single _ -> true
    | _ -> false

(** [get_edge edge btree]
    @param edge the edge as a tuple (x,y).
    @param btree the tree.
    @return the edge Edge(x,y) after verifying the tuple.
    @raise Invalid edge when the edge does not belong to the tree. *)
let get_edge (x, y) btree =
    let edge = Edge(x, y) in
        match (EdgeSet.mem edge btree.d_edges) with
        | true -> edge
        | false -> raise Invalid_Edge

(** [normalize_edge edge tree] returns either [edge] or the reverse-direction
    [edge'] if one of those is in the tree.  Otherwise, it fails with exception
    [Invalid_Edge]. *)
let normalize_edge e tree =
    let Edge (x,y) = e in
    if EdgeSet.mem e tree.d_edges
    then e
    else
        let e' = Edge (y,x) in
        if EdgeSet.mem e' tree.d_edges
        then e'
        else raise Invalid_Edge

(** [nbr_of node1 node2]
    @param node1 a node
    @param node2 another node
    @return true if node2 is a neighbor of node1. *)
let nbr_of node1 node2 =
    match node1, node2 with
    | Leaf(id1, nbr1), Leaf(id2, nbr2) ->
            if (id1 = nbr2) && (id2 = nbr1) then
                true
            else begin
                Printf.printf "The neighbors of %d and %d are %d and %d"
                id1 id2 nbr1 nbr2;
                false
            end;
    | Leaf(id1, nbr1), Interior(id2, nbr21, nbr22, nbr23)
    |  Interior(id2, nbr21, nbr22, nbr23), Leaf(id1, nbr1) ->
            if (id2 = nbr1) && (List.mem id1 [nbr21; nbr22; nbr23]) then
                true
            else begin
                Printf.printf "The neighbors of %d and %d are %d %d %d and \
                %d\n%!" id2 id1 nbr21 nbr22 nbr23 nbr1;
                false
            end
    | Interior(id1, nbr11, nbr12, nbr13),
      Interior(id2, nbr21, nbr22, nbr23) ->
          if ((List.mem id1 [nbr21; nbr22; nbr23]) &&
          (List.mem id2 [nbr11; nbr12; nbr13])) then
              true
          else begin
              Printf.printf "The neighbors of %d and %d are \
              %d %d %d and %d %d %d\n%!" id1 id2 nbr11 nbr12
              nbr13 nbr21 nbr22 nbr23;
              false
          end
    | _ -> raise Invalid_End_Nodes

(** [verify_edge edge btree]
    @param edge the edge which is being verified i.e. whether it
                it indeed exists in the tree or not.
    @param btree the tree in which the edge exists or not.
    @return true if the edge exists in the tree, false otherwise. *)
let verify_edge edge btree =
    let Edge(e1, e2) = edge in
    let node1 = (get_node e1 btree) in
    let node2 = (get_node e2 btree) in
    (nbr_of node1 node2)

(**************** BEGIN - UNSAFE OPS ***************************************)

(** The following functions are unsafe i.e. they allow us to modify the
structure of the tree in a manner that does not maintain the invariants
of the tree data-structure. These functions should never be exposed to the
outside user. *)

(************************ ADD FUNCTIONS *************************)

(** [add_node node btree]
    @param node the node that needs to be added to the tree.
    @param tree the tree to which the node is being added.
    @return the tree with the node added.
    This function does not check whether the node is already
    present in the tree or not. If the node is already present then
    the tree is returned unmodified. *)
let add_node node btree =
    let id = (get_id node) in
    assert (if All_sets.IntegerMap.mem id btree.u_topo then begin print_int id;
    print_newline (); false end else true);
    let avail_ids = List.filter (fun i -> i <> id) btree.avail_ids in
    let new_topo = (All_sets.IntegerMap.add id node btree.u_topo) in
    { btree with u_topo = new_topo; avail_ids = avail_ids; }

(** [add_nodes nodes btree]
    @param nodes list of nodes that need to be added to the tree.
    @param btree tree to which the nodes are being added.
    @return tree with the list of nodes added. Older nodes with the
    same ids will be overwritten. *)
let add_nodes nodes btree =
    let f btree node = (add_node node btree) in
        (List.fold_left f btree nodes)

(** [add_edge edge btree]
    @param edge the edge that needs to be added to the tree.
    @param btree the tree to which the edge is being added.
    @return tree with the given edge added. If the edge already
    exists, tree is returned as is. *)
let add_edge edge btree =
    if debug_print_edge_addition then begin
        let Edge (e1, e2) = edge in
        Printf.printf "Adding edge %d %d.\n%!" e1 e2;
    end;
    let new_edges = (EdgeSet.add edge btree.d_edges) in
        { btree with d_edges = new_edges }

(** [add_edges edges btree]
    @param edges the edges that need to be added to the tree.
    @param btree the tree to which edges need to be added.
    @return a new tree with the edges added. *)
let add_edges edges btree =
    let f btree edge = (add_edge edge btree) in
    List.fold_left f btree edges

(** [add_handle id btree]
    @param id node id to be added as a handle.
    @param btree tree to which the node is being added as a handle.
    @return tree with the node added as a handle. Any previous node
    or handle with the id of node is overwritten. *)
let add_handle id btree =
    let new_handles = (All_sets.Integers.add id btree.handles) in
    { btree with handles = new_handles }

(** [add_handles nodes btree]
    @param nodes node ids to be added as handles.
    @param btree tree to which the nodes are added as handles.
    @return btree with nodes added as handles. *)
let add_handles handles btree =
    let f btree handle = (add_handle handle btree) in
    List.fold_left f btree handles

(************************ REMOVE FUNCTIONS ****************************)

(** [get_node id btree]
    @param id the id of the node that belongs to the tree.
    @param tree tree from whch the node with the given id is
                removed.
    @return tree with the node removed.
    This operation only removes nodes that are not handles.
    To remove handles, use the remove_handles function.
    This operation does not check whether the node is actually
    present in the tree or not. *)
let remove_node id btree =
    assert (All_sets.IntegerMap.mem id btree.u_topo);
    let new_topo = (All_sets.IntegerMap.remove id btree.u_topo) in
    assert (not (All_sets.IntegerMap.mem id new_topo));
    { btree with u_topo = new_topo; avail_ids = id :: btree.avail_ids }

(* [remove_reserved_node id btree] 
 * Removes a node from a tree, without making it an available node for other
 * tree operations in [avail_ids].
 * @param id is the node to be removed but reserved.
 * @param btree is the tree that holds the node
 * @return the tree with the node removed but the available ids unmodified.
 * *)
let remove_reserved_node id btree =
    let new_topo = All_sets.IntegerMap.remove id btree.u_topo in
    { btree with u_topo = new_topo }

(** [remove_nodes ids btree]
    @param ids the list of ids of nodes that belong to the tree.
    @param btree tree from which nodes with ids given the ids list
    will be removed from.
    @return tree with nodes with id in ids list removed. *)
let remove_nodes ids btree =
    let f btree id =  (remove_node id btree) in
        (List.fold_left f btree ids)

(** [remove_handle id btree]
    @param id id of the handle.
    @param btree tree in which the node corresponding to id is a handle.
    @return tree with handle removed. *)
let remove_handle id btree =
    let new_handles = (All_sets.Integers.remove id btree.handles) in
    { btree with handles = new_handles }

(** [remove_handles ids btree]
    @param ids of the handles to be removed.
    @param btree the tree whose handles are to be removed.
    @return tree with the handles removed. *)
let remove_handles ids btree =
    let f btree handle = (remove_handle handle btree) in
    List.fold_left f btree ids

(** [remove_edge edge btree]
    @param edge edge to be removed
    @param btree tree from which the edge is being removed.
    @return tree with the edge removed. *)
let remove_edge edge btree =
    if debug_print_edge_removal then begin
        let Edge (e1, e2) = edge in
        Printf.printf "Removing edge %d %d.\n%!" e1 e2;
    end;
    let new_d_edges = (EdgeSet.remove edge btree.d_edges) in
        { btree with d_edges = new_d_edges }

(** [remove_edges edges btree]
    @param edges the edges to be removed.
    @param btree tree from which the edges are being removed.
    @return tree with the edges removed. *)
let remove_edges edges btree =
    let f btree edge = (remove_edge edge btree) in
        (List.fold_left f btree edges)

(** [remove_edge_either edge tree] removes either the edge given or its reverse,
    depending on which exists in the tree.
    @raise Invalid_edge if neither edge exists *)
let remove_edge_either e t = remove_edge (normalize_edge e t) t

(** [remove_incident_edges id tree] returns a tree with all the edges coming
    into or out of node [id] removed. *)
let remove_incident_edges id tree =
    let rem a b t =
        if verify_edge (Edge (a, b)) t
        then remove_edge_either (Edge (a, b)) t
        else t in
    match get_node id tree with
    | Leaf (id, nbr) -> rem id nbr tree
    | Interior (id, n1, n2, n3) ->
          rem id n1
              (rem id n2
                   (rem id n3 tree))
    | Single _ -> tree

(********************** OTHER FNS *************************************)

(** [replace_nbr nbr new_nbr node]
    @return replace the nbr with new_nbr and returns the new node. *)
let replace_nbr nbr new_nbr node =
    match node with
    | Leaf(id1, nbr1) when (nbr1 = nbr) -> Leaf(id1, new_nbr)
    | Interior(id1, nbr1, nbr2, nbr3) when (nbr1 = nbr) ->
        Interior(id1, new_nbr, nbr2, nbr3)
    | Interior(id1, nbr1, nbr2, nbr3) when (nbr2 = nbr) ->
        Interior(id1, nbr1, new_nbr, nbr3)
    | Interior(id1, nbr1, nbr2, nbr3) when (nbr3 = nbr) ->
        Interior(id1, nbr1, nbr2, new_nbr)
    | _ -> raise
            (Invalid_argument "Single node or Incorrect nbr passed in")

(** [replace_nbr_in_tree nbr new_nbr id tree]
    @return returns the tree with node(id) changed. nbr of node(id) is
            replaced with new_nbr. *)
let replace_nbr_in_tree nbr new_nbr id btree =
    let node = (get_node id btree) in
    let new_node = (replace_nbr nbr new_nbr node) in
    let btree = remove_node id btree in
    let btree = add_node new_node btree in
    btree

(** [get_parent id tree]
    @return the nbr, such that Edge(nbr, id) exists in tree. *)
let get_parent id tree =
    match get_node id tree with
    | Leaf(a, nb) -> nb
    | Interior(a, p, l, r) -> p
    | _ -> raise (Invalid_argument "Single Handle passed in")

(** [get_child_leaves tree id]  *)
let get_child_leaves tree id =
    let rec desc acc id =
        match get_node id tree with
        | Leaf (id, par) -> id :: acc
        | Interior (id, par, left, right) ->
              let acc = desc acc left in
              let acc = desc acc right in
              acc
        | _ -> assert false in
    if is_handle id tree
    then begin
        match get_node id tree with
        | Leaf (id, par) -> desc [id] par
        | Interior (id, par, left, right) ->
              let acc = desc [] par in
              let acc = desc acc left in
              let acc = desc acc right in
              acc
        | Single id -> [id]
    end
    else desc [] id

(** [test_component tree handle] performs consistency tests on a component of a
    tree. *)
let test_component tree handle =
    let test_parent e1 e2 =
        let parent = get_parent e2 tree in
        assert (parent = e1); () in
    let test_edge e1 e2 =
        assert (is_edge (Edge (e1, e2)) tree);
        test_parent e1 e2 in
    let test_node e1 = match get_node e1 tree with
    | Single id -> assert (id = e1); ()
    | Leaf (id, nbr) -> assert (nbr <> id); assert (id = e1); ()
    | Interior (id, n1, n2, n3) ->
          assert (id = e1);
          assert (id <> n1);
          assert (id <> n2);
          assert (id <> n3);
          assert (n1 <> n2);
          assert (n1 <> n3);
          assert (n2 <> n3); () in
    let rec r prev e1 = match get_node e1 tree with
    | Single _ -> assert false
    | Leaf (id, n) ->
          assert (prev = n);
          test_node e1;
          test_edge n id
    | Interior (id, n1, n2, n3) ->
          assert (prev = n1);
          test_node e1;
          test_edge n1 id;
          r id n2;
          r id n3 in
    match get_node handle tree with
    | Single id -> test_node handle
    | Leaf (id, nbr) -> test_node handle; r handle nbr
    | Interior (id, n1, n2, n3) ->
          assert (handle = id);
          test_node handle;
          r handle n1;
          r handle n2;
          r handle n3

(** [test_tree tree] performs consistency tests on a tree *)
let test_tree tree =
    All_sets.Integers.iter (test_component tree) tree.handles

(******************* END - UNSAFE OPS ****************************************)

(** [a --> b] performs [b a], allowing for chaining of tree-modifying functions
*)
let (-->) a b = b a

let add_tree_to d add_to tree =
    incr Data.median_code_count;
    let cg () = 
        incr Data.median_code_count; 
        !(Data.median_code_count)
    in
    let rec assign_codes parent data = function
        | Parser.Tree.Leaf name ->
                let tc = 
                    try Data.taxon_code name d with
                    | Not_found as err ->
                            Status.user_message Status.Error
                            ("Could@ not@ find@ data@ loaded@ for@ taxon@ " ^
                            name ^ "@ in@ a@ loaded@ tree.");
                            raise err
                in
                Parser.Tree.Leaf (Leaf (tc, parent)), tc
        | Parser.Tree.Node (child_nodes, txt) ->
                let rec resolve_more_children = function
                    | [a; b] as x -> x
                    | [taxon] ->
                            Status.user_message Status.Error
                            ("Your@ tree@ file@ has@ a@ subtree@ or@ taxon@ " ^
                            "occurring@ as@ the@ unique@ member@ of@ an@ " ^
                            "internal@ node@ in@ the@ tree@ " ^
                            "(maybe@ there@ is@ a@ space@ missing@ between@ " ^
                            "taxa?).@ I@ am@ cancelling@ this@ read@ command");
                            failwith "Illegal Tree file format"
                    | [] -> 
                            Status.user_message Status.Error
                            ("Your@ tree@ file@ has@ a@ (),@ that@ is,@ " ^
                            "an@ opening@ parentheses@ followed@ immediately@ "
                            ^ "by@ a@ closing@ parentheses.@ Empty@ trees@ "
                            ^ "are@ not@ allowed@ in@ a@ tree@ file,@ so@ I@ " ^
                            "am@ cancelling@ this@ read@ command");
                            failwith "Illegal Tree file format"
                    | a :: b :: t ->
                            resolve_more_children ((Parser.Tree.Node ([a; b],
                            txt)) :: t)
                in
                let sc = cg () in
                let child_nodes = resolve_more_children child_nodes in
                match child_nodes with
                | [a; b] ->
                        let ta, ca = assign_codes sc data a in
                        let tb, cb = assign_codes sc data b in
                        Parser.Tree.Node ([ta; tb], 
                        Interior (sc, parent, ca, cb)), sc
                | _ -> failwith "Tree.assign_codes"
    in
    let add_edge a b = EdgeSet.add (Edge (a, b)) in
    let remove_edge a b = EdgeSet.remove (Edge (a, b)) in
    let replace_parent vertices v par = 
        let vertex = All_sets.IntegerMap.find v vertices in
        let vertex = 
            match vertex with
            | Interior (a, _, b, c) -> Interior (a, par, b, c)
            | Leaf (a, _) -> Leaf (a, par)
            | Single _ -> failwith "Unexpected Tree.replace_parent"
        in
        All_sets.IntegerMap.add v vertex vertices
    in
    let rec add_edges_n_vertices tree (edges, vertices) =
        match tree with
        | Parser.Tree.Node ([x; y], ((Interior (a, b, c, d)) as v)) ->
                let vertices = All_sets.IntegerMap.add a v vertices 
                and edges =  edges --> add_edge b a --> add_edge a c -->
                    add_edge a d
                in
                add_edges_n_vertices x 
                (add_edges_n_vertices y (edges, vertices))
        | Parser.Tree.Leaf ((Leaf (a, b)) as v) ->
                let vertices = All_sets.IntegerMap.add a v vertices 
                and edges = add_edge b a edges in
                (edges, vertices)
        | _ -> failwith "Unexpected Tree.add_edges_n_vertices"
    in
    let tree =  
        (* We clean up a tree that has only one leaf, this is valid when
        processing forest, but invalid inside a tree. *)
        match tree with
        | (Parser.Tree.Leaf _) as x
        | Parser.Tree.Node ([((Parser.Tree.Leaf _) as x)], _) -> x
        | y -> y
    in
    match assign_codes (-1) d tree with
    | Parser.Tree.Node ([a; b], (Interior (sc, _, ca, cb))), _ ->
            let edges, vertices = 
                add_edges_n_vertices b
                (add_edges_n_vertices a 
                (add_to.d_edges, add_to.u_topo))
            in
            let vertices = replace_parent vertices ca cb in
            let vertices = replace_parent vertices cb ca in
            let edges = edges -->
                remove_edge sc ca --> remove_edge sc cb -->
                    add_edge ca cb
            in
            let handles = All_sets.Integers.add ca add_to.handles in
            { u_topo = vertices; d_edges = edges; handles = handles;
            avail_ids = []; new_ids = cg () }
    | Parser.Tree.Leaf (Leaf (tc, _)), _ -> 
            let vertices = 
                All_sets.IntegerMap.add tc (Single tc)
                add_to.u_topo 
            in
            let handles = All_sets.Integers.add tc add_to.handles in
            { add_to with u_topo = vertices; handles = handles }
    | _ ->failwith "We need trees with more than two taxa"


let convert_to trees data = 
    List.fold_left (add_tree_to data) empty trees

(** [make_disjoint_tree n]
    @return a disjointed tree with the given nodes and 0 edges *)
let make_disjoint_tree nodes =
    let f tr m =
        let tr = (add_node (Single(m)) tr) in
        let tr = (add_handle m tr) in
            tr
    in
        (List.fold_left f empty nodes)

(** [get_children id tree] returns the two children of node [id] in [tree]
    @raise [Failure] if the node has no children *)
let get_children id tree =
    match get_node id tree with
    | Interior (node, parent, c1, c2) -> c1, c2
    | _ -> failwith "get_children"

(** [handle_of n tree] returns the handle of the component in [tree] containing
    node [n] *)
let rec handle_of n tree =
    if debug_handle_of
    then odebug ("handle of: " ^ string_of_int n);
    if is_handle n tree
    then n
    else handle_of (get_parent n tree) tree

(** [get_path_to_handle id tree]
    @return this function returns the path from node to the handle.
            The path is a list of directed edges. *)
let get_path_to_handle id tree =
    let rec aux id elist =
        if (is_handle id tree) then
            id, elist
        else
            let nbr = (get_parent id tree) in
            let edge = (Edge(nbr, id)) in
                (aux nbr (edge :: elist))
    in
        (aux id [])

let get_vertices_to_handle id tree =
    let rec aux id acc =
        if is_handle id tree then 
            List.rev (id :: acc)
        else 
            let nbr = get_parent id tree in
            aux nbr (id :: acc)
    in
    aux id []

(** [path_up ?acc nfrom nto tree] returns the path from [nfrom] towards the
    root to [nto]; the path is returned in reverse order, with [nto] first *)
let rec path_up ?(acc=[]) nfrom nto tree =
    try
        if nfrom = nto then nto :: acc
        else path_up ~acc:(nfrom :: acc) (get_parent nfrom tree) nto tree
    with
    | err ->
            Status.user_message Status.Error ("nfrom: " ^ string_of_int nfrom ^
            " nto: " ^ string_of_int nto);
            raise err

(** [reorient_node parent child tree] changes the !Interior structore of
    [child] so that [parent] is its parent (i.e., is in the first position) *)
let reorient_node parent child tree =
    match get_node child tree with
    | Interior (node, c1, c2, c3) ->
          if parent = c1
          then tree
          else if parent = c2
          then begin
              let tree = remove_node child tree in
              let tree = add_node (Interior (child, parent, c1, c3)) tree in
              tree
          end else if parent = c3
          then begin
              let tree = remove_node child tree in
              let tree = add_node (Interior (child, parent, c1, c2)) tree in
              tree
          end else failwith "reorient_node"
    | Leaf (node, c) ->
          if parent = c
          then tree
          else failwith "reorient_node"
    | Single _ -> failwith "reorient_node"


(** [fix_path_to_handle tree list] changes nodes and reorients edges to provide
    consistency when moving a handle *)
let rec fix_path_to_handle tree list = match list with
| [] -> tree
| [new_handle] -> tree
| p :: c :: list ->
      let tree = tree --> remove_edge (Edge (p, c))
          --> add_edge (Edge (c, p)) --> reorient_node c p in
      fix_path_to_handle tree (c :: list)


(** Function to move the node handle from one node to another.
    This will change the directions of the edges to account
    for this change. *)
let move_handle node_id tree =
    let handle = handle_of node_id tree in
    let path = path_up node_id handle tree in
    let tree = fix_path_to_handle tree path in
    let tree = tree --> remove_handle handle --> add_handle node_id in
    if debug_tests then test_tree tree;
    tree, path

(** [other_two_nbrs nbr int_node]
    @return returns the other two nbrs of the interior node. *)
let other_two_nbrs nbr node =
    match node with
    | Interior (id, nbr1, nbr2, nbr3) ->
            if (nbr = nbr1) then (nbr2, nbr3)
            else if (nbr = nbr2) then (nbr1, nbr3)
            else begin
                assert (
                    if nbr = nbr3 then true
                    else begin
                        let mst = 
                            Printf.sprintf "This is the failure point: %d but the \
                            neighbors of %d are %d %d and %d" nbr id nbr1 nbr2
                            nbr3 
                        in
                        let _ = Status.user_message Status.Error mst in
                        false
                    end);
                (nbr1, nbr2)
            end
    | _ -> raise (Invalid_argument "Not interior node")

(** [break edge btree] breaks the tree at the jxn.
    @param edge the junction {!Tree.break_jxn} where the tree should
    be broken.
    @param btree the binary tree whose edge is being broken.
    @return an {!Tree.aux_tree}, the new binary tree with the edge
            removed and the changes to the difference between the old
            and new trees as a {!Tree.tree_delta} structure. *)
let break jxn tree =
    let (e1, e2) = jxn in

    assert(verify_edge (Edge (e1, e2)) tree);

    (* Check whether root is on left side or right side *)
    let handle_left = is_edge (Edge (e1, e2)) tree in

    (* Handle the left side *)

    let node1 = get_node e1 tree in
    let node2 = get_node e2 tree in
    let handle_side handle_our_side node other_vertex tree = 
        match node with
        | Leaf (id, _) ->
              (* if it's a leaf: make it a single *)
              let tree = tree --> remove_node id --> add_node (Single id) in

              (* add a handle if it's not already *)
              let tree =
                  if not handle_our_side
                  then add_handles [id] tree
                  else tree in

              tree, id, Single_Jxn id, `Single (id, not handle_our_side)

        | Interior (id, n1, n2, n3) ->
              let l = id in
              let l1, l2 = other_two_nbrs other_vertex node in
              let new_root = not handle_our_side || (is_handle l tree) in

              let tree, jxn, root =
                  if new_root
                  then begin
                      let tree =
                          if is_handle l tree then remove_handle l tree else tree in
                      let tree = tree --> add_handle l1 --> add_edge (Edge (l1, l2))
                          --> replace_nbr_in_tree l l2 l1
                          --> replace_nbr_in_tree l l1 l2
                          --> reorient_node l1 l2
                          --> reorient_node l2 l1
                      in
                      tree, Edge_Jxn (l1, l2),Some l1
                  end
                  else begin
                      let li, lj =
                          if is_edge (Edge (l1, l)) tree then l1, l2 else l2, l1 in
                      let tree = tree --> add_edge (Edge (li, lj))
                          --> replace_nbr_in_tree l li lj
                          --> replace_nbr_in_tree l lj li
                          --> reorient_node li lj
                      in
                      tree, Edge_Jxn (li, lj), None
                  end in
              let tree = tree
                  --> remove_edge_either (Edge(l, l1))
                  --> remove_edge_either (Edge(l, l2))
                  --> remove_node l in

              tree, id, jxn, `Edge (l, l1, l2, root)
        | Single _ -> assert false
    in

    let tree, t1_handle, t1_jxn, ld = handle_side handle_left node1 e2 tree in
    let tree, t2_handle, t2_jxn, rd = handle_side (not handle_left) node2 e1 tree
    in

    let tree = remove_edge_either (Edge(e1, e2)) tree in

    if debug_tests then test_tree tree;
    tree, (ld, rd)


(** [make_minimal_tree]
    @return the minimal unrooted tree with 3 leaf nodes. *)
let make_minimal_tree () =
    let nd1 = Interior(1, 2, 3, 4) and
        nd2 = Leaf(2, 1) and
        nd3 = Leaf(3, 1) and
        nd4 = Leaf(4, 1) and
        e1 = Edge(1, 2) and
        e2 = Edge(1, 3) and
        e3 = Edge(1, 4) in
    let bt = (add_nodes [nd1; nd2; nd3; nd4] empty) in
    let bt = (add_edges [e1; e2; e3] bt) in
    let bt = (add_handle 1 bt) in
    if debug_tests then test_tree bt;
        bt

(** [edge_map f tree]
    @param f function to be applied to each edge of the tree.
    @param tree tree whose edges to which f is applied.
    @return the function is applied to each edge of the tree. The
            results are in the form of an assoc-list. *)
let edge_map f tree =
    let edges = (EdgeSet.elements tree.d_edges) in
    let results = (List.map f edges) in
        (List.combine edges results)

(** [join jxn1 jxn2 atree]
    @param jxn1 jxn in the first tree to which the second tree will
                be grafted onto.
    @param jxn2 jxn in the second tree which will be grafted to a jxn
                in the first tree.
    @param atree aux_tree.
    @return an aux tree with the two smaller trees grafted onto a larger
            tree. *)
let join jxn1 jxn2 tree =
    test_tree tree;
    (* The junctions should come from different components *)
    assert (handle_of (jxn_choose_node jxn1) tree
            <> handle_of (jxn_choose_node jxn2) tree);
    (* [split node edge tree] *)
    let split ex_nd edge bt =
        let Edge (e1, e2) = edge in
        bt --> replace_nbr_in_tree e1 ex_nd e2
           --> replace_nbr_in_tree e2 ex_nd e1
           --> remove_edge edge
           --> add_edge (Edge (e1, ex_nd))
           --> add_edge (Edge (ex_nd, e2))
    in

    (* changes and return tree, left new node id, and a function to make
       the actual node given its neighbor's id *)
    let tree, lid, lnode, ldel = 
        match jxn1 with
        | Single_Jxn id ->
                tree --> remove_reserved_node id, id, (fun nbr -> Leaf (id, nbr)), 
                `Single (id, false)
        | Edge_Jxn (e1, e2) ->
                let Edge (e1, e2) = normalize_edge (Edge (e1, e2)) tree in
                let n, tree = get_available tree in
                let tree = split n (Edge (e1, e2)) tree in
                tree, n, (fun nbr -> Interior (n, e1, e2, nbr)), `Edge (n, e1, e2, None) 
    in
    (* make changes and return tree, right new node id, a function to make the
       actual node given its neighbor's id, and a list of nodes that have to be
       changed in direction *)
    let tree, rid, rnode, rdel, rpath = 
        match jxn2 with
        | Single_Jxn id ->
                let tree = tree --> remove_handle id --> remove_reserved_node id in
                tree, id, (fun nbr -> Leaf (id, nbr)), `Single (id, true), [id]
        | Edge_Jxn (e1, e2) ->
                (* This case is the most difficult, as it puts the tree on the RHS
                into a bad state.  We proceed as follows.   *)

                (* make edge e1->e2 so that e1 is closer to the root *)
                let Edge (e1, e2) = normalize_edge (Edge (e1, e2)) tree in
                let handle = handle_of e1 tree in
                let n, tree = get_available tree in
                let tree = remove_handle handle tree in

                (* fix nodes along the path *)
                (* (this returns path with handle as the first element) *)
                let path = 
                    try path_up ~acc:[n] e1 handle tree 
                    with err ->
                        Status.user_message Status.Error (Printf.sprintf "The \
                        error is with new node %d\n%!" n);
                        raise err
                in
                let Edge (e1, e2) = normalize_edge (Edge (e1, e2)) tree in
                let tree = split n (Edge (e1, e2)) tree in
                let tree = tree --> reorient_node n e1 --> reorient_node n e2 in
                let tree = fix_path_to_handle tree path in
                tree, n, (fun nbr -> Interior (n, nbr, e1, e2)),
                `Edge (n, e1, e2, Some handle), path 
    in
    let n1 = lnode rid in
    let n2 = rnode lid in
    let tree = 
        tree --> add_edge (Edge (lid, rid)) --> add_node n1 --> add_node n2 
    in
    let path = rpath in
    if debug_tests then test_tree tree;
    tree, (ldel, rdel, path)

(** [pre_order_edge_map f id btree]
    @param id id of the node whose subtree will be mapped.
    @param  f function that is applied to each edge.
    @param bt the tree whose edges will be mapped by this function.
    @return (edge, (f edge)) list. *)
let pre_order_edge_map f id bt =
    let rec visit_node pred id accum =
        match (get_node id bt) with
        | Leaf(nd, nbr) ->
              assert(pred = nbr);
              let e = Edge(pred, nd) in
              let r = (e, (f e)) in
              r :: accum
        | Interior(nd, nbr1, nbr2, nbr3) as node ->
              let (x, y) = (other_two_nbrs pred node) in
              let e = Edge(pred, nd) in
              let r = (e, (f e)) in
              let accum = r :: accum in
              let accum = (visit_node nd x accum) in
              let accum = (visit_node nd y accum) in
              accum
        | _ -> raise (Invalid_argument "Check node type")
    in
    match (get_node id bt) with
    | Leaf(nd, nbr) ->
          assert(is_edge (Edge (nd, nbr)) bt);
          let accum = (visit_node nd nbr []) in
          accum
    | Interior(nd, nbr1, nbr2, nbr3) as node ->
          begin
              match (is_handle nd bt) with
              | true ->
                    let accum = (visit_node nd nbr1 []) in
                    let accum = (visit_node nd nbr2 accum) in
                    let accum = (visit_node nd nbr3 accum) in
                    accum
              | false ->
                    let pred = (get_parent nd bt) in
                    let (x, y) = (other_two_nbrs pred node) in
                    let accum = (visit_node nd x []) in
                    let accum = (visit_node nd y accum) in
                    accum
          end
    | Single(_) -> []

(** [pre_order_edge_iter f id btree]
    @param id id of the node whose subtree will be iter'd.
    @param  f function that is applied to each edge.
    @param bt the tree whose edges will be iter'd by this function.
    @return () *)
let pre_order_edge_iter f id bt =
    let rec visit_node pred id =
        match (get_node id bt) with
        | Leaf(nd, nbr) ->
              assert(pred = nbr);
              let e = Edge(pred, nd) in
              (f e)
        | Interior(nd, nbr1, nbr2, nbr3) as node ->
              let (x, y) = (other_two_nbrs pred node) in
              let e = Edge(pred, nd) in
              (f e);
              (visit_node nd x);
              (visit_node nd y)
        | _ -> raise (Invalid_argument "Check node type")
    in
    match (get_node id bt) with
    | Leaf(nd, nbr) ->
          assert(is_edge (Edge (nd, nbr)) bt);
          (visit_node nd nbr)
    | Interior(nd, nbr1, nbr2, nbr3) as node ->
          begin
              match (is_handle nd bt) with
              | true ->
                    (visit_node nd nbr1);
                    (visit_node nd nbr2);
                    (visit_node nd nbr3)
              | false ->
                    let pred = (get_parent nd bt) in
                    let (x, y) = (other_two_nbrs pred node) in
                    (visit_node nd x);
                    (visit_node nd y)
          end
    | Single(_) -> ()

(** [pre_order_edge_visit_up_to_depth f id ptree accum d]
@param f function to be applied to each edge of the subtree
         of ptree rooted at id.
@param id all the edges that are descendents of this node are visited
          in pre-order manner.
@param ptree the ptree whose edges are being visited.
@param accum the accumulator, a list.
@return the return values of the function being applied on the edges. *)
let pre_order_edge_visit_with_depth f id bt accum max_d =
    let break_or_continue pred nd ret c_with s_flag cur_depth =
        let status, accum = ret in
        match status with
        | Continue ->
              (c_with pred nd accum cur_depth)
        | Break -> (Break, accum)
        | Skip ->
              begin
                  match s_flag with
                  | true -> (c_with pred nd accum cur_depth)
                  | false -> (Continue, accum)
              end
    in
    let visit_node pred nd acc =
        let e = Edge(pred, nd) in
        (f e acc)
    in
    let rec process_children pred nd accum cur_depth =
        let node = (get_node nd bt) in
        let (x, y) = other_two_nbrs pred node in
        let x, y =
            match Random.int 2 with
            | 0 -> x, y
            | _ -> y, x
        in
        let ret = traverse nd x accum (cur_depth + 1) in
        (break_or_continue nd y ret traverse true (cur_depth + 1))
    and traverse pred id accum cur_depth =
        match (get_node id bt) with
        | Leaf(nd, nbr) ->
              assert(pred = nbr);
              (visit_node pred id accum)
        | Interior(nd, nbr1, nbr2, nbr3) when cur_depth < max_d ->
              let ret = (visit_node pred nd accum) in
              (break_or_continue pred nd ret process_children false cur_depth)
        | Interior(nd, nbr1, nbr2, nbr3) ->
              (visit_node pred nd accum)
        | _ -> raise (Invalid_argument "poev 2: Check node type")
    in
    match (get_node id bt) with
    | Leaf(nd, nbr) ->
          assert(is_edge (Edge (nd, nbr)) bt);
          let _, accum = (traverse nd nbr accum 1) in
          accum
    | Interior(nd, nbr1, nbr2, nbr3) ->
          begin
              match (is_handle nd bt) with
              | true ->
                    let ret = (traverse nd nbr1 accum max_d) in
                    let _, accum =
                        (break_or_continue nbr1 nd ret process_children true
                             1) in
                    accum
              | false ->
                    let pred = (get_parent nd bt) in
                    let _, accum = (process_children pred nd accum 1) in
                    accum
          end
    | Single(_) -> accum

(** [pre_order_edge_visit f id ptree accum]
@param f function to be applied to each edge of the subtree
         of ptree rooted at id.
@param id all the edges that are descendents of this node are visited
          in pre-order manner.
@param ptree the ptree whose edges are being visited.
@param accum the accumulator, a list.
@return the return values of the function being applied on the edges. *)
let pre_order_edge_visit f id bt accum =
    let break_or_continue pred nd ret c_with s_flag =
        let status, accum = ret in
        match status with
        | Continue ->
              (c_with pred nd accum)
        | Break -> (Break, accum)
        | Skip ->
              begin
                  match s_flag with
                  | true -> (c_with pred nd accum)
                  | false -> (Continue, accum)
              end
    in
    let visit_node pred nd acc =
        let e = Edge(pred, nd) in
        (f e acc)
    in
    let rec process_children pred nd accum =
        let node = (get_node nd bt) in
        let (x, y) = other_two_nbrs pred node in
        let x, y =
            match Random.int 2 with
            | 0 -> x, y
            | _ -> y, x
        in
        let ret = (traverse nd x accum) in
        (break_or_continue nd y ret traverse true)
    and traverse pred id accum =
        match (get_node id bt) with
        | Leaf(nd, nbr) ->
              assert(pred = nbr);
              (visit_node pred id accum)
        | Interior(nd, nbr1, nbr2, nbr3) ->
              let ret = (visit_node pred nd accum) in
              (break_or_continue pred nd ret process_children false)
        | _ -> raise (Invalid_argument "poev 2: Check node type")
    in
    let assertion_test_for_debugging edge tree =
        if is_edge edge tree then true
        else begin
            let Edge (a, b) = edge in
            Printf.printf "Assertion failure:\n";
(*             print_tree_delta tree; *)
            Printf.printf "I expected to find the edge %d %d\n%!" a b;
            false
        end
    in
    match (get_node id bt) with
    | Leaf(nd, nbr) ->
          assert(assertion_test_for_debugging (Edge (nd, nbr)) bt);
          let _, accum = (traverse nd nbr accum) in
          accum
    | Interior(nd, nbr1, nbr2, nbr3) ->
          begin
              match (is_handle nd bt) with
              | true ->
(*                     print_endline "TREE: this is a handle!!"; *)
                    let ret = (traverse nd nbr1 accum) in
                    let _, accum =
                        (break_or_continue nbr1 nd ret process_children true) in
                    accum
              | false ->
(*                     print_endline "TREE: not a handle."; *)
                    let pred = (get_parent nd bt) in
                    let _, accum = (process_children pred nd accum) in
                    accum
          end
    | Single(_) -> accum

(** [pre_order_node_visit f id bt ad acc]
@param f function to applied to all the nodes in pre-order.
@param id the node_id from where the traversal is started.
@param bt the tree whose nodes are visited.
@param acc the result of the function application on the edges.
@return the function applied to the subtree of id as imposed by the
        handle in the component, the results are returned as a list. *)
let pre_order_node_visit f id bt accum =
    let visit_node pred nd acc =
        (f pred nd acc)
    in
    let break_or_continue pred nd ret c_with s_flag =
        let status, accum = ret in
        match status with
        | Continue ->
              (c_with pred nd accum)
        | Break -> (Break, accum)
        | Skip ->
              begin
                  match s_flag with
                  | true -> (c_with pred nd accum)
                  | false -> (Continue, accum)
              end
    in
    let rec process_children pred nd acc =
        match pred with
        | Some p ->
              let node = (get_node nd bt) in
              let (x, y) = (other_two_nbrs p node) in
              let ret = (traverse (Some nd) x acc) in
              (break_or_continue (Some nd) y ret traverse true)
        | _ -> raise (Invalid_argument "Needs valid predecessor.")
    and traverse pred id accum =
        match (get_node id bt) with
        | Leaf(nd, nbr) ->
              begin
                  match pred with
                  | None -> assert(is_handle id bt)
                  | Some(p) -> assert(p = nbr)
              end;
              (visit_node pred nd accum)
        | Interior(nd, nbr1, nbr2, nbr3) ->
              let ret = (visit_node pred nd accum) in
              (break_or_continue pred nd ret process_children false)
        | _ -> raise (Invalid_argument "ponv 1: Check node type")
    in
    match (get_node id bt) with
    | Leaf(nd, nbr) ->
        (* if the node is a leaf and the edge is pointing into the node,
         * then we just visit the one node and stop. *)
          let ret = (visit_node None nd accum) in
          if (is_edge (Edge (nd, nbr)) bt) then
              let _, acc =
                  (break_or_continue (Some nd) nbr ret traverse false) in
              acc
          else
              let _, acc = ret in
              acc
    | Interior(nd, nbr1, nbr2, nbr3) ->
          begin
              match (is_handle nd bt) with
              | true ->
                    let ret = (visit_node None nd accum) in
                    let ret =
                        (break_or_continue (Some nd) nbr1 ret traverse true) in
                    let _, acc =
                        (break_or_continue (Some nbr1) nd
                             ret process_children true) in
                    acc
              | false ->
                    let pred = (get_parent nd bt) in
                    let ret = (visit_node (Some pred) nd accum) in
                    let _, acc =
                        (break_or_continue (Some pred) nd
                             ret process_children false) in
                    acc
          end
    | Single(_) -> accum


let post_order_node_with_edge_visit f g (Edge (a, b)) bt accum =
    let rec processor prev curr accum =
        match get_node curr bt with
        | Leaf (nd, nbr) ->
                f prev curr accum
        | Interior (nd, nbr1, nbr2, nbr3) as node ->
                let a, b = other_two_nbrs prev node in
                let aacc = processor nd a accum
                and bacc = processor nd b accum in
                g prev curr aacc bacc
        | Single _ -> accum
    in
    let a = processor b a accum
    and b = processor a b accum in
    a, b

(** [post_order_node_with_edge_visit_simple f e t a] is a simplified visitor
* function [f], which is applied on every (non single) vertex, starting in 
* the (hypothetical) root located between the two vertices of edge [e], over
* tree [t] with accumulator [a]. *)
let post_order_node_with_edge_visit_simple f (Edge (a, b)) bt acc =
    let rec processor prev curr acc =
        match get_node curr bt with
        | Leaf (nd, nbr) ->
                f prev curr acc
        | Interior (nd, nbr1, nbr2, nbr3) as node ->
                let a, b = other_two_nbrs prev node in
                acc --> 
                    processor curr a 
                    --> processor curr b 
                    --> f prev curr
        | Single _ -> acc
    in
    acc --> processor b a --> processor a b

(** [post_order_node_visit f id bt ad acc]
@param f function to applied to all the nodes in post-order.
@param id the node_id from where the traversal is started.
@param bt the tree whose nodes are visited.
@param ad auxilliary data used by the function - scratch space.
@param acc the result of the function application on the edges.
@return the function applied to the subtree of id as imposed by the
        handle in the component, the results are returned as a list. *)
let post_order_node_visit f id bt accum =
    let visit_node pred nd acc =
        (f pred nd acc)
    in
    let break_or_continue pred nd ret c_with =
        let status, acc = ret in
        match status with
        | Continue ->
              (c_with pred nd acc)
        | Break -> (Break, acc)
        | _ -> (failwith "Invalid status returned")
    in
    let rec process_children pred nd acc =
        match pred with
        | Some p ->
              let node = (get_node nd bt) in
              let (x, y) = (other_two_nbrs p node) in
              let ret = (traverse (Some nd) x acc) in
              (break_or_continue (Some nd) y ret traverse)
        | _ -> raise (Invalid_argument "Needs a predecessor")
    and traverse pred id accum =
        match (get_node id bt) with
        | Leaf(nd, nbr) ->
              begin
                  match pred with
                  | None -> assert(is_handle id bt)
                  | Some p -> assert(p = nbr);
              end;
              (visit_node pred nd accum)
        | Interior(nd, nbr1, nbr2, nbr3) ->
              let ret = (Continue, accum) in
              let ret =
                  (break_or_continue pred nd ret process_children)
              in
              (break_or_continue pred nd ret visit_node)
        | _ -> raise (Invalid_argument "ponv 1: Check node type")
    in
    match (get_node id bt) with
    | Leaf(nd, nbr) ->
            (* if the start node is a leaf node and the edge is pointing into
             * the leaf, just visit the leaf. *)
(*          if (is_edge (Edge (nd, nbr)) bt) then*)
              let ret = (Continue, accum) in
              let ret = (break_or_continue (Some nd) nbr ret traverse) in
              let _, acc =
                  (break_or_continue (Some nbr) nd ret visit_node) in
              acc
(*          else*)
(*              let _, acc = visit_node None nd accum in*)
(*              acc*)
    | Interior(nd, nbr1, nbr2, nbr3) ->
          begin
              match (is_handle nd bt) with
              | true ->
                    let ret = (Continue, accum) in
                    let ret =
                        (break_or_continue (Some nd) nbr1 ret traverse)
                    in
                    let ret =
                        let tmp = Some nbr1 in
                        (break_or_continue tmp nd ret process_children)
                    in
                    let _, acc =
                        (break_or_continue (Some nbr1) nd ret visit_node)
                    in
                    acc
              | false ->
                    let pred = (get_parent nd bt) in
                    let ret = (Continue, accum) in
                    let ret =
                        let tmp = Some pred in
                        (break_or_continue tmp nd ret process_children)
                    in
                    let _, acc =
                        (break_or_continue (Some pred) nd ret visit_node)
                    in
                    acc
          end
    | Single(_) -> accum

(** [get_pre_order_edges hs tree]
    @param hs the start handle.
    @param tree the tree whose edges in pre-order are desired.
    @return the edges of the tree as visited by a pre_order traversal
            in list form. *)
let get_pre_order_edges hs tree =
    let get_edge e acc =  Continue, e :: acc in
    let rev_edges = (pre_order_edge_visit get_edge hs tree []) in
       List.rev rev_edges

(** [get_edges tree hs] returns the edges in a tree component in an arbitrary
    order *)
let get_edges ?(acc=[]) tree hs =
    let get_edge e acc =  Continue, e :: acc in
    pre_order_edge_visit get_edge hs tree acc

(** [get_edges_tree tree] returns all the edges in the tree in an arbitrary
    order *)
let get_edges_tree tree =
    List.fold_left
        (fun acc h -> get_edges ~acc tree h)
        []
        (handle_list tree)

(** [get_post_order_edges hs tree]
    @param hs the start handle.
    @param tree the tree whose edges in ppost_order are desired.
    @return the edges of the tree as visited in a post-order traversal
            in list form.
let get_post_order_edges hs tree =
    let get_edge e acc =  Continue, e :: acc in
    let rev_edges = (post_order_edge_visit get_edge hs tree []) in
       List.rev rev_edges *)

(*****************************************************************************)

(** [print_edge edge]
    @return prints the edge as a tuple. *)
let print_edge (Edge(e1, e2)) =
    let e1_s = (string_of_int e1) and
        e2_s = (string_of_int e2) in
    print_string ("(" ^ e1_s ^ "," ^ e2_s ^ ")")


(** [print_tree tree]
    @param id id of the node that will be used as a starting point.
    @param tree tree being printed.
    @return () - this function prints the binary tree to the screen
            using a backward-in-order traversal. *)
let print_tree id tree =
    let pipe = "|" and
        empty_string = "" and
        suffix = " " in
    let update_prefix prefix =
        (prefix ^ suffix ^ pipe) in
    let already_visited node_id visited =
        (All_sets.Integers.mem node_id visited) in
    let mark_visited node_id visited =
        (All_sets.Integers.add node_id visited) in
    let visit_node node_id prefix =
        (print_endline (prefix ^ (string_of_int node_id))) in
    let rec visit_nbr nbr_id prefix visited =
        if (already_visited nbr_id visited) then
            visited
        else
            (print_tree_aux nbr_id prefix visited)
    and print_tree_aux node_id prefix visited =
        match (get_node node_id tree) with
        | Single(id) ->
            (visit_node id prefix);
            (mark_visited id visited)
        | Leaf(id, pid) ->
            let visited_n = (mark_visited node_id visited) in
            let new_prefix = (update_prefix prefix) in
            let visited_n_p = (visit_nbr pid new_prefix visited_n) in
                (visit_node id prefix);
                visited_n_p
        | Interior(id, pid, lid, rid) ->
            let visited_n = (mark_visited node_id visited) in
            let new_prefix = (update_prefix prefix) in
            let visited_p = (visit_nbr pid new_prefix visited_n) in
            let visited_n_l = (visit_nbr lid new_prefix visited_p) in
                (visit_node id prefix);
                (visit_nbr rid new_prefix visited_n_l)
    in
        let _ =
            (print_tree_aux id empty_string All_sets.Integers.empty) in
            ()

(** [print_forest forest]
    @return () - prints all the trees in the forest. *)
let print_forest forest =
    let handles = (All_sets.Integers.elements (get_handles forest)) in
        (ignore (List.map (fun x -> print_newline ();
                                    print_tree x forest) handles))

module Fingerprint = struct
    type t = int list
    let empty : t = []

    let find_smallest_leaf {u_topo = topo} =
        All_sets.IntegerMap.fold
            (fun k _ m -> min k m) topo max_int
    let move_handle_to_smallest t =
        let r, _ = move_handle (find_smallest_leaf t) t in
        r
    let fingerprint t =
        let smallest = find_smallest_leaf t in
        let t, _ = move_handle smallest t in
        let parent_id = match get_node smallest t with
        | Leaf (_, p) -> p
        | _ -> failwith "Bad leaf node" in
        let rec fp p = function
            | Single _ -> failwith "Strange place to leave a single"
            | Leaf (myid, _) -> [myid]
            | (Interior (myid, c1, c2, c3)) as node ->
                  let c1, c2 = other_two_nbrs p node in
                  let c1l = fp myid (get_node c1 t) in
                  let c2l = fp myid (get_node c2 t) in
                  let c1h = List.hd c1l in
                  let c2h = List.hd c2l in
                  if c1h < c2h
                  then c1l @ c2l
                  else c2l @ c1l
        in smallest :: (fp smallest (get_node parent_id t))

    let compare = compare

    let to_string t = String.concat ", " (List.map string_of_int t)
end


module CladeFP = struct
    type fp = All_sets.Integers.t
    type t = fp EdgeMap.t
    let fpcompare = All_sets.Integers.compare

    (** [Ordered] allows for maps and sets of fingerprints *)
    module Ordered = struct
        type t = fp
        let compare = fpcompare
    end

    let calc tree =
        let module Set = All_sets.Integers in
        let module Map = EdgeMap in
        let calc_component h m =
            let comp_leaves = get_leaves tree h in
            let comp_leaves =
                List.fold_right Set.add comp_leaves Set.empty in
            (* complement *)
            let comp s = Set.diff comp_leaves s in
            let add p n set m =
                let m = Map.add (Edge (p, n)) set m in
                let m = Map.add (Edge (n, p)) (comp set) m in
                m in
            (* [r p n m] is parent, node, map *)
            let rec r p n m = match get_node n tree with
            | Leaf (id, _) ->
                  let set = Set.singleton id in
                  set, add p n set m
            | Single _ -> assert (false)
            | Interior (_, _, a, b) ->
                  let aset, m = r n a m in
                  let bset, m = r n b m in
                  let set = Set.union aset bset in
                  set, add p n set m in
                    match get_node h tree with
                    | Leaf (id, par) ->
                          let set = Set.singleton id in
                          let m = add par id set m in
                          let _, m = r id par m in
                          m
                    | Single _ -> m
                    | Interior (id, a, b, c) ->
                          let _, m = r id a m in
                          let _, m = r id b m in
                          let _, m = r id c m in
                          m in
                (Set.fold calc_component tree.handles Map.empty : t)

    module CladeSet = Set.Make (Ordered)

    let sets t =
        let module Set = All_sets.Integers in
        let module Map = EdgeMap in
        Map.fold (fun _ set acc -> CladeSet.add set acc) (calc t) 
        CladeSet.empty

    let query = (EdgeMap.find : edge -> t -> fp)

    let num_leaves (fp : fp) = All_sets.Integers.cardinal fp

    let fold (fn : edge -> fp -> 'a -> 'a) t a =
        EdgeMap.fold fn t a

end

(** [get_break_handles delta tree] returns the ids of the handles of the left
    and right components of a tree after a break *)
let get_break_handles (a, b) tree =
    let h1 = match a with
    | `Single (i, _) -> i
    | `Edge (_, l1, l2, m) -> begin match m with
      | None -> handle_of l1 tree
      | Some m -> m
      end in
    let h2 = match b with
    | `Single (i, _) -> i
    | `Edge (_, l1, l2, m) -> begin match m with
      | None -> handle_of l1 tree
      | Some m -> m
      end in
    h1, h2

let fix_handle_neighbor h n tree =
    match get_node h tree with
    | Interior (_, par, a, b) ->
            if par = n then tree
            else if n = a then 
                let tree = remove_node h tree in
                add_node (Interior (h, a, par, b)) tree
            else if n = b then
                let tree = remove_node h tree in
                add_node (Interior (h, b, a, par)) tree
            else failwith "Tree.fix_handle_neighbor"
    | Leaf _ -> tree
    | Single _ -> failwith "Tree.fix_handle_neighbor 2"



(** {2 Tree Fusing} *)

(** [edge_summary tree] takes the edges in [tree] and returns a map from all
    nodes to their parents and a map from all nodes to a list of their children
*)
let edge_summary tree =
    let parents = ref All_sets.IntegerMap.empty in
    let children = ref All_sets.IntegerMap.empty in
    EdgeSet.iter
        (fun (Edge(f, t)) ->
             assert (false = All_sets.IntegerMap.mem t !parents);
             parents := All_sets.IntegerMap.add t f !parents;
             children := match All_sets.IntegerMap.mem f !children with
             | false -> All_sets.IntegerMap.add f [t] !children
             | true -> All_sets.IntegerMap.add f
                   (t :: All_sets.IntegerMap.find f !children) !children
        ) tree.d_edges;
    (!parents, !children)

(** [update_clade_with_edges node tree] returns [tree] with all nodes under
    [node] updated to respect the edges in [tree] *)
let update_clade_with_edges node tree =
    let (parents, children) = edge_summary tree in
    let rec r node tree' = match get_node node tree with
    | Leaf _ -> tree'                   (* leaves stay leaves *)
    | Interior (_, par, a, b) ->
          let par' = All_sets.IntegerMap.find node parents in

          let a', b' = match All_sets.IntegerMap.find node children with
          | [ a; b ] -> a, b
          | _ -> failwith "Tree.update_clade_with_edges too many/few children"
          in
          
          let tree' = tree' --> remove_node node
              --> add_node (Interior (node, par', a', b')) in
          let tree' = tree' --> r a' --> r b' in
          tree'
    | Single _ -> tree' in
    r node tree

(** [destroy_under_node node tree] removes all the edges and nodes underneath
    [node] in [tree].  WARNING: this leaves the tree in an inconsistent state.
    Leaves are left as-is, and the node itself is untouched, both of which are
    inconsistent.  For some measure of usefulness, we set the node's parent to
    itself... *)
let rec destroy_under_node node tree =
    match get_node node tree with
    | Leaf _ -> tree                    (* leave leaves: we don't want their
                                           IDs to be recycled... *)
          (* TODO: set node's parent to itself *)
    | Single _ -> tree                  (* same with singles *)
    | Interior (_, par, a, b) ->
          (* remove edges *)
          let tree = tree --> remove_edge (Edge (node, a))
              --> remove_edge (Edge (node, b)) in
          (* recursively remove *)
          let tree = tree --> destroy_under_node a
              --> destroy_under_node b in
          (* remove self *)
          let tree = tree --> remove_node node in
          tree

(** [reconstruct_node (source, snode) (target, tparent)] reconstructs the
    topology in [source] tree into [target] tree.  The clade under node [snode]
    will be reconstructed in the target tree as the child of node [ *)
let rec reconstruct_node (source, snode) (target, tparent) =
    match get_node snode source with
    | Leaf (sid, _) ->
          let target = target --> remove_node sid
              --> add_edge (Edge (tparent, sid))
              --> add_node (Leaf (sid, tparent)) in
          if debug_fusing then print_endline ("leaf: " ^ string_of_int sid
                                              ^ " with parent "
                                              ^ string_of_int tparent);
          target, sid
    | Single _ -> failwith "Tree.reconstruct_node"
    | Interior (sid, spar, sa, sb) ->
          let tid, target = get_available target in
          if debug_fusing then print_endline ("interior: " ^ string_of_int tid
                                              ^ " with parent "
                                              ^ string_of_int tparent);
          let target, t1 =
              reconstruct_node (source, sa) (target, tid) in
          let target, t2 =
              reconstruct_node (source, sb) (target, tid) in
          let target = target --> add_node (Interior (tid, tparent, t1, t2))
              --> add_edge (Edge (tparent, tid)) in
          target, tid

(** [source_to_target (source, snode) (target, tnode)] fuses the clade in
    [source] rooted at [snode] into [target], beginning at node [tnode] *)
let source_to_target (source, snode) (target, tnode) =
    assert (not (is_handle tnode target));
    assert (not (is_leaf tnode target));

    (* make sure the set of leaves is the same *)
    if debug_fusing then begin
        let sleaves = get_leaves source snode in
        let tleaves = get_leaves target tnode in
        let sleaves = List.sort compare sleaves in
        let tleaves = List.sort compare tleaves in
        print_string "sleaves: ";
        print_endline (String.concat "; " (List.map string_of_int sleaves));
        print_string "tleaves: ";
        print_endline (String.concat "; " (List.map string_of_int tleaves));
        if sleaves <> tleaves
        then begin
            print_endline "sets of leaves disagree!\n";
            assert false
        end
    end;
    let tparent = get_parent tnode target in
    let target = target --> destroy_under_node tnode
        --> remove_edge (Edge (tparent, tnode)) in
    let (target, tnewid) =
        reconstruct_node (source, snode) (target, tparent) in
    assert (tnewid = tnode);            (* TODO: if the node has changed,
                                           update the parent to refer to the
                                           new node (...) *)
    if debug_fusing then test_tree target;
    target


module CladeSet = Set.Make (CladeFP.Ordered)
module CladeFPMap = Map.Make (CladeFP.Ordered)

let tree_fps_map ?(map=CladeFPMap.empty) tree =
    let fps = CladeFP.calc tree in
    let store edge fp map =
        try
            let list = CladeFPMap.find fp map in
            let map = CladeFPMap.remove fp map in
            CladeFPMap.add fp ((tree, edge) :: list) map
        with Not_found ->
            CladeFPMap.add fp [(tree, edge)] map in
    CladeFP.fold store fps map

let tree_fps_map_withaux ?(map=CladeFPMap.empty) (aux, tree) =
    let fps = CladeFP.calc tree in
    let store edge fp map =
        try
            let list = CladeFPMap.find fp map in
            let map = CladeFPMap.remove fp map in
            CladeFPMap.add fp ((aux, tree, edge) :: list) map
        with Not_found ->
            CladeFPMap.add fp [(aux, tree, edge)] map in
    CladeFP.fold store fps map

type 'a fuse_locations = ('a * u_tree * edge) list Sexpr.t

let fuse_locations ?(filter=fun _ -> true) sources (taux, target) =
    let target_fps = tree_fps_map target in
    let source_fps =
        let folder map tree = tree_fps_map_withaux ~map tree in
        List.fold_left folder CladeFPMap.empty sources in
    let locations = CladeFPMap.fold
        (fun key value sexpr ->
             try
                 let _, tedge = match value with
                 | [e] -> e
                 | _ -> failwith "Tree.fuse_locations.locations" in

                 let value = CladeFPMap.find key source_fps in
                 if filter (key, CladeFP.num_leaves key)
                 then (`Single ((taux, target, tedge)
                                :: value)) :: sexpr
                 else sexpr
             with Not_found -> sexpr) target_fps [] in
    match locations with
    | [] -> `Empty
    | l -> `Set l

let fuse_all_locations ?(filter=fun _ -> true) trees =
    let fps =
        let folder map tree = tree_fps_map_withaux ~map tree in
        List.fold_left folder CladeFPMap.empty trees in
    let atleast2 = function
        | a :: b :: _ -> true
        | _ -> false in

    let locations = CladeFPMap.fold
        (fun key value sexpr ->
             if atleast2 value && filter (key, CladeFP.num_leaves key)
             then (`Single value) :: sexpr
             else sexpr) fps [] in
    match locations with
    | [] -> `Empty
    | l -> `Set l

let fuse_cladesize ~min ~max (_, size) = min <= size && size <= max

let fuse ~source ~target =
    let (stree, sedge) = source in
    let (ttree, tedge) = target in
    let Edge(sf, st) = sedge in
    let Edge(tf, tt) = tedge in
    let stree, spath =
        if is_edge sedge stree
        then stree, []
        else let Edge(f, t) = sedge in move_handle f stree in
    let ttree, tpath =
        if is_edge tedge ttree
        then ttree, []
        else let Edge(f, t) = tedge in move_handle f ttree in
    source_to_target (stree, st) (ttree, tt)


let rec update_vertex tree depth vertex acc =
    let cur_depth = All_sets.TupleMap.find (vertex, -1) acc in
    if cur_depth > depth + 1 then
        match get_node vertex tree with
        | Interior (_, par, c1, c2) ->
                let my_depth = depth + 1 in
                let acc = All_sets.TupleMap.add (vertex, par) my_depth acc in
                let acc = update_vertex tree my_depth c1 acc in
                update_vertex tree my_depth c2 acc
        | _ -> acc
    else acc

let rec depth_of_vertex tree best_known vertex acc =
    match get_node vertex tree with
    | Interior (_, par, c1, c2) ->
            let dc1, acc = depth_of_vertex tree (best_known + 1) c1 acc in
            let dc2, acc = 
                let new_best_known = min (best_known + 1) (dc1 + 1) in
                depth_of_vertex tree new_best_known c2 acc 
            in
            let my_depth, acc = 
                if dc2 < dc1 && dc2 < best_known then
                    let my_depth = dc2 + 1 in
                    my_depth, update_vertex tree my_depth c1 acc
                else if dc1 < best_known then
                    dc1 + 1, acc
                else best_known + 1, acc
            in
            my_depth, All_sets.TupleMap.add (vertex, par) my_depth acc
    | Leaf (_, par) ->
            1, All_sets.TupleMap.add (vertex, par) 1 acc
    | Single _ -> 0, acc


let big_number = 10000000

let depth_of_handle tree handle acc =
    match get_node handle tree with
    | Interior (_, par, c1, c2) ->
            let dp, acc = depth_of_vertex tree big_number par acc in
            let dc1, acc = depth_of_vertex tree dp c1 acc in
            let dc2, acc = depth_of_vertex tree (min dp dc1) c2 acc in
            let mydepth, acc = 
                if dc2 < dp && dc2 < dc1 then 
                    let acc = update_vertex tree dc2 par acc in
                    dc2 + 1, update_vertex tree dc2 c1 acc
                else if dc1 < dp then
                    dc1 + 1, update_vertex tree dc1 par acc
                else dp, acc
            in
            All_sets.TupleMap.add (handle,par) mydepth acc
    | Leaf (_, par) ->
            let dp, acc = depth_of_vertex tree 1 par acc in
            All_sets.TupleMap.add (handle, par) dp acc
    | Single _ -> acc


let depths tree = 
    All_sets.Integers.fold (depth_of_handle tree) (get_handles tree)
    All_sets.TupleMap.empty

(* Functions to compare two trees weather or not they are the same *)
let reroot (a, b) tree =
    let bt, _ = move_handle a tree in
    fix_handle_neighbor a b bt

(* Find a leaf *)
let choose_leaf tree =
    let (Edge (a, _)) = EdgeSet.choose tree.d_edges in
    let rec find_leaf x =
        match get_node x tree with
        | Leaf (a, b)  ->  a, b
        | Interior (_, _, c, _) -> find_leaf c
        | Single _ -> failwith "Tree.choose_leaf"
    in
    find_leaf a

let cannonize_on_edge ((a, b) as edge) tree = 
    assert (is_leaf a tree);
    let tree = reroot edge tree in
    let rec my_cannonizer vertex res = 
        match get_node vertex tree with
        | (Interior (a, b, c, d)) as vx ->
                (* Recursively cannonize the children and then myself *)
                let res, minc = my_cannonizer c res in
                let res, mind = my_cannonizer d res in
                if minc < mind then
                    All_sets.IntegerMap.add a vx res, minc
                else 
                    let v = (Interior (a, b, d, c)) in
                    All_sets.IntegerMap.add a v res, mind
        | vx ->
                (* There is nothing to cannonize in leafs and singles *)
                All_sets.IntegerMap.add vertex vx res, vertex
    in
    let res, _ = my_cannonizer b All_sets.IntegerMap.empty in
    let res = All_sets.IntegerMap.add a (Leaf (a, b)) res in
    { tree with u_topo = res }

let cannonize_on_leaf a tree =
    match get_node a tree with
    | Leaf (a, b) -> cannonize_on_edge (a, b) tree
    | _ -> failwith "Tree.cannonize_on_leaf: the vertex is not a leaf"

let compare_cannonical a b = 
    let rec recursive_compare ac bc =
        match get_node ac a, get_node bc b with
        | Leaf _, Leaf _ -> true
        | Interior (_, _, a1, a2), Interior (_, _, b1, b2) ->
                (recursive_compare a1 b1) && (recursive_compare a2 b2)
        | _, _ -> false
    in
    let compare handle =
        match get_node handle a, get_node handle b with
        | Leaf (_, a'), Leaf (_, b') ->
                recursive_compare a' b'
        | Single _, Single _ -> true
        | Interior _, Interior _ -> failwith "Canonize first"
        | _, _ -> false
    in
    let handles = get_handles a in
    match All_sets.Integers.cardinal handles with
    | 1 -> compare (All_sets.Integers.choose handles)
    | _ -> failwith "Comparing trees can not handle forests"

let get_unique trees = 
    match trees with
    | (_, tree) :: _ ->
        let a, _ = choose_leaf tree in
        let trees = List.rev_map (fun (b, x) ->
            b, x, (cannonize_on_leaf a x)) trees 
        in
        let rec remove_duplicated acc = function
            | ((_, _, h) as pair) :: t ->
                    let are_different (_, _, x) = not (compare_cannonical h x) in
                    remove_duplicated (pair :: acc) (List.filter are_different t)
            | [] -> acc
        in
        List.rev_map (fun (a, b, _) -> a, b) (remove_duplicated []  trees)
    | x -> x
