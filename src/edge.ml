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

let () = SadmanOutput.register "Edge" "$Revision: 1616 $"

exception IllegalEdgeConversion

module type EdgeSig = sig
    (** The type of the edge data *)
    type e
    (** The type of a node *)
    type n
    (** Weather or not an edge holds data (false when e = unit), or the edge
    * type is not to be used *)
    val has_information : bool
    (* Calculate the median between the nodes *)
    (*
    val calculate : n -> n -> e
    *)

    (* Convert the contents of an edge into a node (if it is possible). If it is
    * not possible due to the nature of the edge information, raise
    * IllegalEdgeConversion *)
    val to_node : int -> (int * int) -> e -> n
    val of_node : int option -> n -> e
end

module Edge : EdgeSig with type e = unit with type n = Node.node_data = struct
    type e = unit
    type n = Node.node_data
    let has_information = false
    let calculate _ x = ()
    let of_node _ n = ()
    let to_node _ _ e = failwith "Impossible"
end

module SelfEdge : EdgeSig with type e = Node.node_data with type n = Node.node_data
= struct
    type e = Node.node_data
    type n = Node.node_data
    let has_information = false
    let calculate _ x = x
    let of_node _ n = n
    let to_node _ _ e = e
end


(*
module SuperRoot (Node : NodeSig.S) : 
    EdgeSig with type e = Node.n with type n = Node.n = struct

        type e = Node.n
        type n = Node.n
        let has_information = true
        let calculate = Node.median None
        let to_node x = x

    end

module LazyRoot (Node : NodeSig.S) :
    EdgeSig with type e = Node.n Lazy.t with type n = Node.n = struct

        type e = Node.n Lazy.t
        type n = Node.n
        let has_information = true
        let calculate a b = Lazy.lazy_from_fun (fun () -> Node.median None a b)
        let to_node x = Lazy.force x

    end
*)
module LazyEdge : EdgeSig with type e = AllDirNode.OneDirF.n with type n =
    AllDirNode.AllDirF.n = struct
        type e = AllDirNode.OneDirF.n
        type n = AllDirNode.AllDirF.n
        let has_information = true
        let to_node code dir e = 
            let res = [{ AllDirNode.lazy_node = e; dir = Some dir; code = code}]
            in
            { AllDirNode.unadjusted = res; adjusted = res }
        let of_node a b = 
            match a with
            | Some a -> (AllDirNode.not_with a b.AllDirNode.unadjusted).AllDirNode.lazy_node
            | None ->
                    match b.AllDirNode.unadjusted with
                    | [x] -> x.AllDirNode.lazy_node
                    | _ -> failwith "Edge.LazyEdge.of_node"
    end

module HybridEdge : EdgeSig with type e = AllDirNode.OneHybrid.n with type n =
    AllDirNode.Hybrid.n = struct
        type e = AllDirNode.OneHybrid.n
        type n = AllDirNode.Hybrid.n
        let has_information = true

        let to_node code dir e = 
            let dy = 
                let tmp =
                    [{ AllDirNode.lazy_node = e.AllDirNode.dy;
                    dir = Some dir; code = code}]
                in
                { AllDirNode.unadjusted = tmp; adjusted = tmp }
            in
            { AllDirNode.st = e.AllDirNode.st; dy = dy }

        let of_node a b = 
            match a with
            | Some a -> 
                    { AllDirNode.st = b.AllDirNode.st; 
                    dy = (AllDirNode.not_with a
                    b.AllDirNode.dy.AllDirNode.unadjusted).AllDirNode.lazy_node }
            | None ->
                    match b.AllDirNode.dy.AllDirNode.unadjusted with
                    | [x] -> 
                            { AllDirNode.st = b.AllDirNode.st;
                            AllDirNode.dy = x.AllDirNode.lazy_node}
                    | _ -> failwith "Edge.LazyEdge.of_node"
    end
