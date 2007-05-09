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

let () = SadmanOutput.register "AllDirNode" "$Revision: 1616 $"

type exclude = Node.exclude

type direction = (int * int) option

type a_node = Node.node_data Lazy.t

type node_dir = {
    lazy_node : a_node;
    dir : direction;
    code : int;
}

type node_data = {
    unadjusted : node_dir list; (** The standard downpass node *)
    adjusted : node_dir list;   (** An adjuted node value calculated after the
    downpass *)
}

let my_own_code () = incr Data.median_code_count; !Data.median_code_count

let to_n node = Lazy.lazy_from_val node

let has_code code n =
    match n.dir with
    | None -> true
    | Some (a, b) ->
            a <> code && b <> code

let get_code n =
    match n with
    | x :: _ ->
            let code = x.code in
            assert (List.for_all (fun x -> x.code = code) n);
            x.code
    | [] -> failwith "AllDirNode.get_code"

let print_pairs x = 
    let msg = 
        match x.dir with
        | None -> "No direction!"
        | Some (a, b) ->
                string_of_int a ^ ", " ^ string_of_int b
    in
    Status.user_message Status.Error msg

let yes_with code n =
    assert (0 <> List.length n);
    match List.filter (fun x -> not (has_code code x)) n with
    | [x; y] -> x, y
    | x -> 
            List.iter print_pairs x;
            failwith 
            ("AllDirNode.not_with " ^ string_of_int code ^ 
            " of " ^ string_of_int (get_code x) ^ " has " ^
            string_of_int (List.length x) ^ " matching values.")

let not_with code n =
    assert (0 <> List.length n);
    match List.filter (has_code code) n with
    | [x] -> x
    | x -> 
            List.iter print_pairs x;
            failwith 
            ("AllDirNode.not_with " ^ string_of_int code ^ 
            " of " ^ string_of_int (get_code x) ^ " has " ^
            string_of_int (List.length x) ^ " matching values.")

module OneDirF : 
    NodeSig.S with type e = exclude with type n = a_node with type
    nad8 = Node.Standard.nad8 = struct

    type n = a_node
    type e = exclude

    let load_data ?taxa ?codes ?(classify=true) data = 
        let data, nodes = 
            match taxa, codes with
            | None, None -> Node.Standard.load_data ~classify data 
            | Some v, None -> Node.Standard.load_data ~taxa:v ~classify data
            | None, Some v -> Node.Standard.load_data ~codes:v ~classify data
            | Some v, Some w -> 
                    Node.Standard.load_data ~taxa:v ~codes:w ~classify data
        in 
        data, List.map to_n nodes

    let fix_preliminary x = x

    let apply_f_on_lazy f a b = 
        let a = Lazy.force_val a
        and b = Lazy.force_val b in
        f a b

    let distance ?(para=None) ?(parb=None) a b =
        apply_f_on_lazy Node.Standard.distance a b

    let median code my_code old a b = 
        Lazy.lazy_from_fun 
        (fun () -> apply_f_on_lazy 
        (Node.Standard.median None my_code None) a b)

    let to_string v = Node.Standard.to_string (Lazy.force_val v)

    let apply_single_f_on_lazy f a = 
        f (Lazy.force_val a)

    let total_cost x n =
        apply_single_f_on_lazy (Node.Standard.total_cost x) n

    let node_cost par n = 
        apply_single_f_on_lazy (Node.Standard.node_cost par) n

    let update_leaf x = x

    let taxon_code n = 
        apply_single_f_on_lazy Node.Standard.taxon_code n

    let union_distance _ _ = 0.0

    let is_collapsable a b = 
        apply_f_on_lazy Node.Standard.is_collapsable a b

    let to_xml _ _ _ = ()

    let num_height c n = 
        apply_single_f_on_lazy (Node.Standard.num_height c) n

    let num_otus c n =
        apply_single_f_on_lazy (Node.Standard.num_otus c) n

    let get_sequences c = 
        apply_single_f_on_lazy Node.Standard.get_sequences c

    let get_dynamic_preliminary code n = 
        apply_single_f_on_lazy (Node.Standard.get_dynamic_preliminary code) n

    let edge_distance a b = 
        apply_f_on_lazy Node.Standard.edge_distance a b

    let support_chars starting code n =
        apply_single_f_on_lazy (Node.Standard.support_chars starting code) n

    let n_chars ?(acc=0) n =
        apply_single_f_on_lazy (Node.Standard.n_chars ~acc) n

    let prioritize x = x

    let reprioritize _ x = x

    let f_codes ints n = 
        Lazy.lazy_from_val (apply_single_f_on_lazy (Node.Standard.f_codes ints) n)

    let min_child_code code n = 
        apply_single_f_on_lazy (Node.Standard.min_child_code code) n

    type nad8 = Node.Standard.nad8

    let new_characters = Node.Standard.new_characters

    let build_node static_characters chars n = 
        Lazy.lazy_from_val 
        (apply_single_f_on_lazy 
        (Node.Standard.build_node static_characters chars) 
        n)

    let set_exclude_info e n = 
        Lazy.lazy_from_val 
        (apply_single_f_on_lazy (Node.Standard.set_exclude_info e) n)

    let excludes_median code a b = 
        apply_f_on_lazy (Node.Standard.excludes_median code) a b

    let has_excluded = Node.Standard.has_excluded

    module T = struct

        let add_exclude set = 
            fun x ->
                Lazy.lazy_from_val (Node.Standard.T.add_exclude set 
                (Lazy.force_val x))
    end

    module Union = struct
        type u = Node.Standard.Union.u Lazy.t

        let union code a b c =
            let a = Lazy.force_val a 
            and b = Lazy.force_val b 
            and c = Lazy.force_val c in
            Lazy.lazy_from_val (Node.Standard.Union.union code a b c)

        let union_one which code a b =
            let a = Lazy.force_val a
            and b = Lazy.force_val b in
            Lazy.lazy_from_val (which code a b)

        let union_preliminary a b c =
            union_one Node.Standard.Union.union_preliminary a b c

        let union_final a b c =
            union_one Node.Standard.Union.union_final a b c

        let leaf taxon_code code a = 
            Lazy.lazy_from_val 
                (Node.Standard.Union.leaf taxon_code code (Lazy.force_val a))

        let distance a b = 
            Node.Standard.Union.distance 
            (Lazy.force_val a)
            (Lazy.force_val b)

        let saturation a = 
            Node.Standard.Union.saturation (Lazy.force_val a)

        let distance_node code n u =
            Node.Standard.Union.distance_node code
            (Lazy.force_val n)
            (Lazy.force_val u)

        let compare a b =
            Node.Standard.Union.compare (Lazy.force_val a) (Lazy.force_val b)

        let get_sequence a b c = 
            Node.Standard.Union.get_sequence a b (Lazy.force_val c)
    end

    let for_support a b c d : n list =
        let nb = 
            List.map (fun (a, (b : n)) ->
                a, Lazy.force_val b) b
        in
        let res = Node.Standard.for_support a nb c d in
        List.map Lazy.lazy_from_val res


    let root_cost a = 
        Node.Standard.root_cost (Lazy.force_val a)
end

module AllDirF : NodeSig.S with type e = exclude with type n = node_data with
type nad8 = Node.Standard.nad8 = struct

    type n = node_data
    type e = exclude 

    let to_n node = 
        let node_dir = {
            lazy_node = to_n node;
            dir = None;
            code = node.Node.taxon_code;
        }
        in
        { unadjusted = [node_dir]; adjusted = [node_dir]}

    let load_data ?taxa ?codes ?(classify=true) data = 
        let data, nodes = 
            match taxa, codes with
            | None, None -> Node.Standard.load_data ~classify data 
            | Some v, None -> Node.Standard.load_data ~taxa:v ~classify data
            | None, Some v -> Node.Standard.load_data ~codes:v ~classify data
            | Some v, Some w -> 
                    Node.Standard.load_data ~taxa:v ~codes:w ~classify data
        in 
        data, List.map to_n nodes

    let fix_preliminary x = x

    let distance ?(para=None) ?(parb=None) a b =
        match a.unadjusted, b.unadjusted with
        | [a], [b] -> OneDirF.distance a.lazy_node b.lazy_node
        | a, [b] -> 
                (match parb with
                | None -> failwith "AlldNode.distance 1"
                | Some para -> 
                        let a = not_with para a in
                        OneDirF.distance a.lazy_node b.lazy_node)
        | [a], b -> 
                (match para with
                | None -> failwith "AlldNode.distance 2"
                | Some parb -> 
                        let b = not_with parb b in
                        OneDirF.distance a.lazy_node b.lazy_node)
        | a, b -> 
                (match para, parb with
                | Some para, Some parb -> 
                        let a = not_with para a 
                        and b = not_with parb b in
                        OneDirF.distance a.lazy_node b.lazy_node
                | _ -> failwith "AlldNode.distance 3")

    let median code my_code old a b =
        let my_code =
            match my_code with
            | Some code -> code
            | None -> my_own_code ()
        in
        let na, nb = 
            match code with
            | Some code ->
                    not_with code a.unadjusted, not_with code b.unadjusted
            | None ->
                    match a.unadjusted, b.unadjusted with
                    | [a], [b] -> a, b
                    | _, _ -> failwith "AlldNode.median"
        in
        let lazy_node = 
            OneDirF.median None (Some my_code) None na.lazy_node nb.lazy_node
        in
        let node = {
            lazy_node = lazy_node;
            dir = Some (na.code, nb.code); 
            code = my_code;
        }
        in
        { unadjusted = [node]; adjusted = [node] }

    let to_string _ = ""

    let total_cost par n =
        match par with
        | Some code ->
                OneDirF.total_cost par (not_with code n.unadjusted).lazy_node
        | None ->
                match n.unadjusted with
                | [x] -> OneDirF.total_cost par x.lazy_node
                | [] -> failwith "The emtpy median? AllDirNode.total_cost"
                | _ -> failwith "AllDirNode.total_cost"

    let node_cost par n =
        match par with
        | Some code ->
                OneDirF.node_cost par (not_with code n.unadjusted).lazy_node
        | None ->
                match n.unadjusted with
                | [x] -> OneDirF.node_cost par x.lazy_node
                | [] -> failwith "The emtpy median? AllDirNode.node_cost"
                | _ -> failwith "AllDirNode.node_cost"

    let update_leaf x = x

    let taxon_code n = 
        match n.unadjusted with
        | h :: _ -> (** All the elements in the list have the same code *)
                h.code
        | [] -> failwith "AllDirNode.taxon_code"

    let union_distance _ _ = 0.0

    let is_collapsable a b =
        let acode = taxon_code a
        and bcode = taxon_code b in
        let da = not_with bcode a.unadjusted
        and db = not_with acode b.unadjusted in
        OneDirF.is_collapsable da.lazy_node db.lazy_node

    let to_xml _ _ _ = ()

    let run_any f n =
        match n with
        | h :: _ ->
                Lazy.lazy_from_val (f h.lazy_node)
        | [] -> failwith "AllDirNode.run_any"

    let run_all f n =
        let processor x = 
            let res = f x.lazy_node in
            { x with lazy_node = res }
        in
        { unadjusted = List.map processor n.unadjusted;
        adjusted = List.map processor n.adjusted }

    let get_something f code n =
        let node = 
            match code with
            | Some code ->
                    not_with code n
            | None ->
                    match n with
                    | [x] -> x
                    | [] -> failwith "AllDirNode.num_height 1"
                    | _ -> failwith "AllDirNode.num_height 2"
        in
        f code node.lazy_node

    let num_height code n = 
        get_something OneDirF.num_height code n.unadjusted

    let num_otus code n =
        get_something OneDirF.num_otus code n.unadjusted

    let get_sequences _ = []

    let get_dynamic_preliminary code n =
        get_something OneDirF.get_dynamic_preliminary code n.unadjusted

    let edge_distance a b =
        let acode = taxon_code a
        and bcode = taxon_code b in
        let da = not_with acode b.unadjusted
        and db = not_with bcode a.unadjusted in
        OneDirF.edge_distance da.lazy_node db.lazy_node

    let support_chars starting code n =
        get_something (OneDirF.support_chars starting) code n.unadjusted

    let n_chars ?(acc=0) n =
        Lazy.force_val (run_any (OneDirF.n_chars ~acc) n.unadjusted)

    let prioritize x = x

    let reprioritize _ x = x

    let get_others err code n =
            match code with
            | Some code -> Some (yes_with code n)
            | None -> None

    let get_node err code n =
            match code with
            | Some code -> not_with code n
            | None ->
                    match n with 
                    | [x] -> x
                    | _ -> failwith err

    let f_codes ints n = 
        run_all (OneDirF.f_codes ints) n

    let min_child_code code n = 
        get_something OneDirF.min_child_code code n.unadjusted

    type nad8 = OneDirF.nad8

    let new_characters = Node.Standard.new_characters

    let build_node static_characters chars node =
        let processor x = 
            let res = OneDirF.build_node static_characters chars x.lazy_node in
            { x with lazy_node = res }
        in
        let uadj = List.map processor node.unadjusted
        and adj = List.map processor node.adjusted in
        { unadjusted = uadj; adjusted = adj; }

    let set_exclude_info e x =
        let processor x = 
            let res = OneDirF.set_exclude_info e x.lazy_node in
            { x with lazy_node = res }
        in
        let uadj = List.map processor x.unadjusted
        and adj = List.map processor x.adjusted in
        { unadjusted = uadj; adjusted = adj }

    let get_nodes code a b =
        match code with
        | Some code ->
                not_with code a.unadjusted, not_with code b.unadjusted 
        | None ->
                match a.unadjusted, b.unadjusted with
                | [a], [b] -> a, b
                | _, _ -> failwith "AllDirNode.excludes_median"

    let excludes_median code a b = 
        let nodea, nodeb = get_nodes code a b in
        OneDirF.excludes_median code nodea.lazy_node nodeb.lazy_node

    let has_excluded = Node.Standard.has_excluded

    module T = struct

        let add_exclude set n =
            let processor x = 
                { x with lazy_node = OneDirF.T.add_exclude set x.lazy_node}
            in
            let uadj = List.map processor n.unadjusted
            and adj = List.map processor n.adjusted in
            { unadjusted = uadj; adjusted = adj }
    end

    module Union = struct

        type u = OneDirF.Union.u
        let union code n a b =
            let node = get_node "AllDirNode.AllDirF.Union.union" code
            n.unadjusted in
            OneDirF.Union.union code node.lazy_node a b

        let union_final code a b = 
            let b = get_node "AllDirNode.AllDirF.Union.union_final" code
            b.unadjusted in
            OneDirF.Union.union_final code a b.lazy_node

        let union_preliminary code a b = 
            let b = 
                get_node "AllDirNode.AllDirF.Union.union_preliminary" code
                b.unadjusted
            in
            OneDirF.Union.union_preliminary code a b.lazy_node

        let leaf taxon_code code n =
            let x = get_node "AllDirNode.AllDirF.Union.leaf" code n.unadjusted in
            OneDirF.Union.leaf taxon_code code x.lazy_node

        let distance a b = OneDirF.Union.distance a b

        let saturation = OneDirF.Union.saturation

        let distance_node code n u =
            let node = 
                get_node "AllDirNode.AllDirF.Union.distance_node" code
                n.unadjusted
            in
            OneDirF.Union.distance_node code node.lazy_node u

        let compare = OneDirF.Union.compare

        let get_sequence = OneDirF.Union.get_sequence

    end

    let for_support a b c d : n list=
        let nb = 
            List.map (fun (a, (b : n)) ->
                match b.unadjusted with
                | [b] -> a, Lazy.force_val b.lazy_node
                | _ -> failwith "AllDirNode.for_support") b 
        in
        let res = Node.Standard.for_support a nb c d in
        let rec merge a b =
            match a, b with
            | (_, [ha]) :: ta, hb :: tb ->
                    [{ ha with lazy_node = Lazy.lazy_from_val hb }] ::
                        merge ta tb
            | [], [] -> []
            | _, _ -> failwith "AllDirNode.for_support 2"
        in
        let b = List.map (fun (a, x) -> a, x.unadjusted) b in
        let res = merge b res in
        List.map (fun x -> { unadjusted = x; adjusted = x }) res

    let root_cost a = 
        match a.unadjusted with
        | [a] -> OneDirF.root_cost a.lazy_node
        | _ -> failwith "AllDirNode.root_cost"
end

type 'a node_hybrid = {
    st : Node.Standard.n option;
    dy : 'a;
}

module HybridF = struct
    let get_dynamic x = x.dy
end

module MakeHybrid (LazyNode : NodeSig.S with type e = exclude) : NodeSig.S with
type e = exclude with type n = LazyNode.n node_hybrid =
    struct

        type n = LazyNode.n node_hybrid

        let apply_each_1 static dynamic n =
            match n.st with
            | None ->
                    { st = None; dy = dynamic n.dy }
            | Some st ->
                    { st = Some (static st); dy = dynamic n.dy }

        let apply_each_2 static dynamic n n' =
            match n.st, n'.st with
            | None, None ->
                    { st = None; dy = dynamic n.dy n'.dy }
            | Some st, Some st' ->
                    { st = Some (static st st'); dy = dynamic n.dy n'.dy }
            | _ -> failwith "AllDirNode.MakeHybrid.apply_each_2"

        let apply_two static dynamic cmp default n n' =
            match n.st, n'.st with
            | None, None ->
                    cmp default (dynamic n.dy n'.dy)
            | Some st, Some st' ->
                    cmp (static st st') (dynamic n.dy n'.dy)
            | _ -> failwith "AllDirNode.apply_two"

        let apply_additive_2 static dynamic n n' =
            match n.st, n'.st with
            | None, None ->
                    (dynamic n.dy n'.dy) 
            | Some st, Some st' ->
                    (dynamic n.dy n'.dy) +. (static st st')
            | _ -> failwith "AllDirNode.apply_additive_2"

        let apply_additive static dynamic n =
            match n.st with
            | None -> dynamic n.dy
            | Some st -> (dynamic n.dy) +. (static st)

        let apply_additive_int static dynamic n =
            match n.st with
            | None -> (dynamic n.dy) 
            | Some st -> (dynamic n.dy) + (static st)

        let fix_preliminary (n : n) =
            match n.st with
            | Some st ->
                    { dy = LazyNode.fix_preliminary n.dy;
                    st = Some (Node.Standard.fix_preliminary st)}
            | None ->
                    { dy = LazyNode.fix_preliminary n.dy;
                    st = None }

        let distance ?(para=None) ?(parb=None) a b =
            apply_additive_2 
            (Node.Standard.distance ~para ~parb)
            (LazyNode.distance ~para ~parb)
            a b

        let median a b c d e =
            let dyc, stc = 
                match c with
                | None -> None, None
                | Some c -> 
                        Some c.dy, 
                        match c.st with 
                        | None -> None 
                        | Some v -> Some v
            in
            match d.st, e.st with
            | None, None ->
                        { dy = LazyNode.median a b dyc d.dy e.dy;
                        st = None }
            | Some dst, Some est ->
                        { dy = LazyNode.median a b dyc d.dy e.dy;
                        st = Some (Node.Standard.median a b stc dst est) }
            | _, _ -> failwith "AllDirNode.MakeHybrid.median"

        let to_string n =
            LazyNode.to_string n.dy ^ " " ^ 
            match n.st with
            | None -> ""
            | Some st -> Node.Standard.to_string st

        let total_cost a b =
            apply_additive 
            (Node.Standard.total_cost a) 
            (LazyNode.total_cost a) 
            b

        let node_cost a b = 
            apply_additive
            (Node.Standard.node_cost a)
            (LazyNode.node_cost a)
            b

        let update_leaf n =
            apply_each_1
            Node.Standard.update_leaf 
            LazyNode.update_leaf
            n

        let apply_any dynamic n = dynamic n.dy

        let taxon_code n =
            apply_any 
            LazyNode.taxon_code
            n

        let union_distance a b = 
            apply_additive_2
            Node.Standard.union_distance
            LazyNode.union_distance
            a
            b

        let is_collapsable a b =
            apply_two 
            Node.Standard.is_collapsable
            LazyNode.is_collapsable
            (fun a b -> a & b) 
            true
            a 
            b

        let to_xml _ _ _ = ()

        let num_height a b =
            apply_any 
            (LazyNode.num_height a)
            b

        let num_otus a b =
            apply_any 
            (LazyNode.num_otus a)
            b

        let dynamic_only dynamic n = dynamic n.dy

        let static_only default static n = 
            match n.st with
            | None -> default
            | Some v -> static v

        let get_sequences n =
            dynamic_only LazyNode.get_sequences n

        let get_dynamic_preliminary a b =
            dynamic_only (LazyNode.get_dynamic_preliminary a) b

        let edge_distance a b =
            apply_additive_2 
            Node.Standard.edge_distance 
            LazyNode.edge_distance
            a 
            b

        let support_chars a b n =
            static_only [] (Node.Standard.support_chars a b) n

        let load_data ?taxa ?codes ?(classify=true) data = 
            let static_data = 
                Data.process_ignore_characters false data `AllDynamic
            and dynami_data =
                Data.process_ignore_characters false data `AllStatic
            and make_some = List.map (fun x -> Some x) in
            let res1, res2 = 
                match taxa, codes with
                | Some x, Some y ->
                        let _, a =
                            LazyNode.load_data ~taxa:x ~codes:y ~classify 
                            dynami_data
                        in
                        let _, b = 
                            Node.Standard.load_data ~taxa:x ~codes:y ~classify
                            static_data
                        in
                        a, make_some b
                | Some x, None ->
                        let _, a = 
                            LazyNode.load_data ~taxa:x ~classify dynami_data
                        in
                        let _, b = 
                            Node.Standard.load_data ~taxa:x ~classify 
                            static_data 
                        in
                        a, make_some b
                | None, Some y ->
                        let _, a = 
                            LazyNode.load_data ~codes:y ~classify dynami_data 
                        in
                        let _, b = 
                            Node.Standard.load_data ~codes:y ~classify 
                            static_data 
                        in
                        a, make_some b
                | None, None ->
                        let _, a = LazyNode.load_data ~classify dynami_data in
                        let _, b = 
                            Node.Standard.load_data ~classify static_data 
                        in
                        a, make_some b
            in
            let res = List.combine res1 res2 in
            data, List.map (fun (x, y) ->  {dy = x; st = y}) res

        let n_chars ?(acc = 0) n =
            apply_additive_int
            (Node.Standard.n_chars ~acc)
            (LazyNode.n_chars ~acc:0)
            n

        let prioritize n =
            apply_each_1 
            Node.Standard.prioritize
            LazyNode.prioritize
            n

        let reprioritize a b =
            apply_each_2
            Node.Standard.reprioritize
            LazyNode.reprioritize
            a
            b

        let f_codes lst n =
            apply_each_1 
            (Node.Standard.f_codes lst)
            (LazyNode.f_codes lst)
            n

        let min_child_code x n =
            apply_any
            (LazyNode.min_child_code x)
            n

        type e = exclude

        type nad8 = Node.Standard.nad8

        let new_characters = Node.Standard.new_characters

        let build_node a b c =
            match c.st with
            | None -> c
            | Some st ->
                    { c with st = Some (Node.Standard.build_node a b st) }

        let set_exclude_info a b = 
            match b.st with
            | None -> b
            | Some st ->
                    { b with st = Some (Node.Standard.set_exclude_info a st) }

        let excludes_median a b c =
            match b.st, c.st with
            | None, None -> []
            | Some b, Some c ->
                    Node.Standard.excludes_median a b c
            | _ -> failwith "AllDirNode.excludes_median"

        let has_excluded = Node.Standard.has_excluded

        module T = struct

            let add_exclude a n =
                match n.st with
                | Some st ->
                        { n with st = Some (Node.Standard.T.add_exclude a st) }
                | None -> n

        end

        module Union = struct

            type u = {
                stu : Node.Standard.Union.u option;
                dyu : LazyNode.Union.u;
            }

            let union a n x y =
                match n.st, x.stu, y.stu with
                | None, None, None ->
                        { dyu = 
                            LazyNode.Union.union a n.dy x.dyu y.dyu;
                            stu = None }
                | Some nst, Some xstu, Some ystu ->
                    { dyu = 
                        LazyNode.Union.union a n.dy x.dyu y.dyu;
                    stu = 
                        Some (Node.Standard.Union.union a nst xstu ystu) }
                | _ -> failwith "AllDirNode.MakeHybrid.Union.union"

            let unprepost static dynamic a b c =
                match b.stu, c.st with
                | None, None ->
                        { dyu = dynamic a b.dyu c.dy;
                        stu = None }
                | Some bstu, Some cst ->
                        { dyu = dynamic a b.dyu c.dy;
                        stu = Some (static a bstu cst) }
                | _ -> failwith "AllDirNode.MakeHybrid.Union.unprepost"
                        
            let union_preliminary a b c = 
                unprepost 
                Node.Standard.Union.union_preliminary
                LazyNode.Union.union_preliminary
                a 
                b 
                c

            let union_final a b c = 
                unprepost 
                Node.Standard.Union.union_final
                LazyNode.Union.union_final
                a 
                b 
                c

            let leaf a b n =
                match n.st with
                | Some nst ->
                        { stu = Some (Node.Standard.Union.leaf a b nst);
                        dyu = LazyNode.Union.leaf a b n.dy }
                | None ->
                        { stu = None;
                        dyu = LazyNode.Union.leaf a b n.dy }

            let distance a b =
                (LazyNode.Union.distance a.dyu b.dyu)
                +.
                match a.stu, b.stu with
                | Some astu, Some bstu ->
                        (Node.Standard.Union.distance astu bstu) 
                | None, None -> 0.0
                | _ -> failwith "AllDirNode.MakeHybrid.Union.distance"

            let saturation u = 
                failwith "Not implemented!  LazyNode.Hybrid.Union.saturation"

            let distance_node a b c =
                (LazyNode.Union.distance_node a b.dy c.dyu ) 
                +.
                match b.st, c.stu with
                | Some bst, Some cstu ->
                        (Node.Standard.Union.distance_node a bst cstu)
                | None, None -> 0.0
                | _ -> failwith "AllDirNode.MakeHybrid.Union.distance"

            let compare a b = 
                let initial =
                    match a.stu, b.stu with
                    | None, None -> 0
                    | Some a, Some b -> Node.Standard.Union.compare a b 
                    | _ -> failwith "How am I going to compare them?"
                in
                if initial = 0 then LazyNode.Union.compare a.dyu b.dyu
                else initial

            let get_sequence a b c =
                LazyNode.Union.get_sequence a b c.dyu

        end

        let for_support a b c d = failwith "MOVE ANDRES!"

        let root_cost n =
            apply_additive 
            Node.Standard.root_cost 
            LazyNode.root_cost
            n
    end

module Hybrid = MakeHybrid (AllDirF)
module OneHybrid = MakeHybrid (OneDirF)
