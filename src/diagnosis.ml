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

let () = SadmanOutput.register "Diagnosis" "$Revision: 2157 $"

let debug = true

let sort_using_tree tree all_taxa =
    let set = 
        List.fold_left 
        (fun acc ((x, y) as z) -> All_sets.IntegerMap.add x z acc) 
        All_sets.IntegerMap.empty all_taxa
    in
    let res = 
        All_sets.Integers.fold (fun handle acc ->
            Tree.post_order_node_visit 
            (fun _ x acc -> 
                Tree.Continue,
                if All_sets.IntegerMap.mem x set then 
                    (All_sets.IntegerMap.find x set) :: acc
                else acc)
            handle tree acc)
        (Tree.get_handles tree) []
    in
    List.rev res

let output_implied_alignment (tree, seqname) filename data to_process = 
    Status.user_message (Status.Output (filename, false, [])) 
    ("@[<v>@,@[New Tree@ for@ sequence@ " ^ seqname ^ "@]@,@[");

    (* This function expects only one element *)
    match to_process with
    | [all_taxa, _] ->
            let all_taxa = sort_using_tree tree all_taxa in
            let process_each acc (taxcode, sequence) =
                let name = 
                    try Data.code_taxon taxcode data with
                    | Not_found -> (string_of_int taxcode)
                in
                match sequence with
                | hd_sequence :: tl ->
                        let res = 
                            All_sets.IntegerMap.fold 
                            (fun c s acc -> Some (c, s))
                            hd_sequence None
                        in
                        (match res with 
                         | Some (seqcode, sequence_arr) ->
                               let sequence = sequence_arr.(0) in 
                               let alphabet = 
                                   Data.get_sequence_alphabet seqcode data
                               in
                               let gapcode = Alphabet.get_gap alphabet in
                               let gap = Alphabet.match_code gapcode alphabet
                                in
                                 (* Check if the sequence is missing data *)
                                let preprocess_sequence x =
                                    let len = Array.length x in
                                    let rec check it =
                                        if it = len then true
                                        else if x.(it) = 0 then check (it + 1)
                                        else false
                                    in
                                    if check 0 then
                                        for i = len - 1 downto 0 do
                                            x.(i) <- 0;
                                        done
                                    else ()
                                in
                                preprocess_sequence sequence;
                                let rec folder (result, pos, cnt) base =
                                    if cnt = 0 then (result, pos, 1) 
                                    else if 80 = pos then 
                                        folder (result ^ "@,", 0, cnt) base
                                    else if base = 0 then 
                                        result ^ gap, pos + 1, cnt
                                    else 
                                        let item = 
                                            Alphabet.match_code base alphabet 
                                        in
                                        (result ^ item, pos + 1, cnt) 
                                in
                                let result, _, _ = 
                                    Array.fold_left folder ("", 0, 0) sequence 
                                in
                                let fo = Status.Output (filename, false, []) in
                                Status.user_message fo ("@,@[<v>@,>" ^ name ^
                                "@,");
                                Status.user_message fo result;
                                Status.user_message fo "@]%!";

                                if Array.length sequence_arr = 1 then (taxcode, tl) :: acc
                                else begin
                                    let hd_sequence = All_sets.IntegerMap.map
                                        (fun sequence_arr -> Array.of_list
                                             (List.tl (Array.to_list sequence_arr)))
                                        hd_sequence
                                    in 
                                    (taxcode, hd_sequence::tl)::acc
                                end 


                        | None -> (taxcode, tl) :: acc)
                | [] -> acc
            in
            let rec process_all_sequences = function
                | (_, []) :: _ -> ()
                | x -> 
                        let acc = List.fold_left process_each [] x in
                        let acc = List.rev acc in
                        process_all_sequences acc
            in
            process_all_sequences all_taxa;
            Status.user_message (Status.Output (filename, false, [])) "@]@]";
    | _ -> failwith "Diagnosis.output_implied_alignment 1"

module type S = sig
    type a 
    type b

    val diagnosis :
      Data.d -> (a, b) Ptree.p_tree Sexpr.t -> Methods.diagnosis -> unit
end

module Make 
    (Node : NodeSig.S) (Edge : Edge.EdgeSig with type n = Node.n) 
    (TreeOps : 
        Ptree.Tree_Operations 
                        with type a = Node.n with type b = Edge.e) = struct

    type a = Node.n
    type b = Edge.e

    module IA = ImpliedAlignment.Make (Node) (Edge)
    module CT = CharTransform.Make (Node) (Edge) (TreeOps)
    module TO = TreeOps 

    let report_all_roots fo tree =
        let report_root ((Tree.Edge (a, b)), cost) =
            fo ("@[" ^ string_of_int a ^ "-" ^ string_of_int b ^ ": " ^
            string_of_float cost ^ "@]@\n");
        in
        fo "@[<v 2>@{<u>Tree@}@,@[<v>";
        List.iter report_root (TO.root_costs tree);
        fo "@]";
        fo "@]@,"

    let rec diagnosis (data : Data.d) (trees : (a, b) Ptree.p_tree Sexpr.t) = 
        function
        | `AllRootsCost filename ->
                let fo = 
                    Status.user_message (Status.Output (filename, false, [])) 
                in
                fo "@[<v>@,@,@{<b>All Roots Cost@}@,";
                Sexpr.leaf_iter (report_all_roots fo) trees;
                fo "@]@\n%!"
        | `Implied_Alignment (filename, chars) ->
                let char_codes = 
                    Data.get_code_from_characters_restricted_comp
                    `AllDynamic data chars 
                in
                let res = 
                    List.map (fun code -> Sexpr.map (fun x -> 
                        let seqname = Data.code_character code data in
                        let ia = IA.create CT.filter_characters [code] data x in
                        (x.Ptree.tree, seqname) , ia) trees) char_codes  
                in
                List.iter (fun res ->
                    Status.user_message 
                    (Status.Output (filename, false, [])) "@[<v>@,@,@{<b>Implied \
                    Alignments@}@,";
                    Sexpr.leaf_iter 
                    (fun (x, y) -> List.iter (output_implied_alignment x filename
                    data) y) res;
                    Status.user_message 
                    (Status.Output (filename, false, [])) "@]%!")
                res


end
