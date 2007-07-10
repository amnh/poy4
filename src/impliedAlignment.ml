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

let () = SadmanOutput.register "ImpliedAlignment" "$Revision: 1952 $"

exception NotASequence of int

let debug = false

module Codes = All_sets.IntegerMap
module Handles = All_sets.Integers
module IntSet = All_sets.Integers


type dyna_state_t = Data.dyna_state_t


type ias = {
    seq : Sequence.s;
    codes : (int, int) Hashtbl.t; (* (key=pos -> code) Hashtble *)
    homologous: (int, int Sexpr.t) Hashtbl.t; (* (code, hom_code list) Hashtbl *)
    cannonic_code : int;
    order : int list; (* codes list in reverse order *)
}


(* t is the presentation of a dynamic set (DynamicCS) in order to create implied
   alignments.
   Note: sequences : ias list Codes.t where (ias list) is a list of equally
   optimal medians in case of chromosomes
   
*)
type t = {
    sequences : ias list Codes.t;
    c2 : Cost_matrix.Two_D.m;
    chrom_pam : Data.dyna_pam_t;
    state : dyna_state_t;
    code : int;
}

type pairs = int * t list (* taxon_id * character list *)

module OrderedTuple = struct
    type t = pairs
    let compare (a, _) (b, _) = a - b
end

module AssList = Set.Make (OrderedTuple)

type cg = (unit -> int)


let fprintf = Printf.fprintf

let code_generator () =
    let counter = ref (-1) in
    fun () ->
        incr counter;
        !counter

let create_ias s code cg =
    let add_codes ((code_acc, hom_acc, order_lst) as res) pos _ =
        if (pos = 0) then res 
        else 
            let code = cg () in
            Hashtbl.add code_acc pos code;
            let single = `Single code in 
            Hashtbl.add hom_acc code single;
            code_acc, hom_acc, code :: order_lst
    in
    let c = Hashtbl.create 1667
    and h = Hashtbl.create 1667 in
    let c, h, o = Sequence.foldi add_codes (c, h, []) s in
    { seq = s; codes = c; homologous = h; cannonic_code = code; order = o }


let create_ias_chrom s code cg =
    let add_codes (code_acc, hom_acc, order_lst) pos _ =
        let code = cg () in 
        Hashtbl.add code_acc pos code; 
        let single = `Single code in 
        Hashtbl.add hom_acc code single; 
        code_acc, hom_acc, code :: order_lst 
    in
    let c = Hashtbl.create 1667
    and h = Hashtbl.create 1667 in
    let c, h, o = Sequence.foldi add_codes (c, h, []) s in
    { seq = s; codes = c; homologous = h; cannonic_code = code; order = o }


let rec prepend_until_shared tgt src it = 
    match src with
    | h :: t when h = it -> tgt, t
    | h :: t -> prepend_until_shared (h :: tgt) t it
    | [] -> failwith "prepend_until_shared"

let rec prepend_all tgt = function
    | h :: t -> prepend_all (h :: tgt) t
    | [] -> tgt

let print_debug a' b' a b m =
    let printem = Status.user_message (Status.Output (Some "ia_dia", false, [])) in
    printem "For implied alignment:\n";
    printem (Sequence.to_formater a' Alphabet.nucleotides );
    printem "\n";
    printem (Sequence.to_formater b' Alphabet.nucleotides);
    printem "\n";
    printem "This is the alignment matrix\n";
    Sequence.Align.print_backtrack a.seq b.seq m;
    printem "\n"

let print_algn_debug = false
let print_anc_debug = false

(** [ancestor a b cm m] creates a common ancestor for sequences [a] and [b]
 * using the cost matrix [cm] and the alignment matrix [m] 
 * The resulting common ancestor holds the homology
 * relationships of the codes assigned in [a] and [b]. *)
let ancestor prealigned all_minus_gap a b cm m = 
    let codea = a.cannonic_code
    and codeb = b.cannonic_code in
    if print_anc_debug then
        Status.user_message Status.Information
        ("The ancestors of " ^ string_of_int codea ^ " and " ^ string_of_int codeb);
    let a, b, mincode = 
        if codea < codeb then a, b, codea
        else b, a, codeb
    in
    let lena = Sequence.length a.seq
    and lenb = Sequence.length b.seq 
    and gap = Cost_matrix.Two_D.gap cm in
    let create_gaps len = Sequence.init (fun _ -> gap) len 
    and aempty = Sequence.is_empty a.seq gap
    and bempty = Sequence.is_empty b.seq gap in
    let a', b', _, nogap = 
        if aempty && bempty then
            if lena > lenb then a.seq, a.seq, 0, `A
            else b.seq, b.seq, 0, `B
        else if aempty then
            (create_gaps lenb), b.seq, 0, `A
        else if bempty then
            a.seq, (create_gaps lena), 0, `B
        else 
            if prealigned then a.seq, b.seq, 0, `Both
            else
                let a, b, c = Sequence.Align.align_2 a.seq b.seq cm m in
                a, b, c, `Both
    in
    if print_algn_debug then print_debug a' b' a b m;
    let lena' = Sequence.length a' in
    let anc = Sequence.create (lena' + 1) 
    and a_ord = a.order
    and b_ord = b.order in
    let rec builder = 
        fun position a_pos b_pos anc_pos codes hom a_hom b_hom a_or b_or res_or ->
        if position > (-1) then begin
            let it_a = Sequence.get a' position
            and it_b = Sequence.get b' position in
            let med = 
                match nogap with
                | `A -> it_b
                | `B -> it_a
                | `Both -> Cost_matrix.Two_D.median it_a it_b cm 
            in
            let is_gap_median =
                if it_a <> gap && it_b <> gap then
                    (((Cost_matrix.Two_D.cost it_a it_b cm) <
                    (Cost_matrix.Two_D.cost (all_minus_gap it_a)
                    (all_minus_gap it_b) cm)) || (med = gap))
                else (med = gap)
            in
            let code, hom, n_a_pos, n_b_pos, na_hom, nb_hom, a_or, b_or, res_or =
                match is_gap_median, it_a = gap, it_b = gap with
                | false, false, false ->
                        let codea = Hashtbl.find a.codes a_pos 
                        and codeb = Hashtbl.find b.codes b_pos in
                        let hom_a = Hashtbl.find a_hom codea 
                        and hom_b = Hashtbl.find b_hom codeb in
                        Hashtbl.remove a_hom codea;
                        Hashtbl.remove b_hom codeb;
                        let new_ab = `Set [hom_a; hom_b] in
                        let new_res_or, new_a_or = 
                            prepend_until_shared res_or a_or codea in
                        let new_res_or, new_b_or = 
                            prepend_until_shared new_res_or b_or codeb in
                        Hashtbl.replace hom codea new_ab;
                        codea, hom, a_pos - 1, b_pos - 1, a_hom, 
                        b_hom, new_a_or, new_b_or, (codea :: new_res_or)
                | true, false, false ->
                        let codeb = Hashtbl.find b.codes b_pos 
                        and codea = Hashtbl.find a.codes a_pos in
                        let hom_b = Hashtbl.find b_hom codeb 
                        and hom_a = Hashtbl.find a_hom codea in
                        Hashtbl.remove b_hom codeb;
                        Hashtbl.remove a_hom codea;
                        let new_res_or, new_b_or =
                            prepend_until_shared res_or b_or codeb 
                        in
                        let new_res_or, new_a_or = 
                            prepend_until_shared new_res_or a_or codea 
                        in
                        Hashtbl.add hom codeb hom_b;
                        Hashtbl.add hom codea hom_a;
                        let new_res_or = (codea :: codeb :: new_res_or) in
                        codea, hom, a_pos - 1, b_pos - 1, a_hom, b_hom,
                        new_a_or, new_b_or, new_res_or
                | _, true, false ->
                        let codeb = Hashtbl.find b.codes b_pos in
                        let hom_b = Hashtbl.find b_hom codeb in
                        Hashtbl.remove b_hom codeb;
                        Hashtbl.replace hom codeb hom_b;
                        let new_res_or, new_b_or =
                            prepend_until_shared res_or b_or codeb in
                        codeb, hom, a_pos, b_pos - 1, a_hom, b_hom,
                        a_or, new_b_or, (codeb :: new_res_or)
                | _, false, true ->
                        let codea = Hashtbl.find a.codes a_pos in
                        let hom_a = Hashtbl.find a_hom codea in
                        Hashtbl.remove a_hom codea;
                        Hashtbl.replace hom codea hom_a;
                        let new_res_or, new_a_or = 
                            prepend_until_shared res_or a_or codea in
                        codea, hom, a_pos - 1, b_pos, a_hom, b_hom,
                        new_a_or, b_or, (codea :: new_res_or)
                | _, true, true ->
(*                        assert (a_pos = 0);*)
(*                        assert (b_pos = 0);*)
                        (-1), hom, a_pos, b_pos, a_hom, b_hom, a_or, b_or,
                        res_or
            in
            let n_anc_pos = 
                if not is_gap_median then begin
                    Sequence.prepend anc med;
                    Hashtbl.replace codes anc_pos code;
                    anc_pos + 1
                end else anc_pos
            in
            
(*            List.iter (fun ord -> fprintf stdout "%i " ord) res_or;
            print_newline (); *)
            builder (position - 1) n_a_pos n_b_pos n_anc_pos codes hom na_hom
            nb_hom a_or b_or res_or
        end else begin
            (* We have to prepend a gap to the
            *  ancestor, though we don't include it in the set of homologies. *)
            Sequence.prepend anc gap; 
            (* The codes of the ancestor positions are shifted and are wrong,
            * depending on weather or not the median in a particular position is
            * a gap. And so, we fix the codes *)
            let codes =
                let res = Hashtbl.create 1667 in
                Hashtbl.iter 
                (fun k c -> Hashtbl.add res (anc_pos - k) c) codes;
                res
            in
            (* Now we add the homologies that are in the tree above us, after
            * the current sequence. *)
            Hashtbl.iter (Hashtbl.replace a_hom) hom;
            Hashtbl.iter (Hashtbl.replace a_hom) b_hom;
            let new_res_or = prepend_all res_or a_or in
            let new_res_or = prepend_all new_res_or b_or in
            codes, a_hom, new_res_or
        end
    in
    let initial_codes = Hashtbl.create 1667
    and initial_hom = Hashtbl.create 1667 in
    let codes, hom, order = 
        builder (lena' - 1) (lena - 1) (lenb - 1) 0 initial_codes
        initial_hom a.homologous b.homologous a_ord b_ord []
    in
    let order = List.rev order in
    { seq = anc; codes = codes; homologous = hom; cannonic_code = mincode;
    order = order }

let cmp_chrom_cost isa1 isa2 c2 chrom_pam = 
    let med1 = ChromAli.create_med isa1.seq in 
    let med2 = ChromAli.create_med isa2.seq in
    let cost, _ = ChromAli.cmp_cost med1 med2 c2 chrom_pam in 
    cost

(* Merge the implied alignments of two clades and their respective roots into
* one common ancestor *)
let ancestor_chrom a_ls b_ls cm chrom_pam m = 
    let first_a = List.hd a_ls in  
    let first_b = List.hd b_ls in 
    let a_ls, b_ls, min_can_code = 
        if first_a.cannonic_code < first_b.cannonic_code then a_ls, b_ls, first_a.cannonic_code
        else b_ls, a_ls, first_b.cannonic_code
    in
    let _, a, b = 
        List.fold_left 
            (fun (min_cost, best_a, best_b) isa_a -> 
                 List.fold_left 
                     (fun (min_cost, best_a, best_b) isa_b -> 
                          let cost = cmp_chrom_cost isa_a isa_b cm chrom_pam in 
                          if cost < min_cost then cost, isa_a, isa_b
                          else min_cost, best_a, best_b
                     ) (min_cost, best_a, best_b) b_ls                  
            ) (max_int, first_a, first_b) a_ls  
    in 
    let lena = Sequence.length a.seq
    and lenb = Sequence.length b.seq 
    and gap = Cost_matrix.Two_D.gap cm in
    let create_gaps len = Sequence.init (fun _ -> gap) len 
    and aempty = Sequence.is_empty a.seq gap
    and bempty = Sequence.is_empty b.seq gap in
    let seq_a, seq_b =
        if aempty && bempty then
            if lena > lenb then a.seq, a.seq
            else b.seq, b.seq
        else if aempty then
            (create_gaps lenb), b.seq
        else if bempty then
            a.seq, (create_gaps lena)
        else a.seq, b.seq
    in  
    let med_a = ChromAli.create_med seq_a in 
    let med_b = ChromAli.create_med seq_b in 
    let _, _, med_ls = ChromAli.find_med2_ls med_a med_b cm chrom_pam in 
    let ordera_arr = Array.of_list a.order in 
    let orderb_arr = Array.of_list b.order in 
    let homs_a = a.homologous in 
    let homs_b = b.homologous in 
    let create_order_map order_ls = 
        let order_map, _ = 
            List.fold_left (fun (order_map, p) code ->  
                             (Codes.add code p order_map), p + 1
                        ) (Codes.empty, 0) order_ls
        in 
        order_map
    in 
    let codea_set = Hashtbl.fold 
        (fun _ code codea_set -> IntSet.add code codea_set) a.codes IntSet.empty
    in 
    let codeb_set = Hashtbl.fold 
        (fun _ code codeb_set -> IntSet.add code codeb_set) b.codes IntSet.empty
    in 
    let ordera_map = create_order_map a.order in 
    let orderb_map = create_order_map b.order in 
    let add_deled_pos code order_arr order_map code_set homs res_order_ls res_homs = 
        let rec adder p delted_order_ls =
            if p >= 0 then begin
                let delted_code = order_arr.(p) in 
                let stop = 
                    match IntSet.mem delted_code code_set with 
                    | true ->  true
                    | false ->
                          let a_hom = Hashtbl.find homs delted_code in  
                          Hashtbl.add res_homs delted_code a_hom; 
                          false 
                in 
                if stop = true then List.rev delted_order_ls
                else adder (p - 1) (delted_code::delted_order_ls) 
            end else List.rev delted_order_ls
        in 
        let pos = 
            try 
                Codes.find code order_map 
            with Not_found -> Array.length order_arr            
        in 
        let delted_order_ls = adder (pos - 1) [] in
        (List.append delted_order_ls res_order_ls), res_homs
    in 
    let builder_med ias_ls med =
        let builder (res_codes, res_homs, res_or) (med_pos, med, a_pos, it_a, b_pos, it_b) = 
(*
            fprintf stdout "%i %i, %i %i, %i %i" med_pos med a_pos it_a  b_pos it_b;
            print_newline ();
*)
            let code, res_homs, res_or = 
                match  (med=gap), it_a=gap, it_b=gap with
                | false, false, false ->
                      let codea = Hashtbl.find a.codes a_pos in
                      let codeb = Hashtbl.find b.codes b_pos in
(*
                      fprintf stdout "codea: %i\n" codea; flush stdout;
                      fprintf stdout "codeb: %i\n" codeb; flush stdout;
*)
                      let hom_a = Hashtbl.find homs_a codea 
                      and hom_b = Hashtbl.find homs_b codeb in
                      let new_ab = `Set [hom_a; hom_b] in
                      let res_or, res_homs = add_deled_pos codea ordera_arr
                          ordera_map codea_set homs_a res_or res_homs
                      in 
                      let res_or, res_homs = add_deled_pos codeb orderb_arr
                          orderb_map codeb_set homs_b res_or res_homs
                      in 
                      Hashtbl.replace res_homs codea new_ab; 
                      codea, res_homs, (codea :: res_or) 
                | true, false, false ->
                      let codea = Hashtbl.find a.codes a_pos in
                      let codeb = Hashtbl.find b.codes b_pos in
                      let hom_b = Hashtbl.find homs_b codeb 
                      and hom_a = Hashtbl.find homs_a codea in
                      let res_or, res_homs = add_deled_pos codea ordera_arr
                          ordera_map codea_set homs_a res_or res_homs
                      in 
                      let res_or, res_homs = add_deled_pos codeb orderb_arr
                          orderb_map codeb_set homs_b res_or res_homs
                      in 
                      Hashtbl.add res_homs codeb hom_b;
                      Hashtbl.add res_homs codea hom_a;
                      let res_or = (codea :: codeb :: res_or) in
                      codea, res_homs, res_or
                | _, true, false ->
                      let codeb = Hashtbl.find b.codes b_pos in
                      let hom_b = Hashtbl.find homs_b codeb in
                      Hashtbl.replace res_homs codeb hom_b;
                      let res_or, res_homs = add_deled_pos codeb orderb_arr
                          orderb_map codeb_set homs_b res_or res_homs
                      in 
                      codeb, res_homs, (codeb :: res_or)
                | _, false, true ->
                      let codea = Hashtbl.find a.codes a_pos in
                      let hom_a = Hashtbl.find homs_a codea in
                      Hashtbl.replace res_homs codea hom_a;
                      let res_or, res_homs = add_deled_pos codea ordera_arr
                          ordera_map codea_set homs_a res_or res_homs
                      in 
                      codea, res_homs, (codea :: res_or)
                | _, true, true ->
                      (-1), res_homs, res_or
            in 
            (if (med != gap) then 
                Hashtbl.replace res_codes med_pos code);
            res_codes, res_homs,  res_or        
        in 
        let init_codes = Hashtbl.create 1667
        and init_hom = Hashtbl.create 1667 in
        let map = ChromAli.convert_map med in  
        let res_codes, res_homs, res_or =
            List.fold_left builder 
                (init_codes, init_hom, [])  (List.rev map)
        in
        let res_or, res_homs = add_deled_pos (-1) ordera_arr ordera_map codea_set homs_a
            res_or res_homs 
        in 
        let res_or, res_homs = add_deled_pos (-1) orderb_arr orderb_map codeb_set homs_b
            res_or res_homs 
        in 
        let rev_res_or = List.rev res_or in
        let ias = {seq = med.ChromAli.seq; codes = res_codes; 
                   homologous = res_homs; cannonic_code = min_can_code;
                   order = rev_res_or}
        in 
        ias::ias_ls
    in 
    let ias_ls = List.fold_left builder_med [] med_ls in 
    ias_ls
    
exception IsSankoff

type matrix_class = 
    | AllOne of int
    | AllOneGapSame of (int * int)
    | AffinePartition of (int * int * int)
    | AllSankoff


(* A function that analyzes a cost matrix and an alphabet and
* generates a pair of functions f and g, such that g converts 
* a state into a list of character states, and g converts a state into it's
* appropriate Parser.Hennig.Encoding.s *)
let analyze_tcm tcm alph =
    let gap = Alphabet.get_gap alph 
    and all = Alphabet.get_all alph in
    let alph = Alphabet.simplified_alphabet alph in
    let single_compare (_, a) res (_, b) =
        match res with
        | None -> 
                let cost = (Cost_matrix.Two_D.cost a b tcm) in
                Some cost
        | Some y ->
                let x = Cost_matrix.Two_D.cost a b tcm in
                if x = y then res
                else raise IsSankoff
    in
    let rec compare_costs l1 l2 res =
        match l1, l2 with
        | _, []
        | [], _ -> res
        | h1 :: t1, _ :: t2 ->
                compare_costs t1 t2 (List.fold_left (single_compare h1) res l2)
    in
    let get_cost_of_all_subs () =
        match 
            List.filter (fun (_, x) -> (x <> gap) && (x <> all))
            (Alphabet.to_list alph) 
        with
        | [] -> failwith "An empty alphabet?"
        | (_ :: t) as res ->
                match compare_costs res t None with
                | Some v -> v
                | None -> failwith "No costs?"
    in
    let get_cost_of_gap () =
        match 
            List.filter (fun (_, x) -> (x <> gap) && (x <> all))
            (Alphabet.to_list alph) 
        with
        | [] -> failwith "An empty alphabet?"
        | res ->
                match compare_costs [("", gap)] res None with
                | Some v -> v
                | None -> failwith "No costs?"
    in
    let get_gap_opening tcm =
        match Cost_matrix.Two_D.affine tcm with
        | Cost_matrix.No_Alignment 
        | Cost_matrix.Linnear -> failwith "not affine"
        | Cost_matrix.Affine go -> go
    in
    let all_same_affine () =
        try let _ = get_gap_opening tcm in true with
        | _ -> false
    in
    let get_case =
        try
            let all_excepting_gap = get_cost_of_all_subs ()
            and all_and_gap = get_cost_of_gap () in
            match Alphabet.kind alph with
            | Alphabet.Simple_Bit_Flags ->
                    if 32 > Alphabet.distinct_size alph then
                        if all_same_affine () then
                            AffinePartition 
                            (all_excepting_gap, all_and_gap, 
                            get_gap_opening tcm)
                        else if all_excepting_gap = all_and_gap then 
                            AllOne all_excepting_gap
                        else if all_excepting_gap = 1 then
                            AllOneGapSame 
                            (all_excepting_gap, all_and_gap)
                        else AllSankoff
                    else AllSankoff
            | _ -> AllSankoff
        with
        | IsSankoff -> AllSankoff
    in
    match get_case with
    | AllOne weight ->
            let encoding = 
                Parser.Hennig.Encoding.set_weight
                Parser.Hennig.Encoding.dna_encoding weight
            in
            let to_parser is_missing states acc = 
                match is_missing, states with
                | `Missing, _ -> 
                        Parser.Unordered_Character (all, false) :: acc
                | `Exists, 0 -> Parser.Unordered_Character (gap, false) :: acc
                | `Exists, x -> Parser.Unordered_Character (x, false) :: acc
            and to_encoding _ acc = encoding :: acc in
            get_case, to_parser, to_encoding
    | AllOneGapSame (subsc, gapcost) ->
            let present_absent = 
                Parser.Hennig.Encoding.gap_encoding gapcost
            and subs = 
                Parser.Hennig.Encoding.set_weight Parser.Hennig.Encoding.dna_encoding
                subsc
            in
            let notgap = lnot gap in
            let all = notgap land all in
            let to_parser is_missing states acc =
                match is_missing, states with
                | `Missing, _ ->
                        Parser.Unordered_Character (all, false) ::
                            Parser.Unordered_Character (1 lor 2, false) :: acc
                | `Exists, 0 ->
                        (* All characters, and the gap itself, in other words,
                        * we treat the gap as a separate character, and the
                        * state as missing data *)
                        Parser.Unordered_Character (all, false) ::
                            Parser.Unordered_Character (1, false) :: acc
                | `Exists, x ->
                            Parser.Unordered_Character (x land notgap, false) ::
                                (if x = all then 
                                    Parser.Unordered_Character (3, false) 
                                else 
                                Parser.Unordered_Character (2, false))
                                :: acc
            and to_encoding _ acc = 
                subs :: present_absent :: acc
            in
            get_case, to_parser, to_encoding
    | AffinePartition (subsc, gapcost, gapopening) ->
            (* We have to partition the column in three columns, each
            * corresponding to gap opening, gap extension, and substitution.
            * We will have to filter out columns that are not gap opening
            * but only extension.
            * *)
            let gap_opening = Parser.Hennig.Encoding.gap_encoding gapopening
            and gap_extension = Parser.Hennig.Encoding.gap_encoding gapcost
            and subs = 
                Parser.Hennig.Encoding.set_weight
                Parser.Hennig.Encoding.dna_encoding
                subsc
            in
            let notgap = lnot gap in
            let all = notgap land all in
            let to_parser is_missing states acc =
                match is_missing, states with
                | `Missing, _ ->
                        Parser.Unordered_Character (1 lor 2, false) ::
                            Parser.Unordered_Character (1 lor 2, false) :: 
                                Parser.Unordered_Character (all, false) ::
                                    acc
                | `Exists, 0 -> 
                        (* We have a gap, so we assign both gap opening and
                        * gap extension, we will later cleaunup when gap
                        * opening is not needed *)
                        Parser.Unordered_Character (1, false) ::
                            Parser.Unordered_Character (1, false) ::
                                Parser.Unordered_Character (all, false) ::
                                    acc
                | `Exists, x ->
                        let prev = 
                            (if x = all then
                                Parser.Unordered_Character (1 lor 2, false)
                            else Parser.Unordered_Character (2, false))
                        in
                        prev :: prev :: 
                            Parser.Unordered_Character ((x land notgap), 
                            false) :: 
                                acc
            and to_encoding _ acc =
                gap_opening :: gap_extension :: subs :: acc
            in
            get_case, to_parser, to_encoding
    | AllSankoff ->
            let size = 
                (* We remove one from the all elements representation *)
                (Alphabet.distinct_size alph) - 1 
            in
            let make_tcm () =
                match Alphabet.kind alph with
                | Alphabet.Simple_Bit_Flags ->
                        Array.init size (fun x -> Array.init size 
                        (fun y -> 
                            Cost_matrix.Two_D.cost (1 lsl x) (1 lsl y) tcm)) 
                | Alphabet.Sequential ->
                        Array.init size (fun x -> 
                            Array.init size (fun y ->
                                Cost_matrix.Two_D.cost (x + 1) (y  + 1) tcm))
                | Alphabet.Extended_Bit_Flags -> 
                        failwith "Impliedalignment.make_tcm"
            in
            let enc = 
                let res = Parser.Hennig.Encoding.default () in
                let res = Parser.Hennig.Encoding.set_min res 0 in
                let res = Parser.Hennig.Encoding.set_max res (size - 1) in
                let set = 
                    let rec add_consecutive_integers cur max acc = 
                        if cur = max then acc
                        else 
                            add_consecutive_integers (cur + 1) max 
                            (All_sets.Integers.add cur acc)
                    in
                    add_consecutive_integers 0 size All_sets.Integers.empty
                in
                let res = Parser.Hennig.Encoding.set_set res set in
                Parser.Hennig.Encoding.set_sankoff res (make_tcm ())
            in
            let convert_to_list x =
                match Alphabet.kind alph with
                | Alphabet.Simple_Bit_Flags ->
                        let rec match_bit v pos mask acc = 
                            if pos = 6 then acc
                            else if 0 <> (v land mask) then
                                match_bit v (pos + 1) (mask lsl 1) 
                                ((pos - 1) :: acc)
                            else match_bit v (pos + 1) (mask lsl 1) acc
                        in
                        match_bit x 1 1 []
                | Alphabet.Sequential -> [x]
                | Alphabet.Extended_Bit_Flags -> 
                        failwith "Impliedalignment.convert_to_list"
            in
            let all = convert_to_list all in
            let gap_code =
                (* Always the last code is the one of a gap in Sankoff *)
                size - 1
            in
            let to_parser is_missing states acc = 
                match is_missing, states with
                | `Missing, _ ->
                        (Parser.Sankoff_Character (all, false)) :: acc
                | `Exists, 0 -> 
                        (Parser.Sankoff_Character ([gap_code], false)) ::
                            acc
                | `Exists, x -> 
                        let tuple = ((convert_to_list x),  false) in
                        (Parser.Sankoff_Character tuple) :: acc
            and to_encoding _ acc = 
                enc :: acc 
            in
            get_case, to_parser, to_encoding


module type S = sig
    type a 
    type b
    type tree = (a, b) Ptree.p_tree

            
    (** [of_tree t] generates the implied alignment of all the sequences in the tree
    * [t]. *)
    val of_tree : ((int -> int) * tree) -> Methods.implied_alignment


    val concat_alignment :
          (int * int array list All_sets.IntegerMap.t list) list list ->
          (int * int array All_sets.IntegerMap.t list) list list

    val create : (tree -> int list -> tree) ->
        int list -> Data.d ->
        tree -> Methods.implied_alignment list

    val to_static_homologies : bool ->
        (tree -> int list -> tree) ->
            bool  -> Methods.characters -> Data.d -> tree -> Data.d

end
module Make (Node : NodeSig.S) (Edge : Edge.EdgeSig with type n = Node.n) = struct
    type a = Node.n
    type b =  Edge.e
    type tree = (a, b) Ptree.p_tree

    (** return (taxon_id, character_ls) list (of taxa) * (final ias for each
        character set) list (of characters) *)
    let of_tree_handle all_minus_gap cg handle ptree =
        let vertices = 
            try
                let root = Ptree.get_component_root handle ptree in
                match root.Ptree.root_median with
                | Some (_, v) -> 
                        Some (((Node.num_otus None v) * 2) - 1)
                | None -> None
            with
            | Not_found -> None
        in
        let st = 
            Status.create "Implied Alignments" vertices "vertices calculated"
        in
        let convert_node parent ptree _ id _ =
            let data = Ptree.get_node_data id ptree in
            let taxon_id = Node.taxon_code data in
            let data = 
                let par =                     
                    match parent with
                    | Some _ -> parent
                    | None ->
                            Some (Ptree.get_parent taxon_id ptree)
                in
                let nd = Node.get_dynamic_preliminary par data in 
                nd 
            in
            let data = List.map 
                (fun dyn ->
                     let sequences = DynamicCS.leaf_sequences dyn in      
                     let new_sequences = 
                         match DynamicCS.state dyn with 
                         | `Seq -> 
                               Codes.fold 
                                   (fun code seq acc ->
                                        Codes.add code ([create_ias seq taxon_id cg]) acc
                                   ) sequences Codes.empty  
                         | `Chromosome -> 
                               Codes.fold 
                                   (fun code seq acc ->
                                        Codes.add code ([create_ias_chrom seq taxon_id cg]) acc
                                   ) sequences Codes.empty  
                         | _ -> failwith "create_new_sequences in Impliedalignment"
                     in                                   
                     {sequences = new_sequences;   
                      c2 = DynamicCS.c2 dyn;   
                      chrom_pam = DynamicCS.chrom_pam dyn;  
                      state = DynamicCS.state dyn; 
                      code = DynamicCS.code dyn; }   
                ) data   
            in
            let did = Status.get_achieved st in
            Status.full_report ~adv:(did + 1) st;
            AssList.singleton (taxon_id, data), data 
        in
        let join_2_nodes _ _ (ac, a) (bc, b) =
            let t_ancestor x y =
                let state = x.state in 
                Codes.fold (fun u v acc ->
                                let hom = Codes.find u y.sequences in
                                let anc = match state with 
                                | `Seq ->
                                      let anc = ancestor false all_minus_gap 
                                          (List.hd v) (List.hd hom) x.c2 Matrix.default
                                      in 
                                      [anc]
                                | `Chromosome ->
                                      ancestor_chrom v hom x.c2
                                          x.chrom_pam Matrix.default 
                                | _ -> failwith "Only implied alignment for Seq and Chrom"
                                in 
                                Codes.add u anc acc 
                           ) x.sequences Codes.empty
            in
            let rec ancestor_builder x y =
                match x, y with
                | hx :: tx, hy :: ty ->
                    { hx with sequences = t_ancestor hx hy } ::
                        ancestor_builder tx ty
                | [], [] -> []
                | _, _ -> failwith "Inconsistant data"
            in
            AssList.union ac bc, ancestor_builder a b
        in 
        match Tree.get_node handle ptree.Ptree.tree with
        | Tree.Single self -> 
                let a, b = convert_node None ptree () self ([], []) in
                Status.finished st;
                AssList.elements a, b
        | _ ->
                let self, other = 
                    let root = Ptree.get_component_root handle ptree in
                    match root.Ptree.root_median with
                    | Some ((`Edge (a, b)), the_root) -> a, b
                    | _ -> failwith "no root?"
                in
                let a, b = 
                    Ptree.post_order_node_with_edge_visit 
                    (convert_node None ptree) join_2_nodes (Tree.Edge (self, other)) ptree
                    (AssList.empty, [])
                in 
                let a' = 
                    let new_ptree = 
                        let self_data = Ptree.get_node_data self ptree 
                        and other_data = Ptree.get_node_data other ptree in
                        let single = 
                            Node.to_single ~is_root:true (Some self) other_data (Some other) self_data 
                        in
                        (*
                        Status.user_message Status.Information
                        ("The assigned root in the implied alignment is " ^
                        Node.to_string single);
                        *)
                        Ptree.add_node_data self single ptree
                    in
                    convert_node (Some other) new_ptree () self ([], []) 
                in
                let a = join_2_nodes () () a a' in
                let x, y = join_2_nodes () () a b in
                Status.finished st;
                (if Tree.is_leaf self ptree.Ptree.tree then
                    AssList.elements x
                else List.remove_assoc self (AssList.elements x)), y



    (** t is a final ias for a character set   
        =>
        (number columns, (com_hom_code -> column) map, (code -> com_hom_code) map) map (a character set) *)
    let invert_codes t = 
        let seqs = t.sequences in
        Codes.fold 
            (fun char_code iat_ls acc ->
                 let iat = List.hd iat_ls in  
                 let homs = iat.homologous in  
                 let order = iat.order in 
                 (* number columns, (code -> column) map, (hom_code -> code) map *)
                 let res = List.fold_left   
                     (fun (c, remap, acc) code ->  
                          let hom = Hashtbl.find homs code  in 
                          let acc =   
                              Sexpr.fold_left (fun acc hom_code -> Codes.add hom_code code acc) acc hom 
                          in  
(*                          fprintf stdout "code: %i -> col: %i\n" code c;*)
                          c + 1, Codes.add code c remap, acc  
                     ) (0, Codes.empty, Codes.empty) order  
                 in                    
                 Codes.add char_code res acc  
            ) seqs Codes.empty  
 (**  (number columns, (com_hom_code -> column) map, (code -> com_hom_code) map
  *  return arrays of aligned bases and aligned positions
  *) 
    let convert_a_taxon (len, remap, recode) ias =
        let column code = 
            let fin_code =  
                try   
                    Codes.find code recode  
                with | Not_found ->  
                    failwith ("Not_found hom_code ->  fin_code " ^ (string_of_int code))  
            in 
            try 
                Codes.find fin_code remap 
            with Not_found ->
                failwith ("Not_found in fin_code -> column" ^ (string_of_int fin_code))  
        in
        let results = Array.make (len + 1) 0 in
        let pos_results = Array.make (len + 1) (-1) in
        let add_result ias =
            Hashtbl.iter (fun pos code -> 
                let base = Sequence.get ias.seq pos in
                let col = column code in
                let col = len - col in
                results.(col) <- base;
                pos_results.(col) <- pos;
                         ) ias.codes
        in
        add_result ias;
        results, pos_results
   (** (taxon_id * list of alignments for each character 
       in the character set) list (of characters) ) list (of taxa) *)
    let of_tree (all_but_gap, tree) = 
        let cg = code_generator () in
    (** return ( (taxon_id, character_ls) list (of taxa) * (final ias for each character)
        list (of characters) ) list (of handles) *)

        let res = 
            Handles.fold (fun (x : int) acc -> 
                              (of_tree_handle all_but_gap cg x tree) :: acc
                         ) (Tree.get_handles tree.Ptree.tree) []
        in

   (** ((taxon_id * (aligned_code arrays for each character
      set) list (of characters) ) list (of taxa) ) of list (of handles)*)
        let ali = List.map 
            (fun ((a:pairs list), (b:t list)) -> 
                       (** (char_code -> (int * int Codes.t * 
                           int Codes.t)) Codes.t list 
                          (number columns, (com_hom_code -> column) map, (code -> com_hom_code) map) 
                          map (a character set) list (of character sets)  
                      *) 

                 let inv_codes_b = List.map invert_codes b in
                      (** (taxon_id * (aligned_code arrays for each character
                          set) list (of characters) ) list (of taxa) *)
                 let char_codes = ref IntSet.empty  in 
                 let new_a = List.map  
                      (fun ((tc : int), (tit : t list)) ->
                           let rec builder a b = 
                               match a, b with 
                               | ha :: ta, hb :: tb -> 
                                     let result =  Codes.fold 
                                         (fun char_code fi_hom
                                              (acc_bases, acc_pos) ->  
                                                  char_codes := IntSet.add char_code !char_codes;
                                                  let tmp = Codes.find char_code hb.sequences in 
                                                  let bases, pos =
                                                      convert_a_taxon fi_hom (List.hd tmp) 
                                                  in 
                                                  (Codes.add char_code bases acc_bases),
                                                  (Codes.add char_code pos acc_pos)
                                         ) ha (Codes.empty, Codes.empty)
                                     in 
                                     result :: (builder ta tb) 
                               | [], [] -> [] 
                               | _, _ -> failwith "Unexpected" 
                           in 
                           tc, builder inv_codes_b tit 
                      ) a 
                 in 
                 let get_ali_pos looking_char_code = 
                     List.fold_left 
                         (fun alied_seq_ls (tx_code, ali) ->
                              List.fold_left 
                                  (fun alied_seq_ls (_, alied_pos) ->
                                       Codes.fold 
                                           (fun char_code alied_seq alied_seq_ls ->
                                                if char_code = looking_char_code
                                                then alied_seq::alied_seq_ls
                                                else alied_seq_ls
                                           ) alied_pos alied_seq_ls
                                  ) alied_seq_ls ali
                         ) [] new_a
                 in 

                 let break_map = 
                     IntSet.fold 
                         (fun char_code break_pos_map ->
                              let ali_pos_ls = get_ali_pos char_code in
                              let alied_pos_mat = Array.of_list ali_pos_ls in 
                              let num_taxa = Array.length alied_pos_mat in 
                              let num_col = Array.length (alied_pos_mat.(1)) in
                              let break_ls = ref [] in 
                              let sta = ref 0 in 
                              let cur_pos_arr = Array.init num_taxa (fun ti -> alied_pos_mat.(ti).(0)) in 

                              for col = 1 to num_col - 1 do
                                  let continue = ref true in 
                                  for t = 0 to num_taxa - 1 do 
                                      let pre_pos = cur_pos_arr.(t) in 
                                      let pos = alied_pos_mat.(t).(col) in
                                      (if (pre_pos != -1) && (pos != -1) && (abs (pos - pre_pos) > 1) then
                                           continue := false);  
                                  done;  
                                  
                                  if !continue then begin 
                                      for t = 0 to num_taxa - 1 do
                                          if alied_pos_mat.(t).(col) != -1 then 
                                              cur_pos_arr.(t) <- alied_pos_mat.(t).(col);
                                      done;
                                  end  else begin
                                      break_ls:= List.append !break_ls [(!sta, (col - 1))] ;
                                      sta := col;
                                      for t = 0 to num_taxa - 1 do
                                          cur_pos_arr.(t) <- alied_pos_mat.(t).(col)
                                      done;
                                  end 
                              done;   
                              break_ls:= List.append  !break_ls [(!sta, (num_col - 1))]; 
                              Codes.add char_code !break_ls break_pos_map
                         ) !char_codes (Codes.empty)  
                 in

                 let new_a = List.map 
                     (fun (taxa_code, new_a_taxa) ->
                          let new_a_taxa = List.map 
                              (fun (base_map, _) -> Codes.mapi 
                                   (fun char_code alied_seq ->
                                        let break_ls = Codes.find char_code break_map in 
                                        let seg_ls = Utl.break_array alied_seq break_ls in 

                                        seg_ls
                                   ) base_map
                              ) new_a_taxa
                          in 
                          (taxa_code, new_a_taxa)
                     ) new_a
                 in 
                 new_a
            ) res  
        in 
        ali 

    let post_process_affine_gap_cost subs gapcost gapopening (enc, taxa) =
        let process_position chars pos = 
            let v_pos npos = 
                match chars.(npos) with
                | Parser.Unordered_Character x -> x
                | _ -> failwith "How is this possible?"
            in
            let val_pos npos = let a, _ = v_pos npos in a in
            let _, bol = v_pos (pos + 1) in
            if (0 <> (val_pos (pos + 2) land val_pos (pos + 5))) then
                chars.(pos + 1) <- Parser.Unordered_Character (2, bol)
            else if 2 = val_pos (pos + 1) then
                chars.(pos + 4) <- Parser.Unordered_Character (1, bol)
            else ()
        in
        let correct_position pos =
            List.iter (fun (chars, _) -> process_position chars pos) taxa
        in
        let start = Array.length enc - 7 in
        let rec iterator pos =
            if pos < 0 then (enc, taxa)
            else 
                let () = correct_position pos in
                iterator (pos - 3)
        in
        iterator start

    let ia_to_parser_compatible data imtx =
        match imtx with
        | [all_taxa] ->
                let process_each = fun (acc, enc, clas) (taxcode, sequence) ->
                    let preprocess_sequence alph x =
                        let len = Array.length x in
                        let rec check it =
                            if it = len then true
                            else if x.(it) = 0 then check (it + 1)
                            else false
                        in
                        if check 0 then begin
                            for i = len - 1 downto 0 do
                                x.(i) <- Alphabet.get_all alph
                            done;
                            `Missing
                        end else `Exists
                    in
                    (* Fold over every sequence, and return a list containing all of
                    * them in a tuple with their code and the sequence itself in a
                    * preprocessed way (if the sequence is missing data, then all
                    * the states are on), and a map of the sequence code to the
                    * function that will convert an observed state into it's
                    * appropriate Parser.t *)
                    let sequence, transform_functions = 
                        List.fold_left (fun acc s -> 
                            All_sets.IntegerMap.fold 
                                (fun c s (acc, funs) -> 
                                    let alph = Data.get_alphabet data c in
                                    let funs =
                                        if All_sets.IntegerMap.mem c funs then funs
                                        else 
                                            let tcm = Data.get_sequence_tcm c data in
                                            All_sets.IntegerMap.add c 
                                            (analyze_tcm tcm alph) funs
                                    in
                                    let res = preprocess_sequence alph s in
                                    (c, (res, s)) :: acc, funs) 
                                s acc) 
                        ([], All_sets.IntegerMap.empty) sequence 
                    in
                    let clas, res, encf = 
                        List.fold_left 
                        (fun (_, acc, acc2) (code, (is_missing, s)) -> 
                            let clas, to_parser, to_encoding = 
                                All_sets.IntegerMap.find code transform_functions 
                            in
                            clas,
                            (Array.fold_right (to_parser is_missing) s acc), 
                            (Array.fold_right to_encoding s acc2))
                        (AllSankoff, [], []) sequence 
                    and name = 
                        try Data.code_taxon taxcode data with
                        | Not_found -> (string_of_int taxcode) 
                    in
                    match enc with
                    | Some _ -> (res, name) :: acc, enc, clas
                    | None ->
                            (res, name) :: acc,
                                (let rec apply_map l1 l2 =
                                    match l1, l2 with
                                    | f1 :: t1, it2 :: t2 ->
                                            (f1 it2) :: (apply_map t1 t2)
                                    | [], [] -> []
                                    | _, _ -> failwith "Not matching numbers?"
                                in
                                Some encf), clas
                in
                (match List.fold_left process_each ([], None, AllSankoff) all_taxa with
                | r, Some enc, clas -> 
                        let arr = 
                            Array.of_list enc, (List.map (fun (x, y) ->
                                Array.of_list x, y) r)
                        in
                        let (a, b) = 
                            match clas with
                            | AffinePartition (subs, gapcost, gapopening) ->
                                    (* We have to postprocess and check by
                                    * groups of three whether or not we have a
                                    * gap opening indeed *)
                                    post_process_affine_gap_cost subs gapcost
                                    gapopening arr
                            | _ -> arr
                        in
                        a, b, []
                | [], _, _ -> [||], [], []
                | _, None, _ -> failwith "How is this possible?")
        | _ -> failwith "ImpliedAlignment.ia_to_parser_compatible"

    let update_ia_encodings (encs, species, trees) =
        let add_states int acc =
            match int with
            | Parser.Unordered_Character (int, _) ->
                    let a = 1 land int
                    and b = 2 land int 
                    and c = 4 land int 
                    and d = 8 land int 
                    and e = 16 land int in
                    let addit acc x =
                        if x <> 0 then All_sets.Integers.add x acc
                        else acc
                    in
                    addit (addit (addit (addit (addit acc a) b) c) d) e
            | _ -> failwith "Unordered here?"
        in
        let arr = Array.of_list species in
        let arr = Array.map (fun (a, _) -> a) arr in
        let updater pos enc = 
            if Parser.Hennig.Encoding.is_sankoff enc ||
                Parser.Hennig.Encoding.is_ordered enc then enc
            else 
                let ns = Array.fold_left (fun acc taxon ->
                    add_states taxon.(pos) acc) All_sets.Integers.empty 
                    arr 
                in
                Parser.Hennig.Encoding.set_set enc ns
        in
        let arr = Array.mapi updater encs in
        arr, species, trees

    let to_static_character remove_non_informative character iamtx data = 
        let st = 
            Status.create "Static Approximation" None 
            "Converting implied alignments to static characters"
        in
        let res = ia_to_parser_compatible data iamtx in
        let res = 
            if remove_non_informative then update_ia_encodings res 
            else res
        in
        Status.finished st;
        character, res


    let concat_alignment ia =
        let ia = List.map   
            (fun ia ->  List.map  
                 (fun (taxa_code, ia) ->
                      let ia = List.map 
                          (fun ali_map ->
                               Codes.map (fun (ali_ls : int array list) -> 
                                              let ali = Array.concat ali_ls in 
                                              ali 
                                         ) ali_map
                          ) ia
                                   in 
                      taxa_code, ia
                 ) ia
            ) ia
        in 
        ia


   (** (sequence code list), ( (taxon_id * (aligned_code arrays for each character
       set) list (of characters) ) list (of taxa) ) of list (of trees) *)
    let aux_create_implied_alignment filter_fn codes data tree = 
        let operate_on_tree tree =
            let filtered_trees = 
                List.map 
                    (fun x -> 
                         let alph = Data.get_alphabet data x in
                         let gap = Alphabet.get_gap alph
                         and all = Alphabet.get_all alph 
                         and tcm = Data.get_sequence_tcm x data in
                         (if 1 = Cost_matrix.Two_D.combine tcm then
                            fun x -> x land ((lnot gap) land all)
                         else fun x -> x), filter_fn tree [x]) codes
            in
            List.map of_tree filtered_trees
        in
        codes, operate_on_tree tree


   (** (sequence code list), ( (taxon_id * (aligned_code arrays for each character
       set) list (of characters) ) list (of taxa) ) of list (of trees) *)
    let create filter_fn codes data tree = 
        let codes = (* Check if the codes are sequence codes or not *) 
            List.filter (fun x -> 
                if (List.exists (fun y -> x = y) data.Data.dynamics) then true
                else begin
                    Status.user_message Status.Error
                    ("The character with code " ^ string_of_int x ^
                    " is not a sequence character. You have requested an "
                    ^ "implied alignment of such thing, I will ignore that "
                    ^ "character for the implied alignment.");
                    false
                end) codes
        in
        let _, x = aux_create_implied_alignment filter_fn codes data tree in
        x

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
                | `Random _ | `Missing _ | `All | `AllStatic | `AllDynamic as x ->
                        `Some (Data.get_chars_codes data x)
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
    
    let to_static_homologies ignore filter_fn remove_noninformative 
            (chars : Methods.characters)  data tree = 
        let codes = get_char_codes chars data in
        let all_to_add = 
            List.fold_left (fun acc code -> 
                let _, ia = 
                    aux_create_implied_alignment filter_fn [code] data tree 
                in
                assert (1 = List.length ia);
                let ia = List.hd ia in
                let name =
                    let code = 
                        incr data.Data.character_code_gen;
                        string_of_int !(data.Data.character_code_gen)
                    in
                    "ImpliedAlignment" ^ code
                in
                let ia = concat_alignment ia in 
                (to_static_character remove_noninformative name ia data) :: acc) 
            []
            codes
        in
        let d = Data.add_multiple_static_parsed_file data all_to_add in
        if ignore then Data.process_ignore_characters false d (`Some codes)
        else d

end 
