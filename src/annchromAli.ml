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

let () = SadmanOutput.register "AnnchromAli" "$Revision: 911 $"

(** The implementation of funtions to calculate the cost, alignments and medians
    between annotated chromosomes where both point mutations and rearrangement operations
    are considered *)
let fprintf = Printf.fprintf

type seq_t = {
    seq : Sequence.s;
    seq_ref_code : int;
    seq_ord1 : int;
    seq_ord2 : int;

}

type annchrom_t = {
    seq_arr : seq_t array; 
    ref_code : int; 
    ref_code1 : int;    (** Child's code *)    
    ref_code2 : int;  (** Child's code *)
    cost1 : int;
    recost1 : int;
    cost2 : int;
    recost2 : int;
}

(** Parameters used to align two general character sequences *)
type annchromPam_t = {
    re_meth : Data.re_meth_t;
    keep_median : int;
    circular : int;
    swap_med : int;
    locus_indel_cost : (int * int);
}

let annchromPam_default = {
    re_meth = `Breakpoint 10;
    keep_median = 1;
    circular = 0;
    swap_med = 1;
    locus_indel_cost = (10, 100);
}


let init seq_arr = 
    {
        seq_arr = seq_arr;
        ref_code = Utl.get_new_chrom_ref_code ();
        ref_code1 = -1;
        ref_code2 = -1;
        cost1 = 0;
        cost2 = 0;
        recost1 = 0;
        recost2 = 0;
    }

let get_annchrom_pam user_annchrom_pam = 
    let chrom_pam = annchromPam_default in  
    let chrom_pam = 
        match user_annchrom_pam.Data.re_meth with
        | None -> chrom_pam
        | Some re_meth -> {chrom_pam with re_meth = re_meth}
    in 

    let chrom_pam = 
        match user_annchrom_pam.Data.circular with
        | None -> chrom_pam
        | Some circular -> {chrom_pam with circular = circular}
    in 

    let chrom_pam = 
        match user_annchrom_pam.Data.keep_median with
        | None -> chrom_pam
        | Some keep_median -> {chrom_pam with keep_median = keep_median}
    in 
    
    let chrom_pam = 
        match user_annchrom_pam.Data.swap_med with
        | None -> chrom_pam
        | Some swap_med -> {chrom_pam with swap_med = swap_med}
    in 


    let chrom_pam = 
        match user_annchrom_pam.Data.locus_indel_cost with
        | None -> chrom_pam
        | Some locus_indel_cost -> {chrom_pam with locus_indel_cost = locus_indel_cost}
    in 
    chrom_pam

let print annchrom alpha = 
    Array.iter (fun s -> 
                    let seq = Sequence.to_string s.seq alpha in 
                    fprintf stdout "(%i, %s) | " s.seq_ref_code seq) annchrom.seq_arr;
    print_newline ()

let split chrom =  
    let seq_arr = Array.map (fun s -> s.seq) chrom.seq_arr in  
    let code_arr = Array.map (fun s -> s.seq_ref_code) chrom.seq_arr in  
    seq_arr, code_arr 



(** Given two arrays of sequences [seq1_arr] and [seq2_arr], 
 *  create the general cost matrix and corresponding code arrays  *)
let create_pure_gen_cost_mat seq1_arr seq2_arr cost_mat ali_pam =

    let seq1_arr = Array.mapi (fun ith seq -> seq, (ith * 2 + 1) ) seq1_arr in 

    let len1 = Array.length seq1_arr in 

    let seq2_arr = Array.mapi 
        (fun ith seq -> seq, (ith * 2 + 1 + len1 * 2) ) seq2_arr 
    in 

    let len2 = Array.length seq2_arr in 
    let gen_gap_code = (len1 + len2) * 2 + 1 in 


    let pure_gen_cost_mat = Array.make_matrix (gen_gap_code + 1) (gen_gap_code + 1)
        Utl.infinity 
    in 

    pure_gen_cost_mat.(gen_gap_code).(gen_gap_code) <- 0;
    
    let update (seq1, code1) (seq2, code2) =
        let _, _, cost, _ = UtlPoy.align2 seq1 seq2 cost_mat in 
        pure_gen_cost_mat.(code1).(code2) <- cost;
        pure_gen_cost_mat.(code2).(code1) <- cost;
    in 

        
    Array.iter (fun (seq1, code1) ->
                    Array.iter (fun (seq2, code2) -> 
                                    update (seq1, code1) (seq2, code2)
                               ) seq2_arr) seq1_arr;


    let update_gap (seq, code) = 
        let o,e = ali_pam.locus_indel_cost  in
        pure_gen_cost_mat.(gen_gap_code).(code) <- o + e * (Sequence.length seq) / 100;
        pure_gen_cost_mat.(code).(gen_gap_code) <- pure_gen_cost_mat.(gen_gap_code).(code);
    in 
    Array.iter update_gap seq1_arr;
    Array.iter update_gap seq2_arr;


    let code1_arr = Array.map (fun (seq, code) -> code) seq1_arr in 
    let code2_arr = Array.map (fun (seq, code) -> code) seq2_arr in 


    pure_gen_cost_mat, code1_arr, code2_arr, gen_gap_code



(** Given two annotated chromosomes [chrom1] and [chrom2], compute 
 * the total cost between them which is comprised of editing cost and 
 * rearrangement cost *)
let cmp_cost (chrom1: annchrom_t) (chrom2 : annchrom_t) 
        cost_mat (pure_gen_cost_mat, gen_gap_code) alpha annchrom_pam = 


    let chrom_len1 = Array.fold_left (fun len s -> len + Sequence.length s.seq) 0 chrom1.seq_arr in     
    let chrom_len2 = Array.fold_left (fun len s -> len + Sequence.length s.seq) 0 chrom2.seq_arr in 
    
    if (chrom_len1 < 2) || (chrom_len2 < 2) then 0, 0
    else begin
            
        let ali_pam = get_annchrom_pam annchrom_pam in     
    
        let seq1_arr, _ = split chrom1 in  
        let seq2_arr, _ = split chrom2 in  
    
        let pure_gen_cost_mat, code1_arr, code2_arr, gen_gap_code = 
            create_pure_gen_cost_mat seq1_arr seq2_arr cost_mat ali_pam  
        in 
    
        let total_cost, recost, _, _ = 
            GenAli.create_gen_ali_code  code1_arr code2_arr 
                pure_gen_cost_mat gen_gap_code  
                ali_pam.re_meth ali_pam.swap_med 
                ali_pam.circular  
        in 
    (*
        print chrom1 alpha;
        print chrom2 alpha;
        fprintf stdout "Cost: %i\n" total_cost;
        print_newline ();
    *)
        total_cost, recost
    end 




(** Given two annotated chromosomes [chrom1] and [chrom2], 
 * find all median chromoromes between [chrom1] and [chrom2]. 
 * Rearrangements are allowed *) 
let find_med2_ls (chrom1: annchrom_t) (chrom2 : annchrom_t) 
        (cost_mat : Cost_matrix.Two_D.m) 
        (pure_gen_cost_mat, gen_gap_code) alpha annchrom_pam = 
    

    let chrom_len1 = Array.fold_left (fun len s -> len + Sequence.length s.seq) 0 chrom1.seq_arr in     
    let chrom_len2 = Array.fold_left (fun len s -> len + Sequence.length s.seq) 0 chrom2.seq_arr in 
    
    if (chrom_len1 < 2) then 0,0, [chrom2]
    else if chrom_len2 < 2 then 0,0, [chrom1]
    else begin    
        let ali_pam = get_annchrom_pam annchrom_pam in         
        let seq1_arr, _ = split chrom1 in  
        let seq2_arr, _ = split chrom2 in    
        
        let pure_gen_cost_mat, code1_arr, code2_arr, gen_gap_code = 
            create_pure_gen_cost_mat seq1_arr seq2_arr cost_mat ali_pam  
        in 
    

    
        let total_cost, recost, alied_code1_arr, alied_code2_arr = 
            GenAli.create_gen_ali_code  code1_arr code2_arr 
                pure_gen_cost_mat gen_gap_code  
                ali_pam.re_meth ali_pam.swap_med 
                ali_pam.circular  
        in 
   
    
        let ali_len = Array.length alied_code1_arr in 
    
        let ali_chrom = Array.init ali_len   
            (fun idx ->   
                 let idx1 = Utl.find_index code1_arr alied_code1_arr.(idx) compare in   
                 let seq1, code1 =  
                     match idx1 with  
                     | -1 -> None, -1    
                     | _ -> Some chrom1.seq_arr.(idx1).seq, chrom1.seq_arr.(idx1).seq_ref_code       
                 in                             
    
                 let idx2 = Utl.find_index code2_arr alied_code2_arr.(idx) compare in  
                 let seq2, code2 = 
                     match idx2 with 
                     | -1 -> None, -1   
                     | _ -> Some chrom2.seq_arr.(idx2).seq, chrom2.seq_arr.(idx2).seq_ref_code   
                 in                            
                     
                 let med_seq =
                     match seq1, seq2 with   
                     | Some seq1, Some seq2 -> 
                           let med, _ = UtlPoy.create_median seq1 seq2 cost_mat  in
                           med
                     | Some seq1, None -> UtlPoy.create_median_gap seq1 cost_mat
                     | None, Some seq2 -> UtlPoy.create_median_gap seq2 cost_mat
                     | _, _ -> UtlPoy.get_empty_seq ()
                 in 
                 (med_seq, idx1, idx2)
            )
        in 
    
    
    
        let create_dynamic_med (re_seq2, recost1, recost2) = 
            let rec follow index_ls index =
                if (index = ali_len) ||  
                    (alied_code2_arr.(index) != gen_gap_code) then index_ls  
                else follow (index::index_ls) (index + 1)
            in  
    
            let get_index re_seq2 = 
                let start_index_ls = follow [] 0 in
                let rev_index_ls = 
                    Array.fold_left  
                    (fun index_ls code2 ->
                         let index = Utl.find_index alied_code2_arr (abs code2) compare in
                         
                         match code2 > 0 with   
                         | true -> follow (index::index_ls) (index + 1)
                         | false -> follow (-index::index_ls) (index + 1)                     
                    ) start_index_ls re_seq2
                in 
                List.rev rev_index_ls
            in 
                                     
            let index_ls = get_index re_seq2 in         
    
            let med = 
                List.fold_right  
                    (fun index med ->                     
                         let seq, seq_ord1, seq_ord2 =  ali_chrom.(index) in
                         let seq = UtlPoy.delete_gap seq in 
                         match Sequence.length seq with  
                         | 0 -> med
                         | _ -> 
                               {seq=seq; 
                                seq_ref_code = Utl.get_new_seq_ref_code(); 
                                seq_ord1 = seq_ord1;
                                seq_ord2 = seq_ord2}::med
                    ) index_ls []
            in   
            {seq_arr = Array.of_list med; 
             ref_code = Utl.get_new_chrom_ref_code ();
             ref_code1 = chrom1.ref_code;
             ref_code2 = chrom2.ref_code;
             cost1 = total_cost;
             cost2 = total_cost;
             recost1 = recost2;
             recost2 = recost2;
            }
        in  
    
    
        let re_code2_arr = Utl.filterArr alied_code2_arr  
            (fun code2 -> code2 != gen_gap_code)  
        in   
             
        let all_order_ls =   
            if (Utl.equalArr code2_arr re_code2_arr compare) ||  
                (ali_pam.keep_median = 1) then [code2_arr, 0, recost]   
            else [(code2_arr, 0, recost); (re_code2_arr, recost, 0)]   
        in   
    

        let med_ls =  List.fold_left   
            (fun med_ls (re_seq2, recost1, recost2)  ->   
                 let med = create_dynamic_med (re_seq2, recost1, recost2) in 
                 med::med_ls  
            ) [] all_order_ls     
        in 
    
        total_cost, recost, med_ls           
    end

(** Given two annotated chromosomes [chrom1] and [chrom2], 
 * compare chrom1 and chrom2 *)
let compare annchrom1 annchrom2 = 
    
    let seq1_arr, _ = split annchrom1 in 
    let seq2_arr, _ = split annchrom2 in 
    let len1 = Array.length seq1_arr in
    let len2 = Array.length seq2_arr in 
    match len1 = len2 with
    | false -> len1 - len2
    | true ->
          let rec compare_seq index = 
              if index = len1 then 0 
              else begin 
                  let cmp = Sequence.compare seq1_arr.(index) seq2_arr.(index) in
                  match cmp with 
                  | 0 -> compare_seq (index + 1)
                  | _ -> cmp
              end 
          in 
          compare_seq 0



(** Assign ids for all sequenes of this chromosome *)
let assign_seq_ref annchrom seq_ref_code = 
    let seq_ls, seq_id = Array.fold_right 
        (fun seq (annchrom, seq_id) ->
             match seq.seq_ref_code with 
             | -1 ->
                   let seq = {seq with seq_ref_code = seq_ref_code} in 
                   (seq::annchrom), (seq_ref_code + 1) 
             | _ -> (seq::annchrom), seq_ref_code
        ) annchrom.seq_arr ([], seq_ref_code)
    in 
    let seq_arr = Array.of_list seq_ls in 
    {annchrom with seq_arr = seq_arr}, seq_ref_code



let create_map med child_ref = 
    let str = string_of_int in  


    let seq_arr = Array.mapi 
        (fun med_id m ->
             let p_ref_code, p_seq_ord = (str med.ref_code), (str med_id) in

             let c_ref_code, c_seq_ord = 
                 match child_ref = med.ref_code1 with
                 | true -> (str med.ref_code1), (str m.seq_ord1)
                 | false -> (str med.ref_code2), (str m.seq_ord2)
             in 
             let attributes = [(Tags.GenomeMap.ref_code, p_ref_code);
                               (Tags.GenomeMap.seq_order, p_seq_ord);
                               (Tags.GenomeMap.dir_seg, "`Positive");
                               (Tags.GenomeMap.ref_code, c_ref_code); 
                               (Tags.GenomeMap.seq_order, c_seq_ord);
                               (Tags.GenomeMap.dir_seg, "`Positive")
                              ] 
             in 
             let m : Tags.output = (Tags.GenomeMap.seg, attributes, `String "") in 
             `Single m
        ) med.seq_arr
    in 


    let chrom_map : Tags.output = 
        (Tags.GenomeMap.chrom, [], `Structured (`Set  (Array.to_list seq_arr))) 
    in 

    match child_ref = med.ref_code1 with
    | true -> med.cost1, med.recost1, chrom_map
    | false -> med.cost2, med.recost2, chrom_map
    

let to_formater med alph = 
    let seq_str_arr = 
        Array.map (fun seg -> Sequence.to_formater seg.seq alph) med.seq_arr 
    in 
    String.concat "|" (Array.to_list seq_str_arr)
