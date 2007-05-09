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

let () = SadmanOutput.register "GenAli" "$Revision: 1616 $"

let fprintf = Printf.fprintf;;


(** Given two arrays of orders [seq2] and [re_seq2], 
 * compute their rearrangement distance*)
let cmp_re_cost seq2 re_seq2 re_meth circular =     
    if Array.length seq2 = 0 then 0
    else begin
        let re_cost  = 
            match re_meth with 
            | `Inversion cost -> 
                  (UtlGrappa.cmp_inversion_dis seq2 re_seq2 circular) * cost  
            | `Breakpoint cost ->               
                  (UtlGrappa.cmp_oriented_breakpoint_dis seq2 re_seq2 circular) * cost   
        in  
        re_cost 
    end 


(** Given two sequences [seq1], [seq2] and rearranged sequence 
 * [re_seq2] of [seq2], compute the total cost between [seq1] and [re_seq2] where 
 * total cost = editing cost (seq1, re_seq2) plus rearrangement cost (seq2, re_seq2) *)
let cmp_cost seq1 seq2 re_seq2 gen_cost_mat gap re_meth circular = 

    let alied_seq1, alied_seq2, editing_cost =  
        Utl.create_pair_align seq1 re_seq2 gen_cost_mat gap
    in   

(*  
    let alied_seq1, alied_seq2, editing_cost =  
    UtlPoy.create_general_ali seq1 re_seq2 gap gen_cost_mat 
    in   
*)

    let re_cost = cmp_re_cost seq2 re_seq2 re_meth circular in 

    (editing_cost + re_cost), re_cost, alied_seq1, alied_seq2



(** Given two sequences [seq1], [seq2], find rearranged sequence [re_seq2]
 * of sequence [seq2] using stepwise addition method 
 * such that the total cost is minimum where 
 * total cost = editing cost (seq1, re_seq2) plus rearrangement cost (seq2, re_seq2) *)
let find_wagner_ali seq1 seq2 gen_cost_mat gap re_meth circular = 
    let rec add (best_wagner_seq2 : int array) added_seq2_ls rem_seq2_ls = 
        match rem_seq2_ls with
        | [] -> best_wagner_seq2 
        | code2 :: tl ->
              let added_seq2_ls = added_seq2_ls @ [code2] in 

              let wagner_cost = ref Utl.infinity in 
              let wagner_seq2 = ref [||] in 
              for pos = 0 to Array.length best_wagner_seq2 do 
                  let partial_seq2 = Utl.insert best_wagner_seq2 pos code2 in 

                  let cost, _, _, _  = 
                      cmp_cost seq1 (Array.of_list added_seq2_ls) partial_seq2 
                          gen_cost_mat gap re_meth circular
                  in

                  if cost < !wagner_cost then begin
                      wagner_cost := cost; 
                      wagner_seq2 := partial_seq2 
                  end 
              done;              
              add !wagner_seq2 added_seq2_ls tl
    in
              
    let wagner_seq2 = add [||] [] (Array.to_list seq2) in  
    wagner_seq2


(** Given two sequences [seq1], [seq2] and rearranged sequence 
 * [re_seq2] of [seq2], swap [re_seq2] in order to minimize the total
 * cost between [seq1] and [re_seq2] where 
 * total cost = editing cost (seq1, re_seq2) plus rearrangement cost (seq2, re_seq2) *)
let rec multi_swap_locus seq1 seq2 best_seq2 best_cost 
        gen_cost_mat gap re_meth max_swap_med
        circular num_done_swap =             

    let len2 = Array.length best_seq2 in  
    let swap_ls = ref [] in 

    for donor_pos = 0 to len2 - 2 do
        for rev_pos = donor_pos + 1 to len2 - 1 do 
            let new_seq2 = Utl.swap_item donor_pos rev_pos best_seq2 in 

            let new_cost, _, _, _ = cmp_cost seq1 seq2 new_seq2 
                gen_cost_mat gap re_meth circular
            in                 
            if new_cost < best_cost then 
                swap_ls := (donor_pos, rev_pos, new_cost)::!swap_ls
       done 
    done;

    let swap_ls = 
        List.sort (fun (_, _, s1) (_, _, s2) -> compare s1 s2) !swap_ls
    in 
    let is_depend (s1, e1, _) (s2, e2, _) = 
        if (s1 > e2) || (e1 < s2) 
            || ( (s1 > s2) && (e1 < e2) ) 
              ||( (s2 > s1) && (e2 < s1) )  then false
        else true
    in 
    let ind_swap_ls = List.fold_left 
        (fun ind_swap_ls swap ->
             if List.exists (fun swap2 -> is_depend swap swap2) ind_swap_ls 
             then ind_swap_ls 
             else swap::ind_swap_ls) [] swap_ls 
    in 
                     
    let ind_swap_arr = Array.of_list (List. rev ind_swap_ls) in 

    let rec swap num_swap = 
        let new_seq2 = Array.copy best_seq2 in
        for pos = 0 to num_swap - 1 do 
            let sta, en, _ = ind_swap_arr.(pos) in 
            let tmp = new_seq2.(sta) in
            new_seq2.(sta) <- new_seq2.(en);
            new_seq2.(en) <- tmp;
        done;
            
        let new_cost, _, _, _ = cmp_cost seq1 seq2 new_seq2 
            gen_cost_mat gap re_meth circular
        in                  
        if new_cost < best_cost then begin
            new_cost, new_seq2
        end 
        else swap (num_swap  / 2)
    in 
    
    let num_swap = Array.length ind_swap_arr in 
    if (num_swap = 0) then  best_cost, best_seq2
    else begin
        let new_cost, new_seq2 = swap num_swap in 
        if num_done_swap + 1 >=  max_swap_med then new_cost, new_seq2 
        else 
            multi_swap_locus seq1 seq2 new_seq2 new_cost gen_cost_mat gap 
                re_meth max_swap_med circular (num_done_swap + 1)
    end 

        
(** Given two sequences [seq1] and [seq2] of general character, create 
 * the general alignment between [seq1] and [seq2] with minimum total cost 
 * where total cost = editing cost + rearrangement cost *)
let create_gen_ali (seq1 : Sequence.s) (seq2 : Sequence.s) 
        (gen_cost_mat : Cost_matrix.Two_D.m) 
        (pure_gen_cost_mat : int array array) alpha re_meth 
        max_swap_med circular =

    let gap = Alphabet.get_gap alpha in 
    let seq1 : int array = Sequence.to_array seq1 in 
    let seq2 : int array = Sequence.to_array seq2 in 

    let gen_cost_mat = pure_gen_cost_mat in 

    let wag_seq2 = find_wagner_ali seq1 seq2 gen_cost_mat 
        gap re_meth circular
    in 

    let init_cost, re_cost, alied_seq1, alied_seq2 = 
        cmp_cost seq1 seq2 wag_seq2 gen_cost_mat gap re_meth circular
    in 

    let _, best_seq2 = 
        match max_swap_med with 
        | 0 -> init_cost, wag_seq2
        | _ -> multi_swap_locus seq1 seq2 wag_seq2 init_cost  
              gen_cost_mat gap re_meth max_swap_med circular 0  
    in   

    let final_cost, re_cost, alied_seq1, alied_seq2 =   
        cmp_cost seq1 seq2 best_seq2 gen_cost_mat gap re_meth circular 
    in   

    let ali_len = Array.length alied_seq1 in 
    let alied_seq1 = Sequence.init (fun idx -> alied_seq1.(idx)) ali_len in
    let alied_seq2 = Sequence.init (fun idx -> alied_seq2.(idx)) ali_len in
    final_cost, re_cost, alied_seq1, alied_seq2  



(** Given two sequence [seq1] and [seq2] of character codes, create 
 * the general alignment between [seq1] and [seq2] with minimum total cost 
 * where total cost = editing cost + rearrangement cost *)
let create_gen_ali_code (seq1 : int array) (seq2 : int array) 
        (gen_cost_mat : int array array) gen_gap_code 
        re_meth max_swap_med circular =
    let wag_seq2 = find_wagner_ali seq1 seq2 gen_cost_mat 
        gen_gap_code re_meth circular
    in 


    let init_cost, re_cost, alied_seq1, alied_seq2 =  
        cmp_cost seq1 seq2 wag_seq2 gen_cost_mat gen_gap_code re_meth circular
    in 


    let _, best_seq2 = 
        match max_swap_med with 
        | 0 -> init_cost, wag_seq2
        | _ ->
              multi_swap_locus seq1 seq2 wag_seq2 init_cost  
                  gen_cost_mat gen_gap_code re_meth max_swap_med circular 0  
    in   
    


    let final_cost, re_cost, alied_seq1, alied_seq2 =   
        cmp_cost seq1 seq2 best_seq2 gen_cost_mat 
            gen_gap_code re_meth circular 
    in   

    final_cost, re_cost, alied_seq1, alied_seq2  

