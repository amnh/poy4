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

let () = SadmanOutput.register "AliMap" "$Revision: 1644 $"
(** The implementation of functions to find the map between two chromosomes *)

type chromPairAliPam_t = ChromPam.chromPairAliPam_t
type block_pam_t = Block.blockPam_t
type seed_t = Seed.seed_t
type block_t = Block.block_t
type order_t = ChromPam.order_t
type subseq_t = Subseq.subseq_t

let fprintf = Printf.fprintf



(** Create the the general cost matrix. Each subseq is considered as a state *)
let create_gen_cost_mat subseq1_ls subseq2_ls global_map gen_gap_code 
        seq1 seq2 cost_mat ali_pam =

    let len1 = List.length subseq1_ls in 
    let len2 = List.length subseq2_ls in 
    let len = len1 + len2 + 1 in 

    let gen_cost_mat = Array.make_matrix len len Utl.infinity in 

    let set_cost code1 code2 cost = gen_cost_mat.(code1).(code2) <- cost in 

    set_cost gen_gap_code gen_gap_code 0;

    let num_block = List.length global_map in 

    let block_gap_cost = Utl.infinity / (num_block + 1) in 
    List.iter (fun b -> 
       let subseq1_id = b.Block.subseq1_id in 
       let subseq2_id = b.Block.subseq2_id in 
       let cost = b.Block.cost in 
       set_cost subseq1_id subseq2_id cost;
       set_cost subseq2_id subseq1_id cost;

       set_cost subseq1_id gen_gap_code block_gap_cost; 
       set_cost gen_gap_code subseq1_id block_gap_cost;

       set_cost subseq2_id gen_gap_code block_gap_cost; 
       set_cost gen_gap_code subseq2_id block_gap_cost;
              ) global_map;


    let del_subseq1_ls = List.filter 
        (fun subseq -> List.length subseq.Subseq.block_id_ls = 0) subseq1_ls in 

    let del_subseq2_ls = List.filter 
        (fun subseq -> List.length subseq.Subseq.block_id_ls = 0) subseq2_ls in 

    let empty_seq = UtlPoy.get_empty_seq () in 

    let ali_mat = Array.make_matrix len len (empty_seq, empty_seq) in 

    let pair_gap subseq seq = 
       let id = subseq.Subseq.id in 
       let del_cost = 
           let o, e = ali_pam.ChromPam.locus_indel_cost in
           let len = Subseq.get_len subseq in  
           o + (e * (len - 1)) / 100 
       in
                    
       set_cost id gen_gap_code  del_cost;
       set_cost gen_gap_code id  del_cost;
    in

    List.iter (fun subseq -> pair_gap subseq seq1) del_subseq1_ls;
    List.iter (fun subseq -> pair_gap subseq seq2) del_subseq2_ls;


    List.iter 
        (fun subseq1 ->
             let id1 = subseq1.Subseq.id in 
             let s1 = subseq1.Subseq.sta in 
             let e1 = subseq1.Subseq.en in 
	         let subseq1 = Sequence.sub seq1 s1 (e1 - s1 + 1) in  
             let num_nu1 = UtlPoy.cmp_num_DNA subseq1 in  

             List.iter 
                 (fun subseq2 ->
                      let id2 = subseq2.Subseq.id in 
                      let s2 = subseq2.Subseq.sta in 
                      let e2 = subseq2.Subseq.en in 	              
	                  let subseq2 = Sequence.sub seq2 s2 (e2 - s2 + 1) in 
                      let num_nu2 = UtlPoy.cmp_num_DNA subseq2 in 


                      let alied_seq1, alied_seq2, cost, _ = 
                          match abs (num_nu1 - num_nu2) * 2 >=
                              gen_cost_mat.(id1).(gen_gap_code) + gen_cost_mat.(id2).(gen_gap_code) with 
                              | true -> empty_seq, empty_seq, Utl.infinity, 0
                              | false -> UtlPoy.align2 subseq1 subseq2 cost_mat 
                      in

                      set_cost id1 id2  cost;          
                      set_cost id2 id1  cost;    

                      ali_mat.(id1).(id2) <- (alied_seq1, alied_seq2);
                      ali_mat.(id2).(id1) <- (alied_seq2, alied_seq1)

                 ) del_subseq2_ls
        ) del_subseq1_ls;

    

    gen_cost_mat, ali_mat

    
(** Given a global map between two chromosomes. 
    Create globally general alignment between two *)
let create_general_ali global_map seq1 seq2 cost_mat ali_pam =

    let global_map, subseq1_ls, subseq2_ls = 
        Block.create_subseq_id `Both global_map ali_pam in
(*
    print_endline "Global map";
      List.iter Block.print global_map; print_newline (); 
*)

    let len1 = List.length subseq1_ls in 
    let len2 = List.length subseq2_ls in 
(*    fprintf stdout "(len1: %i, len2: %i)" len1 len2; flush stdout;*)

    List.iter (fun sq -> sq.Subseq.id <- sq.Subseq.id + len1) subseq2_ls;    
    List.iter (fun b -> b.Block.subseq2_id <- b.Block.subseq2_id + len1) global_map;



    let gen_gap_code = 0 in     
    let gen_cost_mat, ali_mat = create_gen_cost_mat subseq1_ls subseq2_ls 
        global_map gen_gap_code seq1 seq2 cost_mat ali_pam in 

    let gen_seq1 = Array.init len1 (fun index -> index + 1) in 
    let gen_seq2 = Array.init len2 (fun index -> index + len1 + 1) in 
    

    let circular = ali_pam.ChromPam.circular in
    let cmp_partial_cost partial_gen_seq2 = 

        let _, _, editing_cost = 
            Utl.create_pair_align gen_seq1 partial_gen_seq2 
                gen_cost_mat gen_gap_code in 


        let recost  = match ali_pam.ChromPam.re_meth with
        | `Inversion cost -> 
              (UtlGrappa.cmp_self_inversion_dis partial_gen_seq2 circular) * cost
        | `Breakpoint cost -> 
              (UtlGrappa.cmp_self_oriented_breakpoint_dis partial_gen_seq2 circular) * cost
        in 

        editing_cost + recost
    in
        

    let rec find_wagner_ali pos2 (best_wagner_seq2 : int array) = 
        match pos2 = len2 with
        | true -> best_wagner_seq2
        | false -> 
              let subseq2_id = gen_seq2.(pos2) in
              let wagner_cost = ref Utl.infinity in 
              let wagner_gen_seq2 = ref [||] in 
              for pos = 0 to Array.length best_wagner_seq2 do 
                  let partial_gen_seq2 = Utl.insert best_wagner_seq2 pos subseq2_id in 
                  let cost = cmp_partial_cost partial_gen_seq2 in
                  if cost < !wagner_cost then begin
                      wagner_cost := cost; 
                      wagner_gen_seq2 := partial_gen_seq2 
                  end 
              done;
              find_wagner_ali (pos2 + 1) !wagner_gen_seq2 
    in

    
    let rec swap_locus  best_cost best_seq2 = 
         let len2 = Array.length best_seq2 in  
         let new_best_seq2 = ref best_seq2 in   
         let new_best_cost = ref best_cost in  


         for donor_pos = 0 to len2 - 2 do
             for rev_pos = donor_pos + 1 to len2 - 1 do 
                 let new_seq2 = Utl.swap_item donor_pos rev_pos !new_best_seq2 
                 in   

                 let new_cost = cmp_partial_cost new_seq2 in     
            
                 if new_cost < !new_best_cost then begin                    
                     new_best_seq2 := new_seq2;    
                     new_best_cost := new_cost;      
                 end;   
             done;
         done; 


         if !new_best_cost < best_cost then 
             swap_locus !new_best_cost !new_best_seq2 
         else best_cost, best_seq2
    in


    let wagner_gen_seq2 = find_wagner_ali 1 [|gen_seq2.(0)|] in 


    let wagner_cost = cmp_partial_cost wagner_gen_seq2 in 



    let best_cost, best_gen_seq2 = swap_locus wagner_cost wagner_gen_seq2 in


    let alied_gen_seq1, alied_gen_seq2, cost = Utl.create_pair_align gen_seq1
        best_gen_seq2 gen_cost_mat gen_gap_code in



    subseq1_ls, subseq2_ls, global_map, ali_mat, 
    alied_gen_seq1, alied_gen_seq2, best_cost







(** Given a global map between two chromosomes. 
    Create globally general alignment between two *)
let create_fast_general_ali global_map seq1 seq2 cost_mat ali_pam =

    let global_map, subseq1_ls, subseq2_ls = 
        Block.create_subseq_id `Both global_map ali_pam 
    in

    let len1 = List.length subseq1_ls in 

    List.iter (fun sq -> sq.Subseq.id <- sq.Subseq.id + len1) subseq2_ls;    
    List.iter (fun b -> b.Block.subseq2_id <- b.Block.subseq2_id + len1) global_map;

    let gen_gap_code = 0 in     
    let gen_cost_mat, ali_mat = create_gen_cost_mat subseq1_ls subseq2_ls 
        global_map gen_gap_code seq1 seq2 cost_mat ali_pam 
    in 

    let rem_seq1 = List.fold_right 
        (fun ss rem_seq1 -> 
             if Subseq.is_free ss then ss.Subseq.id::rem_seq1
             else rem_seq1
        ) subseq1_ls []
    in 

    let rem_seq2 = List.fold_right 
        (fun ss rem_seq2 -> 
             if Subseq.is_free ss then ss.Subseq.id::rem_seq2
             else rem_seq2 
        ) subseq2_ls []
    in 

    let rem_seq1 = Array.of_list rem_seq1 in
    let rem_seq2 = Array.of_list rem_seq2 in

    let swap_med = ali_pam.ChromPam.swap_med in 
    let edit_cost, _, alied_rem_seq1, alied_rem_seq2 = GenAli.create_gen_ali_code         
        rem_seq1 rem_seq2 gen_cost_mat gen_gap_code 
        ali_pam.ChromPam.re_meth swap_med ali_pam.ChromPam.circular
    in   


    let add sta_id1 end_id1 alied_seq1 alied_seq2 = 
        let alied_seq1 = ref alied_seq1 in  
        let alied_seq2 = ref alied_seq2 in  
        for id = sta_id1 to end_id1 do 
            let b = Utl.deref (Block.find_subseq1 global_map id) in 
            alied_seq1 := !alied_seq1 @ [b.Block.subseq1_id]; 
            alied_seq2 := !alied_seq2 @ [b.Block.subseq2_id]; 
        done; 
        !alied_seq1, !alied_seq2
    in 
        
    let last_id1, alied_seq1, alied_seq2 = List.fold_left2 
        (fun (last_id1, alied_seq1, alied_seq2) id1 id2 ->
             let alied_seq1, alied_seq2 = 
                 match id1 = gen_gap_code with 
                 | true -> alied_seq1, alied_seq2
                 | false ->
                       add (last_id1 + 1) (id1 - 1) alied_seq1 alied_seq2 
             in 
             let last_id1 = max last_id1 id1 in 
             let alied_seq1 = alied_seq1 @ [id1] in  
             let alied_seq2 = alied_seq2 @ [id2] in               
             last_id1, alied_seq1, alied_seq2
        ) (0, [], []) (Array.to_list alied_rem_seq1) (Array.to_list alied_rem_seq2)
    in  



    let alied_seq1, alied_seq2 = add (last_id1 + 1) len1 alied_seq1 alied_seq2 in 
    let edit_cost = List.fold_left2 
        (fun cost id1 id2 -> cost + gen_cost_mat.(id1).(id2) ) 0 alied_seq1 alied_seq2
    in 

    let alied_seq1 = Array.of_list alied_seq1 in 
    let alied_seq2 = Array.of_list alied_seq2 in 

    let circular = ali_pam.ChromPam.circular in
    let re_seq2 = Utl.filterArray (fun id -> id > 0) alied_seq2 in 


    let recost  = match ali_pam.ChromPam.re_meth with 
    | `Inversion cost ->  
          (UtlGrappa.cmp_self_inversion_dis re_seq2 circular) * cost 
    | `Breakpoint cost ->  
          (UtlGrappa.cmp_self_oriented_breakpoint_dis re_seq2 circular) * cost 
    in  


    subseq1_ls, subseq2_ls, global_map, ali_mat, 
    alied_seq1, alied_seq2, (edit_cost + recost), recost



