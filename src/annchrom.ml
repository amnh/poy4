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

let () = SadmanOutput.register "Annchrom" "$Revision: 911 $"

(** Median module contains functions to create medians
    between two lists of annotated chromosomes *)
let fprintf = Printf.fprintf

type annchrom_t = AnnchromAli.annchrom_t


(** A list medians between two annotated chromosomes. 
 * Rearrangements are allowed *)
type meds_t = {
    med_ls : annchrom_t list;
    num_med : int;
(** total_cost = editing cost + rearrangement cost *)
    total_cost : int;   
    total_recost : int;
    annchrom_pam : Data.dyna_pam_t;    
    cost_mat : Cost_matrix.Two_D.m;
    alpha : Alphabet.a
}


let init_med (seq_arr : (Sequence.s Data.seq_t) array) 
        cost_mat alpha annchrom_pam = 

    let med = AnnchromAli.init 
        (Array.map (fun s -> s.Data.seq, s.Data.code) seq_arr) in 

    {
        med_ls = [med];
        num_med = 1;
        total_cost = 0;
        total_recost = 0;
        annchrom_pam = annchrom_pam;
        cost_mat = cost_mat;

        alpha = alpha;
    }



let rec keep chrom_pam med_ls = 
    match chrom_pam.Data.keep_median with 
    | None -> med_ls 
    | Some keep_median ->
          if  keep_median >= List.length med_ls then med_ls
          else Utl.get_k_random_elem med_ls keep_median



(** Given two lists of medians [meds1=(x1,...,xk)] and [meds2=(y1,...,yt)]
 * where xi and yj are medians. For each pair (xi, yj) we have 
 * a list of medians z_ij with the same cost c_ij. 
 * Find z*_ij = minargv(z_ij )(c_ij) *)
let find_meds2 ?(keep_all_meds=false) (meds1 : meds_t) (meds2 : meds_t) = 

    let update (med1: annchrom_t) (med2 : annchrom_t) (best_meds : meds_t) =
        let cost, recost, med_ls =   
            AnnchromAli.find_med2_ls med1 med2 meds1.cost_mat  
                meds1.alpha meds1.annchrom_pam   
        in   

        if cost < best_meds.total_cost then   
            {best_meds with  
                 total_cost = cost;  
                 total_recost = recost;
                 med_ls = med_ls;   
                 num_med = List.length med_ls} 
        else best_meds
    in 
                            
        
    let init_meds : meds_t = {
        med_ls = []; num_med = 0;  
        total_cost = max_int;
        total_recost = 0;
        annchrom_pam = meds1.annchrom_pam; 
        cost_mat = meds1.cost_mat; 


        alpha = meds1.alpha}  
    in 

    let best_meds = 
        List.fold_left (fun best_meds1 med1 ->                                
                            List.fold_left (fun best_meds2 med2 ->
                                 update med1 med2 best_meds2 
                            ) best_meds1 meds2.med_ls 
                   ) init_meds meds1.med_ls
    in 
    
    match keep_all_meds with
    | true -> best_meds
    | false ->             
          let kept_med_ls = keep meds1.annchrom_pam best_meds.med_ls in 
          {best_meds with   
               med_ls = kept_med_ls;          
               num_med = List.length kept_med_ls}  

    

(** Given three lists of medians [medsp=(x1,...,xk)], [meds1=(y1,...,yt)]
 * and [meds2=(z1,...,zq)] where xi, yj, and zp are medians. 
 * For each triplet (xi, yj, zp) we have 
 * a list of medians w_ijp with the same cost c_ijp. 
 * Find w*ijp = minargv_(w_ijp) (c_ijp) *)
let find_meds3 (medsp: meds_t) (meds1: meds_t) (meds2: meds_t) =
    let meds1p = find_meds2 meds1 medsp in 
    let meds2p = find_meds2 meds2 medsp in 
    if meds1p.total_cost < meds2p.total_cost then meds1p
    else meds2p
            
       
(** Given two lists of medians [meds1=(x1,...,xk)] and [meds2=(y1,...,yt)]
 * where xi and yj are medians. For each pair (xi, yj) we have 
 * a list of medians z_ij with the same cost c_ij. 
 * Find c*_ij = min (c_ij) *)
let cmp_min_pair_cost (meds1 : meds_t) (meds2 : meds_t) =    
    let min_cost, min_recost = List.fold_left 
        (fun (min_cost, min_recost) med1 -> 
                List.fold_left 
                    (fun (min_cost2, min_recost2) med2 -> 
                         let cost, recost = AnnchromAli.cmp_cost med1 med2
                                meds1.cost_mat 
                                meds1.alpha meds1.annchrom_pam 
                         in  
                         if min_cost2 >cost then cost, recost
                         else min_cost2, min_recost2
                    ) (min_cost, min_recost) meds2.med_ls
        ) (Utl.infinity, 0) meds1.med_ls 
    in 
    min_cost, min_recost


(** Given two lists of medians [meds1=(x1,...,xk)] and [meds2=(y1,...,yt)]
 * where xi and yj are medians. For each pair (xi, yj) we have 
 * a list of medians z_ij with the same cost c_ij. 
 * Find c*_ij = max (c_ij) *)
let cmp_max_pair_cost (meds1 : meds_t) (meds2 : meds_t) =    
    let max_cost, max_recost = List.fold_left 
        (fun (max_cost, max_recost) med1 -> 
                List.fold_left 
                    (fun (max_cost2, max_recost2) med2 -> 
                         let cost, recost = AnnchromAli.cmp_cost med1 med2
                                meds1.cost_mat 
                                meds1.alpha meds1.annchrom_pam 
                         in  
                         if max_cost2 <cost then cost, recost
                         else max_cost2, max_recost2
                    ) (max_cost, max_recost) meds2.med_ls
        ) (0, 0) meds1.med_ls 
    in 
    max_cost, max_recost

(** Compare two list of medians *)
let compare (meds1 : meds_t) (meds2 : meds_t) = 
    let num_med1 = meds1.num_med in 
    let num_med2 = meds2.num_med in 
    match num_med1 != num_med2 with
    | true -> num_med1 - num_med2  
    | false -> 
        let rec compare_meds med1_ls med2_ls = 
            match med1_ls, med2_ls with 
                | med1::tl1, med2::tl2 ->
                    if AnnchromAli.compare med1 med2 = 0 then compare_meds tl1 tl2
                    else AnnchromAli.compare med1 med2
                | _ -> 0
        in  
        compare_meds meds1.med_ls meds2.med_ls 
    


let to_string (med : annchrom_t) alpha = 
    let seq_arr = Array.map 
        (fun s -> 
             let seq = Sequence.to_string s.AnnchromAli.seq alpha in 
             let ref_code = string_of_int s.AnnchromAli.seq_ref_code in 
             "(" ^ seq ^ ", " ^ ref_code ^ ")"                              
        ) med.AnnchromAli.seq_arr 
    in  
    String.concat " | " (Array.to_list seq_arr)



let get_active_ref_code meds = 
    let med = List.hd meds.med_ls in
    med.AnnchromAli.ref_code, med.AnnchromAli.ref_code1, med.AnnchromAli.ref_code2
