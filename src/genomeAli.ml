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
let () = SadmanOutput.register "GenomeAli" "$Revision: 1480 $"

(** The implementation of funtions to calculate the cost, alignments and medians
    between chromosomes where both point mutations and rearrangement operations
    are considered *)



type seed_t = Seed.seed_t
type pairChromPam_t = ChromPam.chromPairAliPam_t
type block_t = Block.block_t
type subseq_t = Subseq.subseq_t
let  fprintf = Printf.fprintf
let  deref = Utl.deref
let  debug = false
let  gap = Alphabet.gap

type direction_t = ChromPam.direction_t

    
type seg_t = {
    sta : int;
    en : int;
    cost : int; 
    med_chrom_id : int;


    ref_code1 : int;    (** Child's code *)
    sta1 : int;
    en1 : int;
    chi1_chrom_id : int;
    alied_seq1 : Sequence.s;
    dir1 : direction_t;

    ref_code2 : int;  (** Child's code *)
    sta2 : int;
    en2 : int;
    chi2_chrom_id : int;
    alied_seq2 : Sequence.s;
    dir2 : direction_t;
}


type chrom_t = {
    chrom_id : int ref;
    chrom_ref_code : int;
    map : seg_t list;    
    seq : Sequence.s;
}

type med_t = {
    chrom_arr : chrom_t array;
    genome_ref_code : int;
}

type genome_block_t = {
    block_id : int;
    chrom1_id : int;    
    chrom2_id : int;
    block : Block.block_t;
}


let to_string med = 
    let chromStr_arr = Array.map (fun chrom -> Sequence.to_string chrom.seq Alphabet.nucleotides)
        med.chrom_arr 
    in
    String.concat "@@" (Array.to_list chromStr_arr)


let print_genome genome = 
    fprintf stdout "Genome code: %i\n" genome.genome_ref_code;
    Array.iter (fun chrom -> 
                    fprintf stdout "%i, %i:" chrom.chrom_ref_code !(chrom.chrom_id);
                    Sequence.print stdout chrom.seq Alphabet.nucleotides; 
                    fprintf stdout " @ ") genome.chrom_arr;
    print_newline ()


let get_chroms med = Array.map (fun chrom -> chrom.seq) med.chrom_arr

let chrom_map_to_string chrom_map = 
    let get_dir dir = 
        match dir with
        | `Positive -> "Positive"
        | `Negative -> "Negative"
        | _ -> ""
    in 

    let str_ls = List.map 
        (fun m -> 
             let node_str =  "Sequence (chrom_id:" ^ (string_of_int m.med_chrom_id)
                 ^ ", start:" ^ (string_of_int m.sta) 
                 ^ ", end: " ^ (string_of_int m.en) ^ ")"
             in 

             let child1_str = 
                 match m.ref_code1 = -1 with
                 | true -> ""
                 | false -> 
                       ", Child1 (genome_ref:" ^ (string_of_int m.ref_code1) 
                       ^ ", chrom_id:" ^ (string_of_int m.chi1_chrom_id) 
                       ^ ", start:" ^ (string_of_int m.sta1)  
                       ^ ", end: " ^ (string_of_int m.en1)  
                       ^ ", direction:" ^ (get_dir m.dir1) ^ ")" 
             in 

             let child2_str = 
                 match m.ref_code2 = -1 with
                 | true -> ""
                 | false -> 
                       ", Child2 (genome_ref:" ^ (string_of_int m.ref_code2) 
                       ^ ", chrom_id:" ^ (string_of_int m.chi2_chrom_id) 
                       ^ ", start:" ^ (string_of_int m.sta2)  
                       ^ ", end: " ^ (string_of_int m.en2)  
                       ^ ", direction:" ^ (get_dir m.dir2) ^ ")" 
             in 
             "(" ^ node_str ^ child1_str ^ child2_str ^ ")"
        ) chrom_map 
    in 
    String.concat " | " str_ls

let genome_map_to_string genome = 
    let chrom_map_str = Array.map 
        (fun chrom -> chrom_map_to_string chrom.map
        ) genome.chrom_arr 
    in 
    String.concat " @@ " (Array.to_list chrom_map_str) 


let ref_genome = ref {chrom_arr = [||]; genome_ref_code = -1}

let assign_hom_chrom med cost_mat user_chrom_pams = 
    if !(med.chrom_arr.(0).chrom_id) = -1 then begin
        let chrom_pams = ChromPam.get_chrom_pam user_chrom_pams in

        let num_chrom1 = Array.length !ref_genome.chrom_arr in
        let num_chrom2 = Array.length med.chrom_arr in
        let chrom_cost_mat = Array.make_matrix num_chrom1 num_chrom2 0.0 in 
        for c1 = 0 to num_chrom1 - 1 do
            for c2 = 0 to num_chrom2 - 1 do
                let chrom1 = !ref_genome.chrom_arr.(c1).seq in 
                let chrom2 = med.chrom_arr.(c2).seq in 
                let chrom_med1 = ChromAli.create_med chrom1 in 
                let chrom_med2 = ChromAli.create_med chrom2 in 
                let cost, recost = (ChromAli.cmp_cost chrom_med1 chrom_med2
                                        cost_mat user_chrom_pams) 
                in
                let len = max (Sequence.length chrom1) (Sequence.length chrom2) in 
                chrom_cost_mat.(c1).(c2) <- (float cost) /. (float len);
            done;
        done;

        let map1_arr = Array.create num_chrom1 false in 
        let map2_arr = Array.create num_chrom2 false in 
        

        for times = 0 to num_chrom2 - 1 do

            let best_c1 = ref (-1) in 
            let best_c2 = ref (-1) in
            let min_cost = ref (float Utl.infinity) in

            for c1 = 0 to num_chrom1 - 1 do
                for c2 = 0 to num_chrom2 - 1 do
                    if (map1_arr.(c1) = false) && (map2_arr.(c2) = false) 
                        && (chrom_cost_mat.(c1).(c2) < !min_cost) then begin 
                        min_cost := chrom_cost_mat.(c1).(c2); 
                        best_c1 := c1; 
                        best_c2 := c2; 
                    end;                               
                done;    
            done;  

            let chrom_hom = float chrom_pams.ChromPam.chrom_hom in 
            if  (!min_cost < chrom_hom /. 100.) then begin                
                med.chrom_arr.(!best_c2).chrom_id := !best_c1;
                map1_arr.(!best_c1) <- true;
                map2_arr.(!best_c2) <- true;            
            end 
        done;

        Array.iteri 
            (fun pos mapped -> 
                 if mapped = false then begin 
                     let chrom_id = Array.length !ref_genome.chrom_arr in  
                     let chrom2 = med.chrom_arr.(pos) in  
                     chrom2.chrom_id := chrom_id; 
                     let chrom_ls = Array.to_list !ref_genome.chrom_arr in  
                     let new_chrom_arr = Array.of_list (chrom_ls @ [chrom2]) in  
                     ref_genome := {!ref_genome with chrom_arr = new_chrom_arr};
                 end 
            ) map2_arr;  


(*        print_genome med;        
        print_endline "End of assigning homologous chromosome in GenomeAli"; *)
    end 




let init (genome  : Sequence.s Data.dyna_data) = 
    let chrom_arr = Array.map 
        (fun chrom -> {chrom_ref_code = chrom.Data.code; 
                       chrom_id = ref (-1);
                       map = [];
                       seq = chrom.Data.seq}
        ) genome.Data.seq_arr
    in 
    let clean_chrom_ls = List.filter 
        (fun chrom -> Sequence.length chrom.seq > 0
        ) (Array.to_list chrom_arr)
    in 
    let med = {chrom_arr = Array.of_list clean_chrom_ls; 
               genome_ref_code = Utl.get_new_genome_ref_code ()
              }
    in 
(*    print_genome med; *)
    med




let find_conserved_areas seq1 seq2 cost_mat chrom_pams =
    (if debug then 
         print_endline "Start find conserved arears"); 

    let ali_pam = ChromPam.get_chrom_pam chrom_pams in      
    let len1 = Sequence.length seq1 in
    let len2 = Sequence.length seq2 in 

    let ali_pam = {ali_pam with   
                       ChromPam.min_pos1 = 0;  
                       ChromPam.max_pos1 = len1 - 1;  
                       ChromPam.min_pos2 = 0;  
                       ChromPam.max_pos2 = len2 - 1;  
                  }  
    in   
    let global_map, _, _ = ChromAli.create_global_map seq1 seq2 cost_mat ali_pam in   
    (if debug then 
         print_endline "End find conserved arears"); 
    global_map


let find_chrom chrom_id genome =
    try
        Some (List.find 
                  (fun chrom ->  !(chrom.chrom_id) = chrom_id) (Array.to_list genome.chrom_arr) 
             ) 
    with | Not_found -> None




let create_loci seq subseq_ls =
    let subseq_arr = Array.of_list subseq_ls in 
    Array.sort (fun s1 s2 -> compare s1.Subseq.sta s2.Subseq.sta) subseq_arr;

    let loci_ls = ref [] in 
    let loci_id = ref 1 in 
    let sta = ref 0 in 
    Array.iter (fun s -> 
                    if !sta < s.Subseq.sta  then begin
                        let ns = {Subseq.id = !loci_id;
                                  Subseq.sta = !sta;
                                  Subseq.en = s.Subseq.sta - 1;
                                  Subseq.block_id_ls = []
                                 } 
                        in 
                        incr loci_id;
                        loci_ls := ns::!loci_ls;
                    end;
                    loci_ls := {s with Subseq.id = !loci_id}::!loci_ls;
                    incr loci_id;                     
                    sta := s.Subseq.en + 1;
               ) subseq_arr;  

    let seq_len = Sequence.length seq in 
    if !sta < seq_len then begin
        let ns = {Subseq.id = !loci_id; 
                  Subseq.sta = !sta; 
                  Subseq.en = seq_len - 1;
                  Subseq.block_id_ls = [] 
                 }   
        in 
        incr loci_id;
        loci_ls := ns::!loci_ls
    end;
    List.rev (!loci_ls)
    


let create_fast_general_ali chrom_id genome1_ref_code chrom1_seq loci1_ls
        genome2_ref_code chrom2_seq loci2_ls gb_ls cost_mat ali_pam =

    (if debug then  
         print_endline "Start create_fast_general_ali");

    let len1 = List.length loci1_ls in 
    let len2 = List.length loci2_ls in 
    let len = len1 + len2 + 1 in

    let loci1_arr = Array.of_list loci1_ls in 
    let loci2_arr = Array.of_list loci2_ls in 

    Array.iter (fun sq -> sq.Subseq.id <- sq.Subseq.id + len1) loci2_arr;    

    let gen_gap_code = 0 in     
    let gen_c2 = Array.make_matrix len len Utl.infinity in 
    for r1 = 0 to len1 - 1 do
        let sq1 = loci1_arr.(r1) in 
        let sq1_id = sq1.Subseq.id in 
        let s1 = sq1.Subseq.sta and e1 = sq1.Subseq.en in 
        let sq1_seq = Sequence.sub chrom1_seq s1 (e1 - s1 + 1) in 
        let free1 = Subseq.is_free sq1 in 

        for r2 = 0 to len2 - 1 do
            let sq2 = loci2_arr.(r2) in 
            let sq2_id = sq2.Subseq.id in 
            let s2 = sq2.Subseq.sta and e2 = sq2.Subseq.en in 

            let sq2_seq = Sequence.sub chrom2_seq s2 (e2 - s2 + 1) in 
            let free2 = Subseq.is_free sq2 in

            if free1 && free2 then begin
                let alied_sq1, alied_sq2, cost, _ = 
                    UtlPoy.align2 sq1_seq sq2_seq cost_mat 
                in 
                gen_c2.(sq1_id).(sq2_id) <- cost;
                gen_c2.(sq2_id).(sq1_id) <- cost;
            end; 
        done
    done;

    let pair_gap subseq chrom_seq = 
       let id = subseq.Subseq.id in 
       let del_cost = 
           let o, e = ali_pam.ChromPam.locus_indel_cost in
           let len = Subseq.get_len subseq in  
           o + (e * (len - 1)) / 100 
       in                    
       gen_c2.(id).(gen_gap_code) <-  del_cost;
       gen_c2.(gen_gap_code).(id) <-  del_cost;
    in

    gen_c2.(gen_gap_code).(gen_gap_code) <- 0;
    Array.iter (fun sq1 -> if Subseq.is_free sq1 then pair_gap sq1 chrom1_seq) loci1_arr;
    Array.iter (fun sq2 -> if Subseq.is_free sq2 then pair_gap sq2 chrom2_seq) loci2_arr;



    let free_id1_ls = List.fold_right 
        (fun locus free_id1_ls -> 
             if Subseq.is_free locus then locus.Subseq.id::free_id1_ls
             else free_id1_ls
        ) loci1_ls []
    in 

    let free_id2_ls = List.fold_right 
        (fun locus free_id2_ls -> 
             if Subseq.is_free locus then locus.Subseq.id::free_id2_ls
             else free_id2_ls
        ) loci2_ls []
    in 


    let free_id1_arr = Array.of_list free_id1_ls in
    let free_id2_arr = Array.of_list free_id2_ls in
(*    
    Utl.printIntArr free_id1_arr;
    Utl.printIntArr free_id2_arr;
*)

    let swap_med = ali_pam.ChromPam.swap_med in 
    let total_cost, (recost1, recost2), alied_free_id1, alied_free_id2 = GenAli.create_gen_ali_code         
        `Genome free_id1_arr free_id2_arr gen_c2 gen_gap_code 
        ali_pam.ChromPam.re_meth swap_med ali_pam.ChromPam.circular
    in   

    let alied_free_len = Array.length alied_free_id1 in 
    
    let search_sq2 sq1 = 
        let sq1_id = sq1.Subseq.id in
        let rec move p =             
            if p = alied_free_len then (-1)
            else if alied_free_id1.(p) = sq1_id then alied_free_id2.(p)
            else move (p+1)
        in 
        let sq2_id =  move 0 in
        if sq2_id = 0 then chrom_id, 0, `Positive
        else if sq2_id > 0 then chrom_id, sq2_id, `Positive
        else begin (* sq2_id = -1 *)
            let gb_id = List.hd sq1.Subseq.block_id_ls in  
            let gb = List.find (fun gb -> gb.block_id = gb_id) gb_ls in              
            let b = gb.block in 
            if gb.chrom2_id = chrom_id then begin
                let sq2 = List.find 
                    (fun locus2 -> locus2.Subseq.sta = b.Block.sta2) loci2_ls 
                in
                chrom_id, sq2.Subseq.id, b.Block.direction
            end else gb.chrom2_id, -1, `Positive
        end 
    in   
    
    let chrom_med_len = ref 0 in 
    let med_ls, seg_ls, chrom_cost = 
        Array.fold_left (fun (med_ls, seg_ls, chrom_cost) sq1 ->
             let sq1_id = sq1.Subseq.id in
             let s1 = sq1.Subseq.sta and e1 = sq1.Subseq.en in 
             let sq1_seq = Sequence.sub chrom1_seq s1 (e1 - s1 + 1) in 

             match Subseq.is_free sq1 with
             | true ->
                   let _, sq2_id, _ = search_sq2 sq1 in

                   if sq2_id = gen_gap_code then begin 
                       let med = UtlPoy.create_median_gap sq1_seq cost_mat in
                       let med_len = UtlPoy.cmp_num_not_gap med in  
                       let sta, en = 
                           match med_len with 
                           | 0 -> -1, -1
                           | _ -> !chrom_med_len, !chrom_med_len + med_len - 1
                       in 

                       let seg = 
                           {sta = sta; en = en;
                            cost = gen_c2.(sq1_id).(gen_gap_code);  
                            med_chrom_id = chrom_id;  
                        
                            ref_code1 = genome1_ref_code; sta1 = s1;  en1 = e1;  
                            chi1_chrom_id = chrom_id;  alied_seq1 = sq1_seq;  dir1 = `Positive;  

                            ref_code2 = -1; sta2 = -1;  en2 = -1;  
                            chi2_chrom_id = chrom_id;  alied_seq2 = UtlPoy.get_empty_seq (); dir2 = `Positive
                           }                        
                       in  
                       chrom_med_len := !chrom_med_len + med_len;
                       med::med_ls, seg::seg_ls, chrom_cost

                    end else begin
                        let sq2 =  loci2_arr.(sq2_id - len1 - 1) in
                        let s2 = sq2.Subseq.sta and e2 = sq2.Subseq.en in 
                        let sq2_seq = Sequence.sub chrom2_seq s2 (e2 - s2 + 1) in  
                        let alied_seq1, alied_seq2, _, _ = UtlPoy.align2 sq1_seq
                            sq2_seq cost_mat 
                        in 
                        let med, cost = UtlPoy.create_median_seq alied_seq1 alied_seq2 cost_mat in
                        let med_len = UtlPoy.cmp_num_not_gap med in  
                        let sta, en = 
                            match med_len with 
                            | 0 -> -1, -1
                            | _ -> !chrom_med_len, !chrom_med_len + med_len - 1
                        in 

                        let seg = 
                            {sta = sta; en = en;
                             cost = cost; med_chrom_id = chrom_id; 
                        
                             ref_code1 = genome1_ref_code; sta1 = s1;  en1 = e1; 
                             chi1_chrom_id = chrom_id; alied_seq1 = alied_seq1; dir1 = `Positive;  

                             ref_code2 = genome2_ref_code; sta2 = s2; en2 = e2; 
                             chi2_chrom_id = chrom_id;  alied_seq2 = alied_seq2; dir2 = `Positive;                          
                            }
                        in 
                        chrom_med_len := !chrom_med_len + med_len; 
                        med::med_ls, seg::seg_ls, chrom_cost
                    end 
             | false ->
                   let gb_id = List.hd sq1.Subseq.block_id_ls in 
                   let gb = List.find (fun gb -> gb.block_id = gb_id) gb_ls in 
                   let b = gb.block in 
                   let alied_seq1 =  Utl.deref b.Block.alied_seq1 in
                   let alied_seq2=  Utl.deref b.Block.alied_seq2 in

                   let med, cost = UtlPoy.create_median_seq alied_seq1 
                       alied_seq2 cost_mat  
                   in  
                   let med_len = UtlPoy.cmp_num_not_gap med in  
                   let sta, en = 
                       match med_len with 
                       | 0 -> -1, -1
                       | _ -> !chrom_med_len, !chrom_med_len + med_len - 1
                   in 

                   
                   let seg = 
                      {sta = sta; en = en;
                       cost = cost; med_chrom_id = chrom_id; 

                       ref_code1 = genome1_ref_code;
                       sta1 = b.Block.sta1; en1 = b.Block.en1; chi1_chrom_id = gb.chrom1_id;
                       alied_seq1 = alied_seq1; dir1 = `Positive;

                       ref_code2 = genome2_ref_code;
                       sta2 = b.Block.sta2; en2 = b.Block.en2; chi2_chrom_id = gb.chrom2_id;
                       alied_seq2 = alied_seq2; dir2 = b.Block.direction;
                   }
                   in 
                   chrom_med_len := !chrom_med_len + med_len; 

                   med::med_ls, seg::seg_ls, chrom_cost + cost 

        ) ([], [], total_cost) loci1_arr 
    in 



    let chrom_bp = ali_pam.ChromPam.chrom_breakpoint in 
    let bp =
        match ali_pam.ChromPam.re_meth with
        | `Breakpoint bp -> bp
        | _ -> failwith "Can not calculate inversion distance for multiple chromosome!"
    in 

    let cmp_breakpoint_cost = 
        let total_bp = ref 0 in
        for p = 0 to len1 - 2 do
            let chrom21_id, sq21_id, dir21 = search_sq2 loci1_arr.(p) in 
            let chrom22_id, sq22_id, dir22 = search_sq2 loci1_arr.(p+1) in
            let one_bp = 
                if (chrom21_id != chrom_id) || (chrom22_id != chrom_id) then chrom_bp
                else if (sq21_id = 0) || (sq22_id = 0) then 0
                else if ( (sq21_id + 1 = sq22_id) && (dir21 = `Positive) && (dir22 = `Positive) ) ||
                    ( (sq21_id - 1 = sq22_id) && (dir21 = `Negative)  && (dir22 = `Negative) ) then 0
                else  bp
            in 
            total_bp := !total_bp + one_bp
        done;
        !total_bp
    in 
    

    let total_bp_cost = cmp_breakpoint_cost in
    let med_ls = List.rev med_ls in 
    let seg_ls = List.rev seg_ls in 
    let med = UtlPoy.concat med_ls in 


    med, seg_ls, (chrom_cost - (recost1 + recost2) + total_bp_cost), total_bp_cost




let create_chrom_med (genome1_ref_code, chrom1) (genome2_ref_code, chrom2) gb_ls cost_mat chrom_pams = 
    let subseq1_ls = 
        List.fold_left  
        (fun subseq1_ls gb -> 
             if gb.chrom1_id = !(chrom1.chrom_id) then begin
                 let subseq1 = 
                     {Subseq.id = -1;
                      Subseq.sta = gb.block.Block.sta1; 
                      Subseq.en = gb.block.Block.en1; 
                      Subseq.block_id_ls = [gb.block_id]} 
                 in 
                 subseq1::subseq1_ls
             end else subseq1_ls
        ) [] gb_ls
        
    in 

    let subseq2_ls = 
        List.fold_left  
        (fun subseq2_ls gb -> 
             if gb.chrom2_id = !(chrom2.chrom_id) then begin
                 let subseq2 = 
                     {Subseq.id = -1;  
                      Subseq.sta = gb.block.Block.sta2; 
                      Subseq.en = gb.block.Block.en2; 
                      Subseq.block_id_ls = [gb.block_id]} 
                 in 
                 subseq2::subseq2_ls
             end else subseq2_ls
        ) [] gb_ls        
    in 


    let loci1_ls = create_loci chrom1.seq subseq1_ls in 
    let loci2_ls = create_loci chrom2.seq subseq2_ls in 

(*    
    List.iter Subseq.print loci1_ls;
    print_newline ();
    List.iter Subseq.print loci2_ls;
*)
    let chrom_id = !(chrom1.chrom_id) in
    let med, seg_ls, chrom_cost, recost = create_fast_general_ali chrom_id
        genome1_ref_code chrom1.seq loci1_ls genome2_ref_code chrom2.seq loci2_ls gb_ls cost_mat chrom_pams 
    in 
    let chrom_ref_code = Utl.get_new_chrom_ref_code () in 



    {chrom_id = ref chrom_id; 
     chrom_ref_code = chrom_ref_code; 
     seq = med; map = seg_ls}, chrom_cost, recost


    



(** Compute the cost between two sequences with rearrangement operations *)
let create_genome_blocks med1 med2 cost_mat chrom_pams =
    let num_chrom1 = Array.length med1.chrom_arr in  
    let num_chrom2 = Array.length med2.chrom_arr in 
    let max_chrom = Array.length !ref_genome.chrom_arr in 

    let gb_ls = ref [] in 
    let gb_id = ref 0 in 


    for chrom_id = 0 to max_chrom - 1 do
        let chrom1_opt = find_chrom chrom_id med1 in 
        let chrom2_opt = find_chrom chrom_id med2 in 
        match chrom1_opt, chrom2_opt with
        | Some chrom1, Some chrom2 ->
              let blocks = find_conserved_areas chrom1.seq chrom2.seq cost_mat chrom_pams in

              gb_ls := List.fold_left 
                  (fun gb_ls block -> 
                       let gb = {block_id = !gb_id; chrom1_id  = chrom_id;
                                 chrom2_id = chrom_id; block = block}
                       in 
                       incr gb_id;
                       gb::gb_ls
                  ) !gb_ls blocks;                               
        | _, _ -> ()    
    done;

    let is_overlaped gb gb_ls = 
        let loci_overlaped s1 e1 s2 e2 = 
            if (e1 < s2) or (e2 < s1) then false
            else true
        in 
    
        List.fold_left
            (fun overlaped gb2 -> 
                 let b1 = gb.block and b2 = gb2.block in 
                 if (gb.chrom1_id = gb2.chrom1_id) &&
                     (loci_overlaped b1.Block.sta1 b1.Block.en1 b2.Block.sta1 b2.Block.en1) then true
                 else if (gb.chrom2_id = gb2.chrom2_id) && 
                     (loci_overlaped b1.Block.sta2 b1.Block.en2 b2.Block.sta2 b2.Block.en2) then true
                 else overlaped
            ) false gb_ls  
    in 


    let mapped_arr = Array.make max_chrom 0 in 
    Array.iter (fun chrom -> 
                    let id = !(chrom.chrom_id) in 
                    mapped_arr.(id) <- mapped_arr.(id) + 1
               ) med1.chrom_arr;

    Array.iter (fun chrom -> 
                    let id = !(chrom.chrom_id) in 
                    mapped_arr.(id) <- mapped_arr.(id) + 1
               ) med2.chrom_arr;



    for c1 = 0 to num_chrom1 -  1 do
        let chrom1 = med1.chrom_arr.(c1) in 
        let chrom1_id = !(chrom1.chrom_id) in 
        for c2 = 0 to num_chrom2 - 1 do
            let chrom2 = med2.chrom_arr.(c2) in 
            let chrom2_id = !(chrom2.chrom_id) in 
            if (chrom1_id != chrom2_id) && (mapped_arr.(chrom1_id) = 2) &&
                (mapped_arr.(chrom2_id) = 2) then begin
                let blocks = find_conserved_areas chrom1.seq chrom2.seq 
                    cost_mat chrom_pams
                in  
              
                gb_ls := List.fold_left  
                  (fun gb_ls block -> 
                       let gb = {block_id = !gb_id; chrom1_id = !(chrom1.chrom_id);
                                 chrom2_id = !(chrom2.chrom_id); block = block}
                       in                        
                       incr gb_id;

                       if is_overlaped gb gb_ls then gb_ls
                       else gb::gb_ls
                  ) !gb_ls blocks                
            end 
        done
    done;
    !gb_ls
    
        
(** Compute the cost between genomes with rearrangement operations *)
let create_med med1 med2 cost_mat user_chrom_pams = 

    assign_hom_chrom med1 cost_mat user_chrom_pams;
    assign_hom_chrom med2 cost_mat user_chrom_pams;

    let gb_ls = create_genome_blocks med1 med2 cost_mat user_chrom_pams in 

    let max_chrom = Array.length !ref_genome.chrom_arr in 

    let rev_chrom_med_ls = ref [] in 
    let g_cost = ref 0 in 
    let g_recost = ref 0 in 



    let ali_pam = ChromPam.get_chrom_pam user_chrom_pams in 
    let child1_genome_ref_code = med1.genome_ref_code in 
    let child2_genome_ref_code = med2.genome_ref_code in 

    for chrom_id = 0 to max_chrom - 1 do 
        let chrom1_opt = find_chrom chrom_id med1 in 
        let chrom2_opt = find_chrom chrom_id med2 in 
        match chrom1_opt, chrom2_opt with
        | Some chrom1, Some chrom2 ->
              let chrom_med,  chrom_cost, recost = create_chrom_med
                  (child1_genome_ref_code, chrom1)
                  (child2_genome_ref_code, chrom2) gb_ls cost_mat ali_pam
              in 
              
              rev_chrom_med_ls := chrom_med::!rev_chrom_med_ls; 

              g_cost := !g_cost + chrom_cost;
              g_recost := !g_recost + recost;
        | Some chrom1, None -> 

              let med_seq = UtlPoy.create_median_gap chrom1.seq cost_mat in 
              let med_len = UtlPoy.cmp_num_not_gap med_seq in 

              let chrom1_len = Sequence.length chrom1.seq in  
              let o, e = ali_pam.ChromPam.chrom_indel_cost in  
              let indel_cost = o + ((UtlPoy.cmp_num_all_DNA chrom1.seq) - 1) * e / 100 in 

              let seg =   
                  {sta = 0; en = med_len - 1; 
                   cost = indel_cost; med_chrom_id = chrom_id; 
                    
                   ref_code1 = child1_genome_ref_code; sta1 = 0;  en1 = chrom1_len - 1;
                   chi1_chrom_id = !(chrom1.chrom_id); alied_seq1 = chrom1.seq; dir1 = `Positive;  

                   ref_code2 = -1; sta2 = -1; en2 = -1;  
                   chi2_chrom_id = -1;  alied_seq2 = UtlPoy.get_empty_seq (); dir2 = `Positive;                          
                  } 
              in  

              let chrom_med = {chrom_ref_code = Utl.get_new_chrom_ref_code ();
                               chrom_id = ref chrom_id; 
                               map = [seg];
                               seq = med_seq}
              in 

              g_cost := !g_cost + indel_cost;
              rev_chrom_med_ls := chrom_med::!rev_chrom_med_ls;                   

        | None, Some chrom2 -> 
              let med_seq = UtlPoy.create_median_gap chrom2.seq cost_mat in
              let med_len = UtlPoy.cmp_num_not_gap med_seq in 

              let o, e = ali_pam.ChromPam.chrom_indel_cost in 
              let chrom2_len = Sequence.length chrom2.seq in 
              let indel_cost = o + ( (UtlPoy.cmp_num_all_DNA chrom2.seq) - 1) * e / 100 in

              let seg =  
                  {sta = 0; en = med_len - 1;
                   cost = indel_cost; med_chrom_id = chrom_id; 
                    
                   ref_code1 = -1; sta1 = -1;  en1 = -1;
                   chi1_chrom_id = -1; alied_seq1 = UtlPoy.get_empty_seq (); dir1 = `Positive;  


                   ref_code2 = child2_genome_ref_code; sta2 = 0;  en2 = chrom2_len - 1;
                   chi2_chrom_id = chrom_id;  alied_seq2 = chrom2.seq; dir2 = `Positive;                          
                  } 
              in  
              

              let chrom_med = {chrom_ref_code = Utl.get_new_chrom_ref_code ();
                               chrom_id = ref chrom_id; 
                               map = [seg];
                               seq = med_seq}
              in 

              g_cost := !g_cost + indel_cost;
              rev_chrom_med_ls := chrom_med::!rev_chrom_med_ls;               
        | None, None -> ()
    done;

    let chrom_med_ls = 
        List.map (fun chrom -> 
                      {chrom with seq = (UtlPoy.delete_gap chrom.seq)}
                 ) (List.rev !rev_chrom_med_ls)
    in 
    let chrom_med_ls = List.filter 
        (fun chrom -> Sequence.length chrom.seq > 0) chrom_med_ls
    in 
    let genome_med = {chrom_arr = Array.of_list chrom_med_ls; 
                      genome_ref_code = Utl.get_new_genome_ref_code ()}
    in 
    
    if (List.length chrom_med_ls = 0) then failwith "Created a fucking empty genome";
    
    genome_med, !g_cost, !g_recost
        


let cmp_cost med1 med2 cost_mat user_chrom_pams = 
    if debug = true then begin
        fprintf stdout "cmp_cost"; print_newline ();
        let genomeFile = open_out "genome12" in 
        fprintf genomeFile ">genome1\n";  
        Array.iter (fun chrom -> 
                    Sequence.print genomeFile chrom.seq Alphabet.nucleotides;
                    fprintf genomeFile " @ ") med1.chrom_arr;
        fprintf genomeFile "\n";  

        fprintf genomeFile ">genome2\n";  
        Array.iter (fun chrom -> 
                    Sequence.print genomeFile chrom.seq Alphabet.nucleotides;
                    fprintf genomeFile " @ ") med2.chrom_arr;
        fprintf genomeFile "\n";  
        close_out genomeFile;
    end;   

    let _, cost, recost = create_med med1 med2 cost_mat user_chrom_pams in 

    cost, recost


(** Compute the cost between two genomes with rearrangement operations *)
let find_med2_ls med1 med2 cost_mat user_chrom_pams = 
    if debug = true then begin
        fprintf stdout "Find_meds_ls"; print_newline ();
        let genomeFile = open_out "genome12" in 
        fprintf genomeFile ">genome1\n";  
        Array.iter (fun chrom -> 
                    Sequence.print genomeFile chrom.seq Alphabet.nucleotides;
                    fprintf genomeFile " @ ") med1.chrom_arr;
        fprintf genomeFile "\n";  

        fprintf genomeFile ">genome2\n";  
        Array.iter (fun chrom -> 
                    Sequence.print genomeFile chrom.seq Alphabet.nucleotides;
                    fprintf genomeFile " @ ") med2.chrom_arr;
        fprintf genomeFile "\n";  
        close_out genomeFile;
    end;   
    

    let med, cost, recost  = create_med med1 med2 cost_mat user_chrom_pams in 

    cost, recost, [med]
    

let compare med1 med2 =
    let max_chrom = Array.length (!ref_genome).chrom_arr in 
    let chrom_ids = Array.make max_chrom 0 in 
    Array.iter (fun chrom -> 
                    let id = !(chrom.chrom_id) in 
                    chrom_ids.(id) <- chrom_ids.(id) + 1
               ) med1.chrom_arr;

    Array.iter (fun chrom -> 
                    let id = !(chrom.chrom_id) in 
                    chrom_ids.(id) <- chrom_ids.(id) + 1
               ) med2.chrom_arr;

    let rec is_compared chrom_id =
        if chrom_id = max_chrom then true  
        else if (chrom_ids.(chrom_id) = 0) || (chrom_ids.(chrom_id) = 2) then
            is_compared (chrom_id + 1)
        else false
    in 

    match is_compared 0 with
    | false -> compare (Array.length med1.chrom_arr) (Array.length med2.chrom_arr)
    | true -> 
          let rec compare_chrom chrom_id = 
              if chrom_id = max_chrom then 0
              else begin
                  let chrom1_opt = find_chrom chrom_id med1 in   
                  let chrom2_opt = find_chrom chrom_id med2 in   
                  match chrom1_opt, chrom2_opt with  
                  | Some chrom1, Some chrom2 ->  
                        let cmp = Sequence.compare chrom1.seq chrom2.seq in
                        if cmp = 0 then compare_chrom (chrom_id + 1) 
                        else cmp
                  | _, _ -> compare_chrom (chrom_id + 1) 
              end 
          in 
          compare_chrom 0
    

