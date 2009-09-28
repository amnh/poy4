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

(** A Breakinv character set implementation. The breakinv
* character set allows rearrangements *)

let () = SadmanOutput.register "BreakinvCS" "$Revision: 1616 $"

exception Illegal_Arguments
let () = SadmanOutput.register "Breakinv Character" "$Revision: 803 $"

let fprintf = Printf.fprintf

module IntMap = All_sets.IntegerMap
module IntSet = All_sets.Integers
type meds_t = Breakinv.meds_t

(** [t] is data structure to present a set of breakinv characters *)
type t = { 
    meds : meds_t IntMap.t; (** a set of breakinv characters *)
    costs : float IntMap.t;
    recosts : float IntMap.t;
    total_cost : float;    (** The total cost of the character set *)
    total_recost : float;  (** The total cost of the character set *)

    subtree_recost : float;         (** The total subtree recost of the character set *)

    c2 : Cost_matrix.Two_D.m;       (** The two dimensional cost matrix to be used in the character set *)
    c3 : Cost_matrix.Three_D.m;     (** The three dimensional cost matrix to be used in the character set *)
    alph : Alphabet.a;              (** The alphabet of the sequence set *)
    breakinv_pam : Data.dyna_pam_t; 
    code : int;                     (** The set code *)
}

let cardinal x = IntMap.fold (fun _ _ acc -> acc + 1) x.meds 0

(** [of_array spec arr code] creates a breakinv set
* from an array of sequences [arr] *)
let of_array spec arr code = 
    let adder (meds, costs, recosts) (seq, key) = 
        let med = 
            Breakinv.init_med seq spec.Data.tcm2d spec.Data.alph spec.Data.pam 
        in 
        (IntMap.add key med meds), 
        (IntMap.add key 0.0 costs), 
        (IntMap.add key 0.0 recosts)
    in
    let meds, costs, recosts = 
        let empty = IntMap.empty in
        Array.fold_left adder (empty, empty, empty) arr
    in
    {
        meds = meds;
        costs = costs;
        recosts = recosts;
        total_cost = 0.0;        
        total_recost = 0.0;
        subtree_recost = 0.;
        c2 = spec.Data.tcm2d;
        c3 = spec.Data.tcm3d;
        alph = spec.Data.alph;
        breakinv_pam = spec.Data.pam;
        code = code;
    }

(** [of_list spec lst code] create a breakinv set
* from a list sequence [lst] *)
let of_list spec lst code = 
    let arr = Array.of_list lst in
    of_array spec arr code

(** [to_list t] returns a list of breakinv characters 
* from a set of breakinv characters *)
let to_list t =
    IntMap.fold (fun code med acc -> (med, code) :: acc) t.meds []

(** [same_codes a b] returns true if code set of [a] 
* is the same to code set of [b], otherwise false *)
let same_codes a b =
    let checker x _ res = res && (IntMap.mem x b) in
    IntMap.fold checker a true

(** [median2 a b] returns the median set 
* between breakinv character sets [a] and [b] *)
let median2 (a : t) (b : t) =
    (* We will use imperative style for this function *)
    let empty = IntMap.empty in


    let median code (meda : meds_t) (medians, costs, recosts, total_cost, total_recost) = 
        let medb : meds_t = IntMap.find code b.meds in
        let medab = Breakinv.find_meds2 meda medb in
        
        let new_median = IntMap.add code medab medians 
        and new_costs = 
            IntMap.add code (float_of_int medab.Breakinv.total_cost) costs  
        and new_recosts = 
            IntMap.add code (float_of_int medab.Breakinv.total_recost) recosts  
        and new_total_cost = total_cost + medab.Breakinv.total_cost 
        and new_total_recost = total_recost + medab.Breakinv.total_recost in
        new_median, new_costs, new_recosts, new_total_cost, new_total_recost
    in
    let medab_map, new_costs, new_recosts, total_cost, total_recost = 
        IntMap.fold median a.meds (empty, empty, empty, 0, 0)
    in

    let subtree_recost = a.subtree_recost +. b.subtree_recost +. (float_of_int total_recost) in 
    { a with meds = medab_map; costs = new_costs; recosts = new_recosts;
          total_cost = float_of_int total_cost; 
          total_recost = float_of_int total_recost;
          subtree_recost = subtree_recost;
    }
   
(** [median3 p n c1 c2] returns the median set of
* breakinv character sets [p], [c1] and [c2] *)
let median3 p n c1 c2 =
(*    print_endline "median3 in BreakinvCS module"; *)
    let median code  medp res_medians = 
        let med1= IntMap.find code c1.meds in 
        let med2 = IntMap.find code c2.meds in
        let medp12 = Breakinv.find_meds3 medp med1 med2 in
        IntMap.add code medp12 res_medians 
(*        let med12 = Med.find_meds2 med1 med2 p.c2 in 
        IntMap.add code med12 res_medians *)
    in
    let acc = IntMap.empty in
    let medp12_map = IntMap.fold median p.meds acc in
    { n with meds = medp12_map; }
  (*  
        
    if( (n.meds = c1.meds) && (n.meds = c2.meds) ) then
        begin
                Printf.printf " !!! in median3, a leaf \n %!";
                 let median code  medp res_medians = 
                     let medp12 = n.meds in
                     IntMap.add code medp12 res_medians 
(*        let med12 = Med.find_meds2 med1 med2 p.c2 in 
        IntMap.add code med12 res_medians *)
                  in
                  let acc = IntMap.empty in
                  let medp12_map = IntMap.fold median p.meds acc in
                 { n with meds = medp12_map; }
        end
    else
        begin
                    Printf.printf " !!! in median3, not a leaf \n %!";


                    (*    print_endline "median3 in BreakinvCS module"; *)
                    let median code  medp res_medians = 
                         let med1= IntMap.find code c1.meds in 
                         let med2 = IntMap.find code c2.meds in
                         let medp12 = Breakinv.find_meds3 medp med1 med2 in
                         IntMap.add code medp12 res_medians 
(*        let med12 = Med.find_meds2 med1 med2 p.c2 in 
        IntMap.add code med12 res_medians *)
                    in
                    let acc = IntMap.empty in
                    let medp12_map = IntMap.fold median p.meds acc in
                    { n with meds = medp12_map; }
        end
    
  *)


(** [readjust to_adjust modified ch1 ch2 parent mine] returns
* the readjusted median set [mine] of three breakinv character
* sets [ch1], [ch2] and [parent] *)
let readjust to_adjust modified ch1 ch2 parent mine = 
    let empty = IntMap.empty and
            c2 = parent.c2 and
            c3 = parent.c3 
    in

    let adjusted code parent_chrom acc =
        let to_adjust =
            match to_adjust with
            | None -> All_sets.Integers.singleton code
            | Some x -> x
        in
        let (modified, res_medians, res_costs, total) = acc in
        let my_chrom = IntMap.find code mine.meds
        and ch1_chrom = IntMap.find code ch1.meds
        and ch2_chrom = IntMap.find code ch2.meds in
        if (not (All_sets.Integers.mem code to_adjust)) then 
            let new_costs = IntMap.add code 0. res_costs 
            and new_single = IntMap.add code my_chrom res_medians in
            modified, new_single, new_costs, total
        else begin
            let rescost, seqm, changed = 
                Breakinv.readjust_3d ch1_chrom ch2_chrom my_chrom
                    c2 c3 parent_chrom
            in
            let new_single = IntMap.add code seqm res_medians
            and new_costs = IntMap.add code (float_of_int rescost) res_costs 
            and new_total = total + rescost in
            let modified = 
                if changed then All_sets.Integers.add code modified
                else modified
            in
            modified, new_single, new_costs, new_total        
        end 
    in 
    let modified, meds, costs, total_cost = 
        IntMap.fold adjusted parent.meds (modified, empty, empty, 0)
    in
    let tc = float_of_int total_cost in
    modified,
    tc,
    { mine with meds = meds; costs = costs; total_cost = tc }

(** [distance a b] returns total distance between 
* two breakinv character sets [a] and [b] *)
let distance (a : t) (b : t)  = 
    let single_distance code meda (acc_cost, acc_recost) =
        let medb = IntMap.find code b.meds in
        let cost, recost = Breakinv.cmp_min_pair_cost meda medb in 
        acc_cost + cost, acc_recost + recost
    in
    let cost, _ = IntMap.fold single_distance a.meds (0, 0) in 
    float_of_int cost

(** [max_distance a b] returns total maximum distances
* between two breakinv character sets [a] and [b]*)
let max_distance (a : t) (b : t)  = 
    let single_distance code meda (acc_cost, acc_recost) =
        let medb = IntMap.find code b.meds in
        let cost, recost = Breakinv.cmp_max_pair_cost meda medb in 
        acc_cost + cost, acc_recost + recost
    in
    let cost, _ = IntMap.fold single_distance a.meds (0,0) in 
    float_of_int cost

(** [to_string a] converts the breakinv character set [a]
* into string format *)
let to_string a =
    let builder code med acc =
        let code = string_of_int code in 
        let seq_ls = List.map (fun med -> Sequence.to_string med.BreakinvAli.seq a.alph) 
            med.Breakinv.med_ls 
        in 

        let seq = String.concat ":" seq_ls in 
        acc ^ code ^ ": " ^ seq ^ "; "
    in
    IntMap.fold builder a.meds ""

(* [dist_2 n a b] calculates the cost of joining 
* the node containing breakinv character set [n]
* between two nodes containing [a] and [b]. 
* [a] must be the parent (ancestor) of [b] *)
let dist_2 n a b =
    let cost_calculator code medb (acc_cost, acc_recost) =
        let medn = IntMap.find code n.meds
        and meda = IntMap.find code a.meds 
        and medb = IntMap.find code b.meds in
        
        let medab : meds_t = Breakinv.find_meds2 meda medb in 
        
        let cost, recost = Breakinv.cmp_min_pair_cost medn medab in 
        acc_cost + cost, acc_recost + recost
    in
    let cost, _ =     IntMap.fold cost_calculator b.meds (0,0) in 
    float_of_int cost


(** [f_codes s c] returns breakinv character subset
* of breakinv character set [s] whose codes are also in breakinv character set [c] *)
let f_codes s c = 
    let check x = All_sets.Integers.mem x c in
    let adder x y acc = 
        if check x then IntMap.add x y acc 
        else acc
    in
    let n_meds = IntMap.fold adder s.meds IntMap.empty
    and n_costs = IntMap.fold adder s.costs IntMap.empty in
    { s with meds = n_meds; costs = n_costs}

(** [f_codes s c] returns breakinv character subset
* of breakinv character set [s] whose codes are NOT in breakinv character set [c] *)
let f_codes_comp s c = 
    let check x = not (All_sets.Integers.mem x c) in
    let adder x y acc = 
        if check x then IntMap.add x y acc 
        else acc
    in
    let n_meds = IntMap.fold adder s.meds IntMap.empty
    and n_costs = IntMap.fold adder s.costs IntMap.empty in
    { s with meds = n_meds; costs = n_costs }

(** [compare_data a b] compares breakinv character
* set [a] and [b]. Returns 0 if they are the same, 
* otherwise (-1) or (1) *)
let compare_data a b =
    let comparator code medb acc =
        if acc = 0 then begin
            let meda = IntMap.find code a.meds in
            Breakinv.compare meda medb
        end else acc
    in
    IntMap.fold comparator b.meds 0


(** [to_formatter ref_codes attr t parent_t d] returns
* the map between breakinv character set [t] and its parents
* [parent_t] in the Tag.output format *)
let to_formatter ref_codes attr t (parent_t : t option) d : Xml.xml Sexpr.t list = 
    let _, state = List.hd attr in 
    let output_breakinv code med acc =
        let med = 
            try
                List.find (fun med -> 
                               IntSet.mem med.BreakinvAli.ref_code  ref_codes
                          ) med.Breakinv.med_ls
            with Not_found -> failwith "Not found med -> to_formatter -> BreakinvCS"
        in         
        let cost, recost =
            match parent_t with  
            | None -> 0, 0
            | Some parent -> begin
                  let parent_med_ls = IntMap.find code parent.meds in   
                  let parent_med = List.find 
                      (fun med -> 
                           IntSet.mem med.BreakinvAli.ref_code ref_codes 
                      ) parent_med_ls.Breakinv.med_ls
                  in                                                                  
                  let cost, recost =
                      match state with
                      | `String "Preliminary" ->
                            BreakinvAli.get_costs parent_med med.BreakinvAli.ref_code  
                      | `String "Final" ->
                            BreakinvAli.get_costs med parent_med.BreakinvAli.ref_code                               
                      | _ ->
                            let cost = IntMap.find code t.costs in 
                            let recost = IntMap.find code t.recosts in 
                            (int_of_float cost), (int_of_float recost)
                  in 
                  cost, recost
              end 
        in  
        let seq = Sequence.to_formater med.BreakinvAli.seq t.alph in
        let module T = Xml.Characters in
        (PXML 
            -[T.breakinv]
                (* Attributes *)
                ([T.name] = [`String (Data.code_character code d)])
                ([T.cost] = [`Int cost])
                ([T.recost] = [`Int recost])
                ([T.definite] = [`Bool (cost > 0)])
                ([attr])

                { `String seq }
            --) :: acc
    in
    IntMap.fold output_breakinv t.meds []



(** [to_single ref_codes root single_parent mine] returns
* the single states of breakinv character set [mine] *) 
let to_single ref_codes (root : t option) single_parent mine = 
    let previous_total_cost = mine.total_cost in 

    let median code med (acc_meds, acc_costs, acc_recosts, acc_total_cost) =        
        let amed = List.hd med.Breakinv.med_ls in

        let parent_med = IntMap.find code single_parent.meds in  
      
        let aparent_med = List.hd parent_med.Breakinv.med_ls in
                
        let cost, (recost1, recost2) = 
            match root with 
            | Some root -> 0,(0, 0) 
            | None ->
                  BreakinvAli.cmp_cost amed aparent_med 
                      med.Breakinv.gen_cost_mat med.Breakinv.pure_gen_cost_mat med.Breakinv.alpha
                      med.Breakinv.breakinv_pam
        in 
        
        let single_med = {med with Breakinv.med_ls = [amed]} in 
        
        let new_single = IntMap.add code single_med acc_meds in
        let new_costs = IntMap.add code (float_of_int cost) acc_costs in 
        let new_recosts = IntMap.add code (float_of_int (recost1 + recost2) ) acc_recosts in 
        new_single, new_costs, new_recosts, (acc_total_cost + cost)
    in  
    
    let meds, costs,  recosts, total_cost = 
        match root with
        | Some root ->
              IntMap.fold median root.meds (IntMap.empty, IntMap.empty, IntMap.empty, 0)
        | None ->
              IntMap.fold median mine.meds (IntMap.empty, IntMap.empty, IntMap.empty, 0)
    in 
        
    previous_total_cost, float_of_int total_cost, 
    {mine with meds = meds; 
         costs = costs;
         recosts = recosts;
         total_cost = float_of_int total_cost}
   



(** [get_active_ref_code t] returns active reference codes
* of breakinv character set [t]. One active reference code
* for one breakinv character *)
let get_active_ref_code t = 
    IntMap.fold 
        (fun _ meds (acc_ref_code, acc_child_ref_code) ->
             let ref_code, child1_ref_code, child2_ref_code = 
                 Breakinv.get_active_ref_code meds 
             in
             IntSet.add ref_code acc_ref_code,
             IntSet.add child2_ref_code (IntSet.add child1_ref_code acc_child_ref_code)  
        ) t.meds (IntSet.empty, IntSet.empty)
