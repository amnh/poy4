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

(** A Genome Character Set implementation *)
exception Illegal_Arguments
let () = SadmanOutput.register "GenomeCS" "$Revision: 1266 $"

let fprintf = Printf.fprintf

module IntMap = All_sets.IntegerMap

type meds_t = Genome.meds_t

(** A sequence character type. *)
type t = { 
    (** The sequences that form the set *)
    meds : meds_t IntMap.t;
    costs : float IntMap.t;
    recosts : float IntMap.t;
    total_cost : float;             (** The total cost of the character set *)
    total_recost : float;             (** The total cost of the character set *)
    subtree_recost : float;         (** The total subtree recost of the
                                        character set*)

    c2 : Cost_matrix.Two_D.m;       (** The two dimensional cost matrix to 
                                    be used in the character set *)
    c3 : Cost_matrix.Three_D.m;     (** The three dimensional cost matrix to be 
                                    used in the character set *)
    alph : Alphabet.a;              (** The alphabet of the sequence set *)
    code : int;                     (** The set code *)
}

let cardinal x = IntMap.fold (fun _ _ x -> x + 1) x.meds 0


let of_array spec arr code taxon num_taxa = 
    let adder (cur_meds, cur_costs, cur_recosts) (seq, key) = 
        let med = Genome.init_med seq spec.Data.pam taxon num_taxa in
        (IntMap.add key med cur_meds),
        (IntMap.add key 0.0 cur_costs),
        (IntMap.add key 0.0 cur_recosts)
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
        code = code;
    }

let of_list spec lst =
    let arr = Array.of_list lst in
    of_array spec arr

let to_list t =
    IntMap.fold (fun code med acc -> (med, code) :: acc) t.meds []



let same_codes a b =
    let checker x _ res = res && (IntMap.mem x b) in
    IntMap.fold checker a true



let median2 (a : t) (b : t) =
    (* We will use imperative style for this function *)
    let empty = IntMap.empty in

    let median code (meda : meds_t) (medians, costs, recosts, total_cost, total_recost) = 
        let medb : meds_t = IntMap.find code b.meds in
        let medab = Genome.find_meds2 meda medb in
        
        let new_median = IntMap.add code medab medians 
        and new_costs = IntMap.add code (float_of_int medab.Genome.total_cost) costs  
        and new_recosts = IntMap.add code (float_of_int medab.Genome.total_recost) recosts  
        and new_total_cost = total_cost + medab.Genome.total_cost 
        and new_total_recost = total_recost + medab.Genome.total_recost in

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
    

let median3 p n c1 c2 = 
(*    print_endline "median3 in GenomeCs module"; *)
    let median code  medp res_medians = 
        let med1= IntMap.find code c1.meds in 
        let med2 = IntMap.find code c2.meds in
        
        let medp12 = Genome.find_meds3 medp med1 med2 in
          IntMap.add code medp12 res_medians 
(*        let med12 = Genome.find_meds2 med1 med2 p.c2 in 
        IntMap.add code med12 res_medians *)
    in
    let acc = IntMap.empty in
    let medp12_map = IntMap.fold median p.meds acc in
    { n with meds = medp12_map; }

let distance (a : t) (b : t)  = 
    let single_distance code meda (acc_cost, acc_recost) =
        let medb = IntMap.find code b.meds in
        let cost, recost = Genome.cmp_min_pair_cost meda medb in 
        acc_cost + cost, acc_recost + recost
    in
    let cost, _ = IntMap.fold single_distance a.meds (0,0) in 
    float_of_int cost

let max_distance (a : t) (b : t)  = 
    let single_distance code meda (acc_cost, acc_recost) =
        let medb = IntMap.find code b.meds in
        let cost, recost = Genome.cmp_max_pair_cost meda medb in 
        acc_cost + cost, acc_recost + recost
    in
    let cost, _ = IntMap.fold single_distance a.meds (0,0) in 
    float_of_int cost

let to_string a =
    let builder code med acc =
        let code = string_of_int code in 
        let seq_ls = List.map GenomeAli.to_string med.Genome.med_ls in 
        let seq = String.concat ":" seq_ls in 
        acc ^ code ^ ": " ^ seq ^ "; "
    in
    IntMap.fold builder a.meds ""


let dist_2 n a b =
    let cost_calculator code medb (acc_cost, acc_recost) =
        let medn = IntMap.find code n.meds
        and meda = IntMap.find code a.meds 
        and medb = IntMap.find code b.meds in
        
        let medab : meds_t = Genome.find_meds2 meda medb in 
        
        let cost, recost = Genome.cmp_min_pair_cost medn medab in 
        acc_cost + cost, acc_recost + recost
    in
    let cost, _ =     IntMap.fold cost_calculator b.meds (0,0) in 
    float_of_int cost



let f_codes s c = 
    let check x = All_sets.Integers.mem x c in
    let adder x y acc = 
        if check x then IntMap.add x y acc 
        else acc
    in
    let n_meds = IntMap.fold adder s.meds IntMap.empty
    and n_costs = IntMap.fold adder s.costs IntMap.empty in
    { s with meds = n_meds; costs = n_costs}

let f_codes_comp s c = 
    let check x = not (All_sets.Integers.mem x c) in
    let adder x y acc = 
        if check x then IntMap.add x y acc 
        else acc
    in
    let n_meds = IntMap.fold adder s.meds IntMap.empty
    and n_costs = IntMap.fold adder s.costs IntMap.empty in
    { s with meds = n_meds; costs = n_costs }


let compare_data a b =
    let comparator code medb acc =
        if acc = 0 then begin
            let meda = IntMap.find code a.meds in
            Genome.compare meda medb
        end else acc
    in
    IntMap.fold comparator b.meds 0


let to_formatter attr t (parent_t : t option) d : Tags.output list = 
    let output_genome code med acc =
        let cost, recost = 
            match parent_t with
            | None -> "0", "0"
            | Some parent ->
                  let parent_med = IntMap.find code parent.meds in 

                  let cost, recost = Genome.cmp_min_pair_cost med parent_med in 

                  string_of_int cost, string_of_int recost
 
        in  
        let collect acc (med :  GenomeAli.med_t) = 
            let chrom_arr = GenomeAli.get_chroms med in 
            let chromStr_arr = Array.map 
                (fun chrom -> Sequence.to_formater chrom Alphabet.nucleotides) chrom_arr 
            in
            let genome = String.concat " @@ " (Array.to_list chromStr_arr) in 
            let name = Data.code_character code d in 

            let map = GenomeAli.genome_map_to_string med in 

            let attributes = 
                (Tags.Characters.name, name) ::                    
                (Tags.Characters.cost, cost) ::
                (Tags.Characters.recost, recost) ::
                (Tags.Characters.ref_code, string_of_int med.GenomeAli.genome_ref_code)::
                (Tags.Characters.chrom_map, map)::
                attr
            in
            let contents = `String genome in
            let acc = (Tags.Characters.genome, attributes, contents) :: acc in

            acc
        in

        List.fold_left collect acc med.Genome.med_ls 
    in

    let f = IntMap.fold output_genome t.meds [] in 
    f



