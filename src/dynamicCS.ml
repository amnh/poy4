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

let () = SadmanOutput.register "AllDirChar" "$Revision: 1616 $"
(** A Dynamic Character Set implementation *)
exception Illegal_Arguments
let () = SadmanOutput.register "DynamicCS" "$Revision: 1006 $"


module IntMap = All_sets.IntegerMap
module IntSet = All_sets.Integers

exception No_Union

(** A dynamic character type. 'a can be SeqCS, ChromCS, BreakCS...*)
type t = 
    | SeqCS  of SeqCS.t
    | BreakinvCS of  BreakinvCS.t 
    | ChromCS of ChromCS.t 
    | AnnchromCS of AnnchromCS.t
    | GenomeCS of GenomeCS.t

type u = 
    | U_SeqCS of SeqCS.u
    | U_Others


let failwith_todo f_name = 
    failwith 
        ("Todo: " ^ f_name ^ " dynamicCS")

let alpha (a : t) = 
    match a with 
    | SeqCS a -> a.SeqCS.alph
    | ChromCS a -> a.ChromCS.alph
    | GenomeCS a -> a.GenomeCS.alph
    | BreakinvCS a -> a.BreakinvCS.alph
    | AnnchromCS a -> a.AnnchromCS.alph

let total_cost (a : t) = 
    match a with 
    | SeqCS a -> a.SeqCS.total_cost
    | ChromCS a -> a.ChromCS.total_cost
    | GenomeCS a -> a.GenomeCS.total_cost
    | BreakinvCS a -> a.BreakinvCS.total_cost
    | AnnchromCS a -> a.AnnchromCS.total_cost

let total_recost (a : t) = 
    match a with 
    | SeqCS a -> 0.
    | ChromCS a -> a.ChromCS.total_recost
    | GenomeCS a -> a.GenomeCS.total_recost
    | BreakinvCS a -> a.BreakinvCS.total_recost
    | AnnchromCS a -> a.AnnchromCS.total_recost


let subtree_recost (a : t) = 
    match a with 
    | SeqCS a -> 0.0
    | ChromCS a -> a.ChromCS.subtree_recost
    | GenomeCS a -> a.GenomeCS.subtree_recost
    | BreakinvCS a -> a.BreakinvCS.subtree_recost
    | AnnchromCS a -> a.AnnchromCS.subtree_recost


let c2 (a : t) = 
    match a with 
    | SeqCS a -> a.SeqCS.c2
    | ChromCS a -> a.ChromCS.c2
    | GenomeCS a -> a.GenomeCS.c2
    | BreakinvCS a -> a.BreakinvCS.c2
    | AnnchromCS a -> a.AnnchromCS.c2


let chrom_pam (a : t) = 
    match a with 
    | ChromCS a -> a.ChromCS.chrom_pam
    | _ -> Data.dyna_pam_default 


let state (a : t) : Data.dyna_state_t= 
    match a with 
    | SeqCS a -> `Seq
    | ChromCS a -> `Chromosome
    | GenomeCS a -> `Genome
    | BreakinvCS a -> `Annotated
    | AnnchromCS a -> `Breakinv


let code (a : t) = 
    match a with 
    | SeqCS a -> a.SeqCS.code
    | ChromCS a -> a.ChromCS.code
    | GenomeCS a -> a.GenomeCS.code
    | BreakinvCS a -> a.BreakinvCS.code
    | AnnchromCS a -> a.AnnchromCS.code


let leaf_sequences (a : t) = 
    match a with 
    | SeqCS a -> a.SeqCS.sequences
    | ChromCS a -> 
          let sequences = 
              IntMap.map 
                  (fun med  -> 
                       (List.hd med.Chrom.med_ls).ChromAli.seq 
                  ) a.ChromCS.meds
          in 
          sequences
    | _ -> failwith_todo "sequences in DynamicCS.ml"

let unions (a : u) = 
    match a with 
    | U_SeqCS (Some a) -> a.SeqCS.unions
    | _ -> failwith "DynamicCS.unions"



let reprioritize a b =
    match a, b with 
    | SeqCS a, SeqCS b -> SeqCS (SeqCS.reprioritize a b)
    | _, _ -> failwith "DynamicCS.reprioritize"


let prioritize a = 
    match a with 
    | SeqCS a -> SeqCS (SeqCS.prioritize a)
    | s -> s

let to_union a = 
    match a with 
    | SeqCS a -> U_SeqCS (SeqCS.to_union a)
    | _ -> U_Others

let union a b c =
    match a, b, c with
    | SeqCS a, U_SeqCS b, U_SeqCS c -> U_SeqCS (SeqCS.union a b c)
    | SeqCS _, _, _ -> failwith "DynamicCS.union"
    | _, U_Others, U_Others -> U_Others
    | _, _, _ -> failwith_todo "union"


let cardinal_union a =
    match a with
    | U_SeqCS a -> SeqCS.cardinal_union a
    | U_Others -> 0

let poly_saturation x v =
    let polyacc, polylen =
        match x with
        | U_SeqCS x ->
                let card = SeqCS.cardinal_union x in
                let poly = SeqCS.poly_saturation x v in
                (int_of_float (poly *. (float_of_int card))), 
                card
        | U_Others -> 0, 0
    in
    (float_of_int polyacc) /. (float_of_int polylen)

let of_array spec genome_arr code taxon num_taxa = 
    match spec.Data.state with
    | `Seq | `Breakinv | `Chromosome as meth ->
            let seq_arr = 
                Array.map 
                (fun (genome_data, genome_code) ->
                    let first_seq = genome_data.Data.seq_arr.(0).Data.seq in  
                    (first_seq, genome_code)) genome_arr
            in 
            begin match meth with
            | `Seq -> 
                    let t = SeqCS.of_array spec seq_arr code in
                    SeqCS t
            | `Breakinv -> 
                    let t = BreakinvCS.of_array spec seq_arr code in
                    BreakinvCS t
            | `Chromosome  ->
                    let t = 
                        ChromCS.of_array spec seq_arr code taxon num_taxa 
                    in 
                    ChromCS t
            end
    | `Annotated -> 
          let t = AnnchromCS.of_array spec genome_arr code  taxon num_taxa in
          AnnchromCS t 
    | `Genome  ->
            let t = 
                GenomeCS.of_array spec genome_arr code taxon num_taxa 
            in 
            GenomeCS t 


let of_list spec genome_ls =
    of_array spec (Array.of_list genome_ls) 

let median a b =
    match a, b with 
    | SeqCS a, SeqCS b -> SeqCS (SeqCS.median a b)
    | ChromCS a, ChromCS b -> ChromCS (ChromCS.median2 a b)
    | GenomeCS a, GenomeCS b -> GenomeCS (GenomeCS.median2 a b)
    | BreakinvCS a, BreakinvCS b -> BreakinvCS (BreakinvCS.median2 a b)
    | AnnchromCS a, AnnchromCS b -> AnnchromCS (AnnchromCS.median2 a b)
    | _, _ -> failwith_todo "median"

let median_3 p n c1 c2 =
    match p, n, c1, c2 with 
    | SeqCS p, SeqCS n, SeqCS c1, SeqCS c2 -> 
          SeqCS (SeqCS.median_3 p n c1 c2)
    | BreakinvCS p, BreakinvCS n, BreakinvCS c1, BreakinvCS c2 -> 
          BreakinvCS (BreakinvCS.median3 p n c1 c2)
    | AnnchromCS p, AnnchromCS n, AnnchromCS c1, AnnchromCS c2 -> 
          AnnchromCS (AnnchromCS.median3 p n c1 c2)
    | ChromCS p, ChromCS n, ChromCS c1, ChromCS c2 -> 
          ChromCS (ChromCS.median3 p n c1 c2)

    | GenomeCS p, GenomeCS n, GenomeCS c1, GenomeCS c2 -> 
          GenomeCS (GenomeCS.median3 p n c1 c2)
    | _, _, _, _ -> failwith_todo "median_3"


let distance_of_type t a b =
    let has_t x = List.exists (fun z -> z = x) t in
    let has_seq = has_t `Seq 
    and has_chrom = has_t `Chrom 
    and has_gen = has_t `Genome
    and has_break = has_t `Breakinv
    and has_ann = has_t `Annchrom in
    match a, b with
    | SeqCS a, SeqCS b when has_seq -> (SeqCS.distance a b)
    | ChromCS a, ChromCS b when has_chrom -> ChromCS.distance a b  
    | GenomeCS a, GenomeCS b when has_gen -> GenomeCS.distance a b  
    | BreakinvCS a, BreakinvCS b when has_break -> BreakinvCS.distance a b  
    | AnnchromCS a, AnnchromCS b when has_ann -> AnnchromCS.distance a b  
    | _, _ -> 0.0

let distance a b = 
    match a, b with   
    | SeqCS a, SeqCS b -> (SeqCS.distance a b)
    | ChromCS a, ChromCS b -> ChromCS.distance a b  
    | GenomeCS a, GenomeCS b -> GenomeCS.distance a b  
    | BreakinvCS a, BreakinvCS b -> BreakinvCS.distance a b  
    | AnnchromCS a, AnnchromCS b -> AnnchromCS.distance a b  
    | _, _ -> failwith_todo "distance"  



let distance_union a b =
    match a, b with
    | U_SeqCS a, U_SeqCS b -> SeqCS.distance_union a b
    | U_Others, U_Others -> 0.0
    | _, _ -> failwith "DynamicCS.distance_union"


let to_string a =
    match a with 
    | SeqCS a -> SeqCS.to_string a
    | BreakinvCS a -> BreakinvCS.to_string a
    | ChromCS a -> ChromCS.to_string a
    | GenomeCS a -> GenomeCS.to_string a
    | AnnchromCS a -> AnnchromCS.to_string a



let dist_2 delta n a b =
    match n, a, b with 
    | SeqCS n, SeqCS a, SeqCS b -> (SeqCS.dist_2 delta n a b)
    | ChromCS n, ChromCS a, ChromCS b -> ChromCS.dist_2 n a b
    | GenomeCS n, GenomeCS a, GenomeCS b -> GenomeCS.dist_2 n a b
    | BreakinvCS n, BreakinvCS a, BreakinvCS b -> BreakinvCS.dist_2 n a b
    | AnnchromCS n, AnnchromCS a, AnnchromCS b -> AnnchromCS.dist_2 n a b
    | _, _, _ -> failwith_todo "dist_2"



let f_codes s c = 
    match s with 
    | SeqCS s -> SeqCS (SeqCS.f_codes s c)
    | ChromCS s -> ChromCS (ChromCS.f_codes s c)
    | GenomeCS s -> GenomeCS (GenomeCS.f_codes s c)
    | BreakinvCS s -> BreakinvCS (BreakinvCS.f_codes s c)
    | AnnchromCS s -> AnnchromCS (AnnchromCS.f_codes s c)



let f_codes_comp s c = 
    match s with 
    | SeqCS s -> SeqCS (SeqCS.f_codes_comp s c)
    | ChromCS s -> ChromCS (ChromCS.f_codes_comp s c)
    | GenomeCS s -> GenomeCS (GenomeCS.f_codes_comp s c)
    | BreakinvCS s -> BreakinvCS (BreakinvCS.f_codes_comp s c)
    | AnnchromCS s -> AnnchromCS (AnnchromCS.f_codes_comp s c)



let compare_data a b =
    match a, b with 
    | SeqCS a, SeqCS b -> SeqCS.compare_data a b
    | ChromCS a, ChromCS b -> ChromCS.compare_data a b
    | GenomeCS a, GenomeCS b -> GenomeCS.compare_data a b
    | BreakinvCS a, BreakinvCS b -> BreakinvCS.compare_data a b
    | AnnchromCS a, AnnchromCS b -> AnnchromCS.compare_data a b
    | _, _ -> failwith_todo "compare_data"

let rec compare_union a b = 
    match a, b with
    | (U_SeqCS ha), (U_SeqCS hb) -> SeqCS.compare_union ha hb 
    | U_Others, U_Others -> 0
    | _, _ -> failwith "DynamicCS.compare_union"


let to_formatter ref_codes attr t (parent_t : t option) d : Tags.output list = 
    match t, parent_t with 
    | SeqCS t, _ -> SeqCS.to_formatter attr t d 
    | ChromCS t, _  -> begin 
          match parent_t with 
          | None ->  ChromCS.to_formatter ref_codes attr t None d
          | Some (ChromCS parent_t) -> ChromCS.to_formatter ref_codes attr t (Some parent_t) d
          | _ -> failwith "to_formatter in dynamicCS"
      end 

    | AnnchromCS t, __ -> begin
          match parent_t with 
          | None ->  AnnchromCS.to_formatter ref_codes attr t None d
          | Some (AnnchromCS parent_t) -> AnnchromCS.to_formatter ref_codes attr t (Some parent_t) d
          | _ -> failwith "to_formatter in dynamicCS"
      end 

    | BreakinvCS t, __ -> begin
          match parent_t with 
          | None ->  BreakinvCS.to_formatter ref_codes attr t None d
          | Some (BreakinvCS parent_t) ->BreakinvCS.to_formatter ref_codes attr t (Some parent_t) d
          | _ -> failwith "to_formatter in dynamicCS"
      end 
 

    | GenomeCS t,  _ -> begin
          match parent_t with
          | None -> GenomeCS.to_formatter attr t None d
          | Some (GenomeCS parent_t) -> GenomeCS.to_formatter attr t (Some parent_t) d
          | _ -> failwith "to_formatter in dynamicCS"
      end 




let tabu_distance a_final b_final = 
    match a_final, b_final with 
    | _, SeqCS b_final -> (SeqCS.tabu_distance b_final)
    | ChromCS a_final, ChromCS b_final -> ChromCS.max_distance a_final b_final
    | GenomeCS a_final, GenomeCS b_final -> GenomeCS.max_distance a_final b_final
    | BreakinvCS a_final, BreakinvCS b_final -> BreakinvCS.max_distance a_final b_final
    | AnnchromCS a_final, AnnchromCS b_final -> AnnchromCS.max_distance a_final b_final
    | _, _ -> failwith_todo "tabu_distance"


let get_active_ref_code t = 
    match t with
    | ChromCS t -> ChromCS.get_active_ref_code t
    | AnnchromCS t -> AnnchromCS.get_active_ref_code t
    | BreakinvCS t -> BreakinvCS.get_active_ref_code t
    | _ -> IntSet.empty, IntSet.empty



let cardinal x =
    match x with
    | SeqCS x -> SeqCS.cardinal x
    | ChromCS x -> ChromCS.cardinal x
    | GenomeCS x -> GenomeCS.cardinal x
    | BreakinvCS x -> BreakinvCS.cardinal x
    | AnnchromCS x -> AnnchromCS.cardinal x

let get_sequence_union code x = 
    match x with
    | U_SeqCS x -> SeqCS.get_sequence_union code x
    | U_Others -> failwith "DynamicCS.get_sequence_union"

let encoding enc x =
    match x with
    | SeqCS x -> SeqCS.encoding enc x
    | _ -> failwith "Unsupported DynamicCS.encoding"

let readjust ch1 ch2 parent mine =
    match ch1, ch2, parent, mine with
    | SeqCS ch1, SeqCS ch2, SeqCS parent, SeqCS mine -> 
            let new_cost, nc = SeqCS.readjust ch1 ch2 parent mine in
            let prev_cost = SeqCS.distance ch1 mine +. SeqCS.distance ch2 mine in
            prev_cost, new_cost, (SeqCS nc)
    | _, _, _, mine ->  
            let prev_cost = total_cost mine in
            prev_cost, prev_cost, mine


let to_single ?(is_root=false) ref_codes alied_map parent mine = 
    match parent, mine, alied_map with
    | SeqCS parent, SeqCS mine, _ -> 
            let prev_cost, new_cost, median = SeqCS.to_single parent mine in
            prev_cost, new_cost, SeqCS median

    | ChromCS parent, ChromCS mine, ChromCS alied_map ->
            let prev_cost, new_cost, median =
                ChromCS.to_single ~is_root:is_root ref_codes alied_map parent mine 
            in
            prev_cost, new_cost, ChromCS median          


    | BreakinvCS parent, BreakinvCS mine, BreakinvCS alied_map ->
            let prev_cost, new_cost, median = 
                BreakinvCS.to_single ~is_root:is_root ref_codes alied_map parent mine 
            in
            prev_cost, new_cost, BreakinvCS median          


    | AnnchromCS parent, AnnchromCS mine, AnnchromCS alied_map ->
          let prev_cost, new_cost, median = 
              AnnchromCS.to_single ~is_root:is_root ref_codes alied_map parent mine 
          in
          prev_cost, new_cost, AnnchromCS median          


    | _, mine, _ -> 

            let cst = total_cost mine in
            cst, cst, mine

let to_single_root ref_codes mine = to_single ref_codes mine mine mine
