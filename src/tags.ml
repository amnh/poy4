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

let () = SadmanOutput.register "Tags" "$Revision: 1819 $"

type tag = string
type value = tag
type attribute = tag * value
type attributes = attribute list
type output = 
    (tag * attributes * [`String of string | `Structured of output Sexpr.t])

let make tag atv out = (tag, atv, out)

let remove_non_alpha_numeric str =
    Str.global_replace (Str.regexp "[^0-9a-zA-Z]") "_" str

let to_xml fo item =
    let output_attrs (fo : string -> unit) (a, b) =
        fo (remove_non_alpha_numeric a);
        fo "=\"";
        fo b;
        fo "\" "
    in

    let rec to_xml fo ((tag, attributes, contents) : output) : unit = 
        fo " <";
        fo (remove_non_alpha_numeric tag);
        fo " ";
        List.iter (output_attrs fo) attributes;
        fo ">@\n";
        begin match contents with
        | `String x -> 
              fo x; 
        | `Structured x ->
                Sexpr.leaf_iter (to_xml fo) x;
        end;
        fo " </"; fo tag; fo ">@\n"
    in
    fo "@[<h>";
    to_xml fo item;
    fo "@]%!"


module Characters = struct
    let suffix = " Character"
    let character = "Character"
    (* The characters themselves *)
    let additive = "Additive" ^ suffix
    let nonadditive = "Non Additive" ^ suffix
    let molecular = "Molecular" ^ suffix
    let sankoff = "Sankoff" ^ suffix
    let set = "Set" ^ suffix

    (* Their attributes *)
    let name = "Name"
    let cost = "Cost"
    let recost = "Rearrangment Cost"
    let weight = "Weight"
    let cclass = "Class"
    let alphabet = "Alphabet"
    let words = "Words"
    let ints = "Integers"
    let chars = "Character Functions"

    (* Their values *)
    let min = "Min"
    let max = "Max"
    let value = "Value"
    let sequence = "Sequence"
    let annchrom = "Annotated chromosome"
    let chromosome = "Chromosome"
    let genome = "Genome"
    let breakinv = "Breakinv"

    (* The cost matrices and calculation methods *)
    let fixed_states = "Fixed States"
    let tcm = "Transformation Cost Matrix"
    let state = "State"
    let states = "States"
    let ref_code = "Reference code"
    let chrom_map = "Pairwise_Alignment_Map"
end

module Nodes = struct
    let node = "Node"
    let preliminary = "Preliminary"
    let single = "Single"
    let final = "Final"
    let cost = Characters.cost
    let recost = Characters.recost
    let node_cost = "Node cost"
    let name = Characters.name
    let nce = "Number of Children Edges"
    let notu = "Number of OTUS"
    let child1_name = "Child1's name"
    let child2_name = "Child2's name"
end

module Trees = struct
    let forest = "Forest"
    let tree = "Tree"
    let cost = Nodes.cost    
    let recost = Nodes.recost
end

module Data = struct
    let cost_matrix = "Cost Matrix"
    let trees = "Trees"
    let data = "Data"
    let synonyms = "Synonyms"
    let synonym = "Synonym"
    let value =  Characters.value
    let code = "Code"
    let name = Nodes.name
    let taxon = "Taxon"
    let taxa = "Taxa"
    let file_contents = "File Contents"
    let filename = "File Name"
    let file = "File"
    let files = "Files"
    let ignored_taxa = "Ignored Taxa"
    let ignored_characters = "Ignored Characters"
    let characters = "Characters"
end

module GenomeMap = struct
    let genome = "GenomeMap"
    let chrom = "ChromosomeMap"
    let seg = "SegmentMap"
    let ref_code = "ReferenceCode"
    let seq_order = "SequenceOrder"
    let start_seg = "StartPosition"
    let end_seg = "EndPosition"
    let dir_seg = "Direction"        
end
