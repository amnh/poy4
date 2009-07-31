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


type datatype = 
     DStandard | Dna | Rna | Nucleotide | Protein | Continuous 

type item = 
     Min | Max | Median | Average | Variance | Stderror | SampleSize |
    States 

type statesformat =
     StatesPresent | Individuals | Count | Frequency 

type triangleformat =  Lower | Upper | Both 

type format_options =  
    | Datatype of datatype  
    | RespectCase
    | FMissing of string
    | Gap of string
    | Symbols of string  (** still need to parse the symbols *)
    | Equate of string   (** still need to parse the tuples *)
    | MatchChar of string
    | Labels of bool
    | Transpose
    | Interleave
    | Items of item
    | StatesFormat of statesformat
    | Triangle of triangleformat
    | Tokens of bool 

type charset = 
    Range of (string * string option) | Single of string | Name of string

type char_data = {
    char_taxon_dimensions : string option;
    char_char_dimensions : string;
    char_format : format_options list;
    char_eliminate : charset option;
    char_taxlabels : string list;
    char_statelabels : (string * string * string list) list;
    char_charlabels : string list;
    char_charstates : (string * string list) list;
    chars : string;
}

type unalg_data = {
    unal_taxon_dimensions : string option;
    unal_format : format_options list;
    unal : string;
}

type source =  Inline | File | Resource 

type pictureformat =  Pict | Tiff | Eps | Jpeg | Gif 

type pictureencoding =  None | UUEncode | BinHex 
type polytcount =  MinSteps | MaxSteps 
type gapmode =  Missing | NewState 

type user_type =  StepMatrix of (string * string list) | CSTree of string 

type standard_item = 
     Code of (string * charset list) | IName of (string * charset list) 

type standard_list =
    STDVector of string list | STDStandard of charset list

type set_type =  Standard of standard_item list | Vector of string list 

type set_pair =  
    | TaxonSet of charset list
    | CharacterSet of charset list
    | StateSet of charset list
    | TreeSet of charset list
    | CharPartition of set_type
    | TaxPartition of set_type
    | TreePartition of set_type

type assumption_set = (bool * string * bool * set_type)

type assumption_items = 
     Options of (string option * polytcount * gapmode)
    | UserType of (string * user_type)
    | TypeDef of assumption_set
    | WeightDef of assumption_set
    | ExcludeSet of (bool * string * standard_list)
    | AncestralDef of assumption_set

type likelihood_model = 
    | Model of string
    | Variation of string
    | Variation_Sites of string
    | Variation_Alpha of string
    | Variation_Invar of string
    | Priors of (string * float) list
    | Chars of charset list
    | Parameters of float list
    | GapMode of string
    | Files of string

type poy_data =          (* trees , characters, (nodes , length) *)
    | CharacterBranch of string list * charset list * (string * float) list
    | Likelihood of likelihood_model list
    | Tcm of (bool * string * standard_item list)
    | GapOpening of (bool * string * standard_item list)
    | DynamicWeight of (bool * string * standard_item list)

type block = 
     Taxa of (string * string list) 
    | Characters of char_data 
    | Distances of ((bool * string * string) option * format_options list * string
    list * string)
    | Ignore of string
    | Sets of (string * set_pair) list
    | Unaligned of unalg_data
    | Trees of ((string * string) list * string list) 
    | Notes of ((set_pair list * source * string) option * (set_pair list *
    pictureformat option * pictureencoding option * source * string) option) 
    | Assumptions of assumption_items list 
    | Error of string
    | UnknownBlock of string
    | Poy of poy_data list

type tree = 
    | Leaf of (string * float option)
    | Node of (tree list * string option * float option)
