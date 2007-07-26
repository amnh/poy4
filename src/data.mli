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

exception Illegal_argument

(** This type is needed for the dynamic homologies datasets. Contains all the
 * valid options to perform a dynamic homology analysis. *)
type dynhom_opts = 
    | Tcm of string     (** A transformation cost matrix to be used *)
    | Fixed of string option  (** A fixed states list to be used, could be a filename 
                        or a sequence directly typed in the input. *)

(* The valid contents of an input file. These are not all of them, add others if
* needed, these are all that we need for now. *)
type contents = 
    | Characters  (** Terminal characters *)
    | CostMatrix  (** A transformation cost matrix *)
    | Trees       (** Trees *)

type dyna_state_t = [
(** A short sequence, no rearrangements are allowed*)
| `Seq
(** A long sequence, genes inside are broken down
 * automatically, rearrangements are allowed*)
| `Chromosome

| `Genome 

(** A list of shorted sequences 
 * annotated by piles, rearrangements are allowed *)
| `Annotated

(** A sequence of gene names, rearrangements are allowed *)
| `Breakinv ]


val median_code_count : int ref

type re_meth_t = [ (* The cost of a rearrangement event is the argument *)
    | `Breakpoint of int    
    | `Inversion of int ]

type dyna_pam_t = {
    seed_len : int option;  (* The minimum length of a perfect match to start
    the detection of a homologous segment between a pair of chromosomes *)
    re_meth : re_meth_t option;
    circular : int option; (* 0 is false, 1 is true, integer due to the grappa
    interface requirements *)
    (** [(a, b)] where [a] is the opening cost and [b] is the extension cost *)
    locus_indel_cost : (int * int) option; 
    (** [(a, b)] where [a] is the opening cost and [b] is the extension cost *)
    chrom_indel_cost : (int * int) option; 
    chrom_hom : int option;
    chrom_breakpoint : int option;
    sig_block_len : int option;
    rearranged_len : int option;

    keep_median : int option; 
                    (* The number of medians to be kept in an internal node *)
    swap_med : int option;
                    (* Number of rounds of swapping that is to be used in the
                    * rearranged median search. If None, the default is 1 (see
                    * the chromosome side). *)
    approx : bool option; (* Convert the chromosomes into Sankoff characters *)
}

type dynamic_hom_spec = {
    filename : string;
    fs : string;
    tcm : string;
    tcm2d : Cost_matrix.Two_D.m;
    tcm3d : Cost_matrix.Three_D.m;
    alph : Alphabet.a;
    pool : Sequence.Pool.p;
    state : dyna_state_t;
    pam : dyna_pam_t;
    weight : float;
}

type distr =
    | Items of IntSpec.func
    | MaxLength of (int * IntSpec.func)
                        (* Any of the distributions with a maximum length *)

type affine_f = {
    selfp : float; (* The natural logarithm of the probability *)
    distr : distr; (* The probability distribution *)
}

type model = 
    | InDels of (float * float)
    | InDelSub of (float * float * float)
    | Subs of float
    | AffInDelSub of (affine_f * affine_f * float)
    | AffInDelAffSub of (affine_f * affine_f * affine_f)

type arr = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

type spec = {
    dhs : dynamic_hom_spec;
}

(** A character specification. Contains the information regarding the
 * characteristics of a particular character *)
type specs = 
    (** Static homology characters, includes the encoding specs from the
    * parser and the name of the file it comes from (the name includes the
    * column number). *)
    | Static of (Parser.Hennig.Encoding.s * string)
    (** A dynamic homology based character type, with three parameters, the
    * file name containing the set of sequences, the filename of the valid 
    * fixed states that can be used for that set of sequences, and the file
    * containing the transformation cost matrix to be used to perform their
     * alignments.  Also, we store the alphabet used. *)
    | Dynamic of dynamic_hom_spec
    | Set 


(** [specified] tells us whether or not the user left out the given character
    in the current taxon *)
type specified = [ `Specified | `Unknown ]


type bool_characters = [
    | `All
    | `Some of (bool * int list)
    | `Names of (bool * string list)
    | `Random of float
    | `AllStatic
    | `AllDynamic
    | `Missing of (bool * int)
]



type characters = [
    | `All
    | `Some of int list 
    | `Names of string list
    | `Random of float
    | `AllStatic
    | `AllDynamic
    | `Missing of (bool * int)
]



type 'a seq_t = {
    seq : 'a;
    code : int;
}

type 'a dyna_data = {
    (** typically 'a is a sequence, the integer followed is the code of 'a *)
    seq_arr : ('a seq_t) array;
}


(* A valid loaded character from a file *)
type cs_d = 
    (* A dynamic homology, containing its code, the sequence, the
    * transformation cost matrix and its three dimensional transformation
    * cost matrix *)
    |  Dyna of (int * Sequence.s dyna_data)

    (* A static homology character, containing its code, and the character
    * itself *)
    | Stat of (int * Parser.t)

type cs = cs_d * specified

(** A transformation cost matrix for Sankoff characters *)
type tcm = int array array

module OutputInformation : sig
    type treelengths_information = [
        | `Minimum
        | `Maximum
        | `Summary
    ]

    type file_information = [
        | `Filename 
    ]

    type character_information = [ `Type ]

    type search_script_information = [
        | `Done
        | `ToDo
        | `Filename
        | `Current
    ]

    type t = [
        | `TreeInformation of [ treelengths_information | `Number ] list
        | `TaxonInformation
        | `CharacterInformation of character_information list
        | `FileInformation of file_information list
        | `SearchInformation of search_script_information list
        | `Timer
    ]
end

type alph = 
    | Nucleotides
    | Aminoacids
    | GeneralAlphabet of 
        (string * Cost_matrix.Two_D.m * Cost_matrix.Three_D.m * Alphabet.a)


type d = {
    (** The pairs of synonyms for the loaded taxa *)
    synonyms : string All_sets.StringMap.t;
    do_fs : bool;
    (** The current fixed states list to be used in whatever sequence is
    * loaded next in the dataset *)
    current_fs : Sequence.s list;
    (** The name of the fixed states file that contained the contents of
    * current_fs. *)
    current_fs_file : string;
    (** The transformation cost matrix to be used in the alignments of
    * whatever sequence is loaded next in the dataset *)
    current_tcm : Cost_matrix.Two_D.m;
    (** The three dimentional cost matrix to be used in the alignments of
    * whatever sequence is loaded next in the dataset. This is built based
    * on the [current_tcm]. *)
    current_tcm3 : Cost_matrix.Three_D.m;
    (** The name of the file containing the [current_tcm] being used. *)
    current_tcm_file : string;
    current_alphabet_file : string;
    current_alphabet : alph;
    use_annotated_sequences : bool;

    (** A function to generate codes for character *)
    character_code_gen : int ref;

    (* A function to generate codes for segments in chromosomes *)
    seg_code_gen : int ref;
    (* Set of files where each taxon appears *)
    taxon_files : All_sets.Strings.t All_sets.StringMap.t;
    (** A map between taxon names and their assigned codes *)
    taxon_names : int All_sets.StringMap.t;
    (** A map between the taxon codes and their corresponding names *)
    taxon_codes : string All_sets.IntegerMap.t;
    (** A map of each taxon code and their corresponding character list *)
    taxon_characters : (int, (int, cs) Hashtbl.t) Hashtbl.t;
    (* A map between the character names and their corresponding codes *)
    character_names : (string, int) Hashtbl.t;
    (* A map between the character codes and their corresponding names *)
    character_codes : (int, string) Hashtbl.t;
    (* A map between the character codes and their specifications *)
    character_specs : (int, specs) Hashtbl.t;
    (** The set of taxa to be ignored in the analysis *)
    ignore_taxa_set : All_sets.Strings.t;
    (* The set of taxa to be ignored in the analysis *)
    ignore_character_set : string list;
    trees : string Parser.Tree.t list list;
    non_additive_8 : int list;
    non_additive_16 : int list;
    non_additive_32 : int list;
    non_additive_33 : int list;
    additive : int list;
    sankoff : int list list;
    dynamics : int list;

    complex_schema : Parser.SetGroups.t list;
    (** Tree for how to arrange taxa into complex terminals *)

    files : (string * contents list) list;
    specification_index : SpecIndex.t;
    character_index : (string * CharacSpec.t) list;
    search_information : OutputInformation.t list;

    (** At what taxon to root output trees *)
    root_at : int option;
}

(** [empty ()] creates a fresh empty dataset. *)
val empty : unit -> d

val duplicate : d -> d
(** [add_character_spec s c x] returns fresh [d] generated by adding the
 * specification [s] to [x] with code [c]. *)
val add_character_spec : specs -> int -> d -> d

(** [to_channel ch b] outputs the specification of characters and loaded
 * characters of [b] in a human readable manner in channel [ch]. *)
val to_channel : out_channel -> d -> unit

(** [code_taxon c d] retrieves the name associated with a taxon code [c] in the
 * data [d]. *)
val code_taxon : int -> d -> string

(** [taxon_code n d] finds the code assigned to taxon [n] in the dataset 
 * [d]. *)
val taxon_code : string -> d -> int

val character_code : string -> d -> int

val code_character : int -> d -> string

val get_tcm : int -> d -> tcm

val get_weight : int -> d -> float

val process_parsed_sequences : 
    Alphabet.a -> string -> dyna_state_t -> d -> 
    (Sequence.s list list list * Parser.taxon) list -> d
            

val process_molecular_file : bool -> dyna_state_t -> d -> Parser.filename -> d

val add_static_file : ?report:bool -> d -> Parser.filename -> d

val process_tcm : d -> Parser.filename -> d

val process_trees : d -> Parser.filename -> d

val process_fixed_states : d -> Parser.filename option -> d

val add_synonyms_file : d -> Parser.filename -> d

val add_synonym : d -> (string * string) -> d

val process_dna_sequences : d -> (Parser.filename list * dynhom_opts list option)  -> d

val process_ignore_file : d -> Parser.filename -> d

val process_ignore_taxon : d -> string -> d

val process_analyze_only_file : bool -> d -> Parser.filename list -> d

val number_of_taxa : d -> int

val process_analyze_only_taxa : 
    [`Random of float | `Names of (bool * string list) | `Missing of (bool * int) ] -> d -> d

val categorize : d -> d

val remove_taxa_to_ignore : d -> d

val get_sequence_tcm : int -> d -> Cost_matrix.Two_D.m

val get_sequence_alphabet : int -> d -> Alphabet.a

val get_files : d -> (string * contents list) list

val add_file : d -> contents list -> Parser.filename -> d

val get_taxa : d -> string list

val add_static_parsed_file : d -> string -> 
    Parser.Hennig.Encoding.s array * (Parser.t array * string) list *
        string Parser.Tree.t list list -> d

val add_multiple_static_parsed_file : d -> (string *
    (Parser.Hennig.Encoding.s array * (Parser.t array * string) list *
        string Parser.Tree.t list list)) list -> d

val get_used_observed : int -> d -> (int, int) Hashtbl.t

val characters_to_formatter :  d -> Tags.output

val character_spec_to_formatter : specs -> Tags.output

val ignored_taxa_to_formatter : d -> Tags.output

val files_to_formatter : d -> Tags.output

val taxon_names_to_formatter : d -> Tags.output

val synonyms_to_formatter : d -> Tags.output

val to_formatter : Tags.attributes -> d -> Tags.output 

val get_code_from_characters_restricted :
    [ `Dynamic |  `NonAdditive |
        `Additive | `Sankoff | `AllStatic | `AllDynamic ] ->
             d -> characters -> int list

val get_code_from_characters_restricted_comp :
    [ `Dynamic |  `NonAdditive |
        `Additive | `Sankoff | `AllStatic | `AllDynamic ] ->
             d -> bool_characters -> int list

val transform_dynamic :
  Methods.dynamic_char_transform ->
  d -> d

val transform_chrom_to_rearranged_seq :
  d ->
  Methods.dynamic_char_transform ->
    'c -> (int * int array array All_sets.IntegerMap.t list) list list list -> d

val print : d -> unit

val get_chars_codes : d -> characters -> int list
val get_chars_codes_comp : d -> bool_characters -> int list

val process_ignore_characters : bool -> d -> characters -> d

val process_ignore_characters_file : bool -> d -> Parser.filename -> d

val complement_characters : d -> characters -> [ `All | `Some of int list | `Names of string list ]

val complement_taxa : d -> int list -> int list

val process_analyze_only_characters_file : bool -> bool -> d -> Parser.filename list -> d

val process_rename_characters : d -> (string * string) -> d

val assign_transformation_gaps :
    d -> bool_characters -> int -> int -> d

val assign_affine_gap_cost : 
    d -> bool_characters -> Cost_matrix.cost_model -> d

val assign_tail : 
    d -> bool_characters -> [ `File of Parser.filename | `Array of int array ] -> d

val assign_prepend : 
    d -> bool_characters -> [ `File of Parser.filename | `Array of int array ] -> d

val assign_tcm_to_characters_from_file :
    d -> bool_characters -> Parser.filename option -> d

val process_complex_terminals :
    d -> Parser.filename -> d

val get_pool : d -> int -> Sequence.Pool.p
val get_alphabet : d -> int -> Alphabet.a
val get_pam : d -> int -> dyna_pam_t
val get_character_state : d -> int -> dyna_state_t

val process_taxon_code :
    d -> All_sets.StringMap.key -> string -> d * int

val set_dyna_data : 'a seq_t array -> 'a dyna_data

val set_sequence_defaults : alph -> d -> d

val annotated_sequences : bool -> d -> d

val to_faswincladfile : d -> string option -> unit

val report_taxon_file_cross_reference : 
    bool_characters option -> d -> string option -> unit

val report_terminals_files : 
    string option -> All_sets.Strings.t All_sets.StringMap.t -> All_sets.Strings.t ->
        unit
val dyna_pam_default : dyna_pam_t


val get_empty_seq: Alphabet.a -> Sequence.s seq_t

val find_max_seq_id : d -> int
val flush : d -> unit

val transform_weight : [ `ReWeight of (bool_characters * float) | `WeightFactor of
(bool_characters * float) ] -> d -> d

val file_exists : d -> Parser.filename -> bool

val make_fixed_states : bool_characters -> d -> d

val has_dynamic : d -> bool 

val randomize_taxon_codes : Methods.terminal_transform -> d -> d * (int, int) Hashtbl.t

module Sample : sig
    val generate : d -> [ `Bootstrap | `Jackknife of int ] -> d
end

val prealigned_characters :   
    (Cost_matrix.Two_D.m -> Alphabet.a ->
              'a * ([> `Exists ] -> int -> Parser.t list -> Parser.t list) *
                 (int -> Parser.Hennig.Encoding.s list ->
                     Parser.Hennig.Encoding.s list)) ->
                           d -> bool_characters -> d

(** [compare_all_pairs a b c d] compare for each taxon the characters with code
* [a] and [b], making the reverse complement of [b] if [c] is true, as stored in
* the data structure [d], returning for each character an optional tuple holding
* the number of taxa with comparable characters, and their overal comparison
* index. If none of the taxa hold the required characters, [None] is returned. *)
val compare_all_pairs : int -> int -> bool -> d -> (int * float) option

(** [compare_pairs a b c d] calculates the compare_all_pairs for every pair of
* characters in [a] and [b], calculating the reverse complement of [b] if [c] is
* true, as stored in the structure [d]. The output is a list holding for each
* pair of character names, the comparison index (see [compare_all_pairs]). *)
val compare_pairs : bool_characters -> bool_characters -> bool -> d -> 
    (string * string * float) list

(** [sequence_statistics ch d] returns a list containing pairs consisting 
* of [(n, (w, x, y, z, d, e, f))], where [n] is the name of each character included in
* [ch], [w] is the maximum sequence length, [x] is the minimum sequence length,
* [y] is the sum of the length of all taxa for the character [n], [z] is the
* total number of characters included (taxa containing it), [d] is the maximum
* ammong the all pairs distances, [e] is the minimum ammong the all pairs
* distances, and [f] is the sum of all the distances. In total (z ^ 2 / 2 - z)
* distances are computed. *)
val sequence_statistics : bool_characters -> d -> (string * (int * int * int * int * int * int * int)) list
