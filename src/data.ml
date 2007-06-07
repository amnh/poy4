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

open StdLabels

exception Nam_Collision of (CharacSpec.t * string)
exception Illegal_argument

type filename = string 

module FullTupleMap = All_sets.FullTupleMap
module IntMap = All_sets.IntegerMap
    
let output_error = Status.user_message Status.Error

type dynhom_opts = 
    | Tcm of string     (* A transformation cost matrix to be used *)
    | Fixed of string option  (* A fixed states list to be used, could be a filename 
                        or a sequence directly typed in the input. *)

(** The valid types of contents of a file *)
type contents = Characters | CostMatrix | Trees 

type dyna_state_t = [
    (** A short sequence, no rearrangements are allowed*)
    | `Seq

    (** A long sequence, genes inside are broken automatically, 
    * rearrangements are allowed*)
    | `Chromosome

    | `Genome (** A set of chromosomes *)

    (** A list of shorted sequences annotated by piles, rearrangements are allowed *)
    | `Annotated

    (** A sequence of gene names, rearrangements are allowed *)
    | `Breakinv ]


type re_meth_t = [ `Breakpoint of int | 
                   `Inversion of int ]

type dyna_pam_t = {
    seed_len : int option;
    re_meth : re_meth_t option;
    circular : int option;
    locus_indel_cost : (int * int) option;
    chrom_indel_cost : (int * int) option;
    chrom_hom : int option;
    chrom_breakpoint : int option;
    sig_block_len : int option;
    keep_median : int option;
    swap_med : int option; 
    (** number iterations are applied 
    in refining alignments with rearrangements *)
    approx : bool option
}

let dyna_pam_default = {
    seed_len = None;
    re_meth = None;
    circular = None;
    locus_indel_cost = None;
    chrom_indel_cost = None;
    chrom_hom = None;
    chrom_breakpoint = None;
    sig_block_len = None;
    keep_median = None;
    swap_med = None;
    approx = None;
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
    selfp : float;
    distr : distr;
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
    (** A set of characters that could be of several types. Probably some
    * information is needed, but in the current implementation there is no
    * need. As we get to more complex character types, this will change. *)

    | Set 

(** [specified] tells us whether or not the user left out the given character
    in the current taxon *)
type specified = [ | `Specified | `Unknown ]

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
    | Dyna of (int * Sequence.s dyna_data)
    (* A static homology character, containing its code, and the character
    * itself *)
    | Stat of (int * Parser.t)

type cs = cs_d * specified

(* A transformation cost matrix for Sankoff characters *)
type tcm = int array array

module OutputInformation = struct
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

let median_code_count = ref (-1)

type d = {
    (* The pairs of synonyms for the loaded taxa *)
    synonyms : string All_sets.StringMap.t;
    (* Whether or not fixed states is to be used in the following molecular
    * character *)
    do_fs : bool;
    (* The current fixed states list to be used in whatever sequence is
    * loaded next in the dataset *)
    current_fs : Sequence.s list;
    (* The name of the fixed states file that contained the contents of
    * current_fs. *)
    current_fs_file : string;
    (* The transformation cost matrix to be used in the alignments of
    * whatever sequence is loaded next in the dataset *)
    current_tcm : Cost_matrix.Two_D.m;
    (* The three dimentional cost matrix to be used in the alignments of
    * whatever sequence is loaded next in the dataset. This is built based
    * on the [current_tcm3]. *)
    current_tcm3 : Cost_matrix.Three_D.m;
    (* The name of the file containing the [current_tcm] being used. *)
    current_tcm_file : string;
    current_alphabet_file : string;
    current_alphabet : alph;
    use_annotated_sequences : bool;
    (* A function to generate codes for character *)
    character_code_gen : int ref;
    (* A function to generate codes for segments in chromosomes *)
    seg_code_gen : int ref;
    (* Set of files where each taxon appears *)
    taxon_files : All_sets.Strings.t All_sets.StringMap.t;
    (* A map between taxon names and their assigned codes *)
    taxon_names : int All_sets.StringMap.t;
    (* A map between the taxon codes and their corresponding names *)
    taxon_codes : string All_sets.IntegerMap.t;
    (* A map of each taxon code and their corresponding character list *)
    taxon_characters : (int, (int, cs) Hashtbl.t) Hashtbl.t;
    (* A map between the character names and their corresponding codes *)
    character_names : (string, int) Hashtbl.t;
    (* A map between the character codes and their corresponding names *)
    character_codes : (int, string) Hashtbl.t;
    (* A map between the character codes and their specifications *)
    character_specs : (int, specs) Hashtbl.t;
    (* The set of taxa to be ignored in the analysis *)
    ignore_taxa_set : All_sets.Strings.t;
    (* The set of taxa to be ignored in the analysis *)
    ignore_character_set : string list;
    (* The set of loaded trees *)
    trees : string Parser.Tree.t list list;
    (* The set of codes that belong to the class of Non additive with up to 8
    * states *)
    non_additive_8 : int list;
    (* The set of codes that belong to the class of Non additive with up to 16
    * states *)
    non_additive_16 : int list;
    (* The set of codes that belong to the class of Non additive with up to 32
    * states *)
    non_additive_32 : int list;
    (* The set of codes that belong to the class of Non additive with up to 33
    * states *)
    non_additive_33 : int list;
    (* The set of codes that belong to the class of additive characters *)
    additive : int list;
    (* The set of codes that belong to the class of sankoff characters *)
    sankoff : int list list;
    (* The set of codes that belong to the class of sequence characters *)
    dynamics : int list;
    (** Tree for how to arrange taxa into complex terminals *)
    complex_schema : Parser.SetGroups.t list;
    (** The association list of files and kind of data they could hold *)
    files : (string * contents list) list;
    (* An index of specifications for the MDL principle *)
    specification_index : SpecIndex.t;
    (* An index of characters defined using the MDL principle *)
    character_index : (string * CharacSpec.t) list;
    (* The search information to be presented to the user *)
    search_information : OutputInformation.t list;
    (* At what taxon to root output trees *)
    root_at : int option;
}

type bool_characters = [
    | `All
    | `Some of (bool * int list)
    | `Names of (bool * string list)
    | `AllStatic
    | `AllDynamic
    | `Missing of (bool * int)
]

type characters = [
    | `All
    | `Some of int list 
    | `Names of string list
    | `AllStatic
    | `AllDynamic
    | `Missing of (bool * int)
]

let create_ht () = Hashtbl.create 1667 

(* Empty data! *)
let empty () = 
    {
    synonyms = All_sets.StringMap.empty;
    do_fs = false;
    current_fs = [];
    current_fs_file = "";
    current_tcm = Cost_matrix.Two_D.default;
    current_tcm3 = Cost_matrix.Three_D.default;
    current_tcm_file = "Substitutions: 1, Indels: 2";
    current_alphabet_file = "";
    current_alphabet = Nucleotides;
    use_annotated_sequences = false;
    character_code_gen = ref (-1);
    seg_code_gen = ref 0;
    taxon_names = All_sets.StringMap.empty;
    taxon_files = All_sets.StringMap.empty;
    taxon_codes = All_sets.IntegerMap.empty;
    taxon_characters = create_ht ();
    character_names = create_ht ();
    character_codes = create_ht ();
    character_specs = create_ht ();
    ignore_taxa_set = All_sets.Strings.empty;
    ignore_character_set = [];
    trees = [];
    non_additive_8 = [];
    non_additive_16 = [];
    non_additive_32 = [];
    non_additive_33 = [];
    additive = [];
    sankoff = [];
    dynamics = [];
    files = [];
    specification_index = SpecIndex.empty ();
    character_index = [];
    search_information = [`TreeInformation [`Summary]];
    root_at = None;
    complex_schema = [];
}




let copy_taxon_characters tc = 
    let new_tc = create_ht () in
    Hashtbl.iter (fun code othertbl ->
        Hashtbl.add new_tc code (Hashtbl.copy othertbl)) tc;
    new_tc

let duplicate data = 
    { data with
        taxon_characters = copy_taxon_characters data.taxon_characters;
        character_names = Hashtbl.copy data.character_names;
        character_codes = Hashtbl.copy data.character_codes;
        character_specs = Hashtbl.copy data.character_specs; }

let set_dyna_data seq_arr  = {seq_arr = seq_arr}

let set_sequence_defaults seq_alph data = 
    match seq_alph with
    | Nucleotides ->
            { data with 
                current_tcm = Cost_matrix.Two_D.default_nucleotides;
                current_tcm3 = Cost_matrix.Three_D.default_nucleotides;
                current_fs = [];
                current_fs_file = "";
                current_tcm_file = "";
                current_alphabet_file = "";
                current_alphabet = seq_alph; 
            }
    | Aminoacids ->
            { data with 
                current_tcm = Cost_matrix.Two_D.default_aminoacids;
                current_tcm3 = 
                    Lazy.force (Cost_matrix.Three_D.default_aminoacids);
                current_fs = [];
                current_fs_file = "";
                current_tcm_file = "";
                current_alphabet_file = "";
                current_alphabet = seq_alph; 
            }
    | GeneralAlphabet (alph, twd, threed, alphabet) ->
            { data with 
                current_tcm = twd;
                current_tcm3 = threed;
                current_fs = [];
                current_fs_file = "";
                current_tcm_file = alph;
                current_alphabet_file = alph;
                current_alphabet = seq_alph; 
            }


let get_empty_seq alph = 
    let seq = Sequence.create 1 in
    let seq = Sequence.prepend_char seq (Alphabet.get_gap alph) in
    { seq = seq; code = -1; }

let print (data : d) = 
    print_endline "Start printing data";
    Printf.fprintf stdout "Number of sequences: %i\n" (List.length data.dynamics);
    List.iter (Printf.fprintf stdout "%i ") data.dynamics; print_newline ();


    let print_taxon (key : int) (ch_ls : (int, cs) Hashtbl.t) = 
        let len = Hashtbl.fold (fun _ _ acc -> acc + 1) ch_ls 0 in
        let  taxa_name = All_sets.IntegerMap.find key data.taxon_codes in  
        Printf.fprintf stdout "Taxon: %s, number chars: %i\n" taxa_name len;
        Hashtbl.iter
            (fun _ ch ->
                 match ch with 
                 | Dyna (code, dyna_data), _ ->         
                       let  char_name = Hashtbl.find data.character_codes code in 
                       Printf.fprintf stdout "%s -> " char_name; 
                       Array.iter (fun seq -> 
                                       Printf.fprintf stdout "%i:" seq.code;
                                       Sequence.print stdout seq.seq Alphabet.nucleotides;
                                       Printf.fprintf stdout " | "
                                 ) dyna_data.seq_arr;
                       print_newline ();
                 | _ -> ()
            ) ch_ls;
    in

    let print_specs (code : int) (spec : specs) = 
        let name = Hashtbl.find data.character_codes code in 
        Printf.fprintf stdout "Key: %i, name: %s " code name; 
        (match spec with 
        | Dynamic dspec ->
              (match dspec.state with 
               | `Seq -> Printf.fprintf stdout "Seq"
               | `Breakinv -> Printf.fprintf stdout "Breakinv"
               | `Chromosome -> Printf.fprintf stdout "Chromosome"
               | `Genome -> Printf.fprintf stdout "Genome"
               | `Annotated -> Printf.fprintf stdout "Annotated")
        | _ -> Printf.fprintf stdout "Not Dynamic");

        print_newline ()
    in 
    Hashtbl.iter print_taxon data.taxon_characters;
    Hashtbl.iter print_specs data.character_specs 


let annotated_sequences bool data =
    { data with use_annotated_sequences = bool }

let get_weight c data = 
    match Hashtbl.find data.character_specs c with
    | Static (enc, _) -> 
            float_of_int (Parser.Hennig.Encoding.get_weight enc)
    | _ -> 1.0

(* Returns a fresh object with the added synonym from [a] to [b] to [data]. *)
let add_synonym data (a, b) =
    (* We need to check if [a] already has an assigned synonym *)
    if All_sets.StringMap.mem a data.synonyms then begin
        (* Hum ... so we do have a synonym, we need now to check if [(a, b)]
         * is consistent with whatever is stored currently in [data],
         * otherwise this is an exception *)
        let cur = All_sets.StringMap.find a data.synonyms in
        if cur <> b then begin
            let _ =
                let msg = 
                    "@[Overriding@ synonym:@ " ^ a ^ "@ to@ " ^
                    cur ^ "@ will@ now@ map@ to@ " ^ b ^ "@]" 
                in
                output_error msg
            in
            let ns = All_sets.StringMap.add a b data.synonyms in
            { data with synonyms = ns }
        end else data
    end else begin 
        (* [a] doesn't have a synonym, so we simply add it *)
        let ns = All_sets.StringMap.add a b data.synonyms in
        { data with synonyms = ns }
    end

let add_synonyms_file data file = 
    try
        let ch, file = FileStream.channel_n_filename file in
        let syns = Parser.Dictionary.of_channel ch in
        close_in ch;
        let len = Hashtbl.length syns in
        let msg = 
            "@[The@ file@ " ^ file ^ "@ contains@ " ^ string_of_int len ^ 
            "@ synonyms.@]"
        in
        Status.user_message Status.Information msg;
        Hashtbl.fold (fun a b c -> add_synonym c (a, b)) syns data 
    with
    | Sys_error err ->
            let file = FileStream.filename file in
            let msg = "@[Couldn't@ open@ file@ " ^ file ^ "@ to@ load@ the@ " ^
            "list@ of@ synonyms.@ @ The@ system@ error@ message@ is@ " ^ err ^
            ".@]" in
            output_error msg;
            data

let warn_if_repeated_and_choose_uniquely list str file =
    let repeated, selected = 
        List.fold_left ~f:(fun (repeated, seen) item ->
            if All_sets.Strings.mem item seen then
                All_sets.Strings.add item repeated, seen
            else repeated, All_sets.Strings.add item seen) 
        ~init:(All_sets.Strings.empty, All_sets.Strings.empty) list
    in
    let total = All_sets.Strings.cardinal repeated in
    if total > 0 then
        if total = 1 then
            let item = All_sets.Strings.choose repeated in
            Status.user_message Status.Error
            ("@{<b>Warning!:@}@ " ^ item ^ 
            "@ is@ duplicated@ in@ the@ " ^ str ^ "@ " ^ file)
        else begin
            let message, _ = 
                All_sets.Strings.fold (fun item (str, cnt) ->
                    if cnt = 0 then str ^ item, 1
                    else str ^ ",@ " ^ item, 1) 
                repeated
                (("@{<b>Warning!:@}@ The@ following@ items@ are@ duplicated@ in@ " ^
                "@ the@ " ^ str ^ "@ " ^ file ^ ":@ "), 0)
            in
            Status.user_message Status.Error message
        end
    else ();
    All_sets.Strings.fold (fun x acc -> x :: acc) selected []


let process_trees data file =
    let check_tree cnt trees =
        let check_tree tree = 
            let rec leafs acc tree = 
                match tree with
                | Parser.Tree.Node (c, _) ->
                        List.fold_left ~f:leafs ~init:acc c
                | Parser.Tree.Leaf x -> x :: acc
            in
            let leafs = leafs [] tree in
            let _ =
                warn_if_repeated_and_choose_uniquely leafs 
                ("input@ tree@ " ^ string_of_int cnt ^ "@ of@ file@ ")
                (FileStream.filename file)
            in
            ()
        in
        List.iter ~f:check_tree trees;
        cnt + 1
    in
    try
        let ch, file = FileStream.channel_n_filename file in
        let trees = Parser.Tree.of_channel ch in
        let len = List.length trees in
        let msg = 
            "@[The@ file@ " ^ file ^ "@ contains@ " ^ string_of_int len ^ 
            "@ trees.@]"
        in
        let _ = List.fold_left ~f:check_tree ~init:1 trees in
        Status.user_message Status.Information msg;
        { data with trees = data.trees @ trees }
    with
    | Sys_error err ->
            let file = FileStream.filename file in
            let msg = "@[Couldn't@ open@ file@ " ^ file ^ "@ to@ load@ the@ " ^
            "trees.@ @ The@ system@ error@ message@ is@ "
                ^ err ^
            ".@]" in
            output_error msg;
            data

let process_tcm data file = 
    try
        let ch, file = FileStream.channel_n_filename file in
        let tcm = Parser.TransformationCostMatrix.of_channel ch in
        let tcm3 = Cost_matrix.Three_D.of_two_dim tcm in
        close_in ch;
        let alph = Cost_matrix.Two_D.alphabet_size tcm in
        let msg = 
            "@[The@ file@ " ^ file ^ "@ defines@ a@ transformation@ cost@ matrix@ "
            ^ "for@ an@ alphabet@ of@ size@ " ^ string_of_int alph ^ ".@]"
        in
        Status.user_message Status.Information msg;
        { data with 
            current_tcm = tcm; 
            current_tcm3 = tcm3;
            current_tcm_file = file }
    with
    | Sys_error err ->
            let file = FileStream.filename file in
            let msg = "@[Couldn't@ open@ file@ " ^ file ^ "@ to@ load@ the@ " ^
            "transformation@ cost@ matrix.@ @ The@ system@ error@ message@ is@ "
                ^ err ^
            ".@]" in
            output_error msg;
            data

let process_fixed_states data file = 
        match file with
        | Some file ->
                begin try
                    let ch, file = FileStream.channel_n_filename file in
                    let fs = 
                        Parser.FixedStatesDict.of_channel 
                        Parser.Nucleic_Acids ch 
                    in
                    close_in ch;
                    let len = List.length fs in
                    let msg = 
                        "@[The@ file@ " ^ file ^ "@ defines@ " ^
                        string_of_int len ^ "@ valid@ states.@]"
                    in
                    Status.user_message Status.Information msg;
                    { 
                        data with do_fs = true;
                        current_fs = fs;
                        current_fs_file = file; 
                    }
                with
                | Sys_error err ->
                        let file = FileStream.filename file in
                        let msg = "@[Couldn't@ open@ file@ " ^ file
                            ^ "@ to@ load@ the@ " ^
                            "fixed@ states.@ @ The@ system@ error@ message@ is@ "
                            ^ err ^
                            ".@]" in
                        output_error msg;
                        data
                end;
        | None ->
                { data with do_fs = true; }

let find_code_for_root_if_removed data =
    (* We want to test, if a terminal that is currently root is removed, we choose
    * the one with the lowest terminal code, but if that's not the root, we
    * continue like nothing *)
    match data.root_at with
    | Some c -> 
            if Hashtbl.mem data.taxon_characters c then data
            else
                let nc = Hashtbl.fold (fun c _ acc ->
                    match acc with
                    | None -> Some c
                    | Some accc ->
                            if c < accc then Some c
                            else acc) data.taxon_characters None
                in
                { data with root_at = nc }
    | _ -> data

let pick_code_for_root code data =
    match data.root_at with
    | None -> { data with root_at = Some code }
    | Some previous -> 
            (* We must check if the terminal still is valid! If not we replace
            * it. *)
            if Hashtbl.mem data.taxon_characters previous then 
                data 
            else { data with root_at = Some code }

let trim taxon =
    let rec non_empty_position x n fmod res =
        if x = n then (res x)
        else 
            match taxon.[x] with
            | ' ' | '\t' -> non_empty_position (fmod x) n fmod res
            | _ -> x
    in
    let len = String.length taxon in
    if 0 < len then 
        let start = non_empty_position 0 len succ pred
        and final = non_empty_position (len - 1) (-1) pred succ in
        String.sub taxon start (final - start + 1) 
    else taxon

(** Returns a [d] with the following condition: if [taxon] is present in [data]
 * then [data] is returned, otherwise, [data] with the added name and assigned code
 * is returned. In addition, if there is no preferred taxon assigned to the
 * data structure, we set whatever is being load, that way we ensure that if no
 * root is defined, the first loaded taxon will be it by default. *)
let rec process_taxon_code data taxon file =
    (* Check first if the taxon is in the current list of taxa *)
    let taxon = trim taxon in
    if All_sets.StringMap.mem taxon data.taxon_names then 
        (*already exists, we must have a root already! *)
        let new_taxon_file = 
            let set = All_sets.StringMap.find taxon data.taxon_files in
            let set = All_sets.Strings.add file set in
            All_sets.StringMap.add taxon set data.taxon_files 
        in
        { data with taxon_files = new_taxon_file }, 
        All_sets.StringMap.find taxon data.taxon_names
    else if All_sets.StringMap.mem taxon data.synonyms then
        (* Is a synonym *)
        process_taxon_code data 
        (All_sets.StringMap.find taxon data.synonyms) file
    else begin
        (* It is new, so lets assign it a code and add it *)
        incr median_code_count;
        let code = !median_code_count in
        let taxon_names = 
            All_sets.StringMap.add taxon code data.taxon_names
        in
        let taxon_codes = 
            All_sets.IntegerMap.add code taxon data.taxon_codes
        in
        let taxon_files =
            All_sets.StringMap.add taxon (All_sets.Strings.singleton file)
            data.taxon_files 
        in
        let data = pick_code_for_root code data in
        { data with taxon_names = taxon_names; taxon_codes = taxon_codes; 
        taxon_files = taxon_files }, code
    end

let get_taxon_characters data tcode =
    try Hashtbl.find data.taxon_characters tcode with 
    | _ -> create_ht ()

(* Changes in place *)
let add_static_character_spec data (code, enc, filename) =
    Hashtbl.replace data.character_specs code (Static (enc, filename));
    Hashtbl.replace data.character_names filename code;
    Hashtbl.replace data.character_codes code filename

let report_static_input file (x, y, _) =
    let characters = Array.length x 
    and taxa = List.length y in
    let msg =
        "@[The@ file@ " ^ file ^ "@ defines@ " ^ string_of_int characters 
        ^ "@ characters@ in@ " ^ string_of_int taxa ^ "@ taxa.@]"
    in
    Status.user_message Status.Information msg

let gen_add_static_parsed_file do_duplicate data file (enc, res, trees) =
    let data = 
        if do_duplicate then duplicate data 
        else data
    in
    (* A function to report the taxa loading *)
    let len = List.length res in
    let st = Status.create "Loading Characters" (Some len) "taxa" in
    let len = Array.length enc in
    (* [codes] contain the character speecification for the sequences in
    * this file *)
    let codes = 
        let builder x = 
            incr data.character_code_gen;
            let code = !(data.character_code_gen)
            and enc = enc.(x)
            and filename = file ^ ":" ^ (string_of_int x) in
            code, enc, filename
        in
        Array.init ~f:builder len
    in
    Status.full_report ~msg:"Storing the character specifications" st;
    (* Now we add the codes to the data *)
    Array.iter ~f:(add_static_character_spec data) codes;
    (* And now a function to add one taxon at a time to the data *)
    let process_taxon data (ch, taxon_name) =
        let data, tcode = process_taxon_code data taxon_name file in
        let tl = get_taxon_characters data tcode in
        let add_character column it =
            let chcode, _, _ = codes.(column) in
            let spec = if Parser.is_unknown it then `Unknown else `Specified in
            Hashtbl.replace tl chcode ((Stat (chcode, it)), spec);
            (column + 1)
        in
        let _ = Array.fold_left ~f:add_character ~init:0 ch in
        Hashtbl.replace data.taxon_characters tcode tl;
        let did = Status.get_achieved st in
        Status.full_report ~adv:(did + 1) st;
        data
    in
    let data = List.fold_left ~f:process_taxon ~init:data res in
    Status.finished st;
    { data with trees = data.trees @ trees }

let add_static_parsed_file data file triple =
    gen_add_static_parsed_file true data file triple 


let add_multiple_static_parsed_file data list =
    let data = duplicate data in
    List.fold_left ~f:(fun acc (file, triple) ->
        gen_add_static_parsed_file false acc file triple)
    ~init:data list


let add_static_file ?(report = true) data file = 
    try
        let ch, file = FileStream.channel_n_filename file in
        let r = Parser.Hennig.of_channel ch in
        if report then report_static_input file r;
        close_in ch;
        add_static_parsed_file data file r
    with
    | Sys_error err ->
            let file = FileStream.filename file in
            let msg = "Couldn't@ open@ file@ " ^ file ^ "@ to@ load@ the@ " ^
            "data.@ @ The@ system@ error@ message@ is@ "
                ^ err ^
            "." in
            output_error msg;
            data

let process_parsed_sequences alphabet file dyna_state data res =
    let data = duplicate data in
    let res = 
        (* Place a single sequence together with its taxon *)
        (* This spot removes all the chromosome information comming from one
        * file *)
        let res = List.map (fun (res3, b) -> (List.flatten res3),b) res in 
        let tmp = 
            List.map (fun (res2, b) -> 
                          let res1 = List.flatten res2 in
                          List.map (fun s -> s, b) res1) res
        in
     
        (* Place each locus in a list containing all the taxa *)
        let arr = 
            let lst = List.map (Array.of_list) tmp in
            Array.of_list lst 
        in
        let num_taxa = Array.length arr in
        let loci = Array.fold_left 
            ~f:(fun max_loci loci_arr -> 
                 max max_loci (Array.length loci_arr)
            ) ~init:0 arr 
        in
        (* Check for errors *)
        (if (data.use_annotated_sequences = false) && (dyna_state != `Genome) then 
            match Array.length arr with
            | 0 -> ()
            | n ->
                  let init = ref (Array.length arr.(0)) in 
                  Array.iteri (fun pos x ->  
                       if 0 = (Array.length x) || !init = (Array.length x) then ()
                       else if !init = 0 then init := Array.length x
                       else begin 
                           let _, name = x.(0) in 
                           Status.user_message Status.Error 
                               ("Sequence " ^ name ^  
                                " has an inconsistent number of fragments. " 
                                ^ "I expect " ^ string_of_int !init ^  
                                " based on previous sequences, but it has " 
                                ^ string_of_int (Array.length x)
                               ); 
                           failwith ("Inconsistent input file " ^  file);  
                   end) arr
        ); 

        let res = ref [] in
        for j = loci - 1 downto 0 do 
            let loci = ref [] in
            for i = num_taxa - 1 downto 0 do
                match j < Array.length arr.(i) with
                | true -> loci := arr.(i).(j) :: !loci
                | false ->
                      let _, taxon = arr.(i).(0) in 
                      let seq = Sequence.create 1 in
                      let seq = 
                          Sequence.prepend_char seq (Alphabet.get_gap alphabet)
                      in
                      loci := (seq, taxon ) :: !loci
            done;
            res := !loci :: !res;
        done; 
        !res
    in
    let original_filename = file in
    let locus_name = 
        let c = ref (-1) in
        fun () ->
            incr c;
            file ^ ":" ^ string_of_int !c
    in
    let dyna_state = 
        match data.use_annotated_sequences with
        | true -> `Annotated
        | false -> dyna_state
    in 
    let add_character items max_len data = 
        let file = 
            match data.use_annotated_sequences or (dyna_state = `Chromosome) with 
            | false -> locus_name () 
            | true -> original_filename
        in
        incr data.character_code_gen;
        let chcode = !(data.character_code_gen) in
        let dspec = {
            filename = file;
            fs = data.current_fs_file;
            tcm = data.current_tcm_file;
            tcm2d = data.current_tcm;
            tcm3d = data.current_tcm3;
            alph = alphabet;
            pool = Sequence.Pool.create ((max_len * 5) / 4) items;
            state = dyna_state;
            pam = dyna_pam_default;
            weight = 1.0;
        } in
        Hashtbl.replace data.character_specs chcode (Dynamic dspec);
        Hashtbl.replace data.character_names file chcode;
        Hashtbl.replace data.character_codes chcode file;
        chcode, data
    in 
    let single_loci_processor acc res = 
        let chcode, data = 
            let get_lengths (a, b) (seq, _) =
                a + 1, max b (Sequence.length seq)
            in
            let items, max_len = 
                List.fold_left ~f:get_lengths ~init:(0, 0) res 
            in
            add_character items max_len acc 
        in 
        (* Now a function to process one taxon at a time to be 
        * folded over the taxa collected in the [file]. *)
        let process_a_taxon data (seq, taxon) =
            (* Here is where, at the parser level, the fixed 
            * states support should be added *)
            let data, tcode = 
                process_taxon_code data taxon original_filename 
            in
            let tl = get_taxon_characters data tcode in
            let seqa = 
                match dyna_state with 
                | `Seq -> seq
                | _ -> Sequence.del_first_char seq 
            in 
            let seq = {seq=seqa; code = -1} in
            let dyna_data = {seq_arr =  [|seq|]} in 
            let _ = 
                let spc =
                    if 2 <= Sequence.length seqa then `Specified
                    else `Unknown
                in
                Hashtbl.replace tl chcode (Dyna (chcode, dyna_data), spc) 
            in
            Hashtbl.replace data.taxon_characters tcode tl;
            data
        in
        List.fold_left ~f:process_a_taxon ~init:data res
    in
    let process_annotated_chrom data = 
        let arr = Array.of_list res in 
        let num_loci = Array.length arr in 
        
        let mat = Array.map Array.of_list arr in 
        let locus0 = mat.(0) in 
        let num_taxa = Array.length locus0 in 
        let chcode, data = 
            let max_len = 
                Array.fold_left ~f:(fun x (seq, _) -> max x (Sequence.length seq)) ~init:0 locus0
            in
            add_character num_taxa max_len data 
        in 
        data.seg_code_gen := 0;

        let rec add_taxon data t = 
            match t >= num_taxa with
            | true -> data 
            | false ->
                  let seq, taxon = locus0.(t) in  
                  let data, tcode =  
                      process_taxon_code data taxon original_filename  
                  in 
                  let rec get_annchrom rev_seq_ls l =
                      if l = num_loci then List.rev rev_seq_ls
                      else begin
                           let seq, _ = mat.(l).(t) in 
                           let len = Sequence.length seq in 
                           match len > 1 with 
                           | false -> get_annchrom rev_seq_ls (l + 1)
                           | true -> 
                                 incr data.seg_code_gen; 
                                 let code = !(data.seg_code_gen) in   
                                 incr data.seg_code_gen; (** this code is for
                                                             the negative state *)                                     
                                 let clean_seq = Sequence.sub seq 1 (len - 1) in 
                                 let rev_seq_ls = 
                                     {seq = clean_seq; code = code}::rev_seq_ls 
                                 in 
                                 get_annchrom rev_seq_ls (l + 1)
                      end 
                  in 
                  let seq_arr = Array.of_list (get_annchrom [] 0) in 
                  let chrom_data = {seq_arr =  seq_arr} in  
                  let tl = get_taxon_characters data tcode in 
                  let _ =  
                      Hashtbl.replace tl chcode 
                      (Dyna (chcode, chrom_data), `Specified) 
                  in 
                  Hashtbl.replace data.taxon_characters tcode tl;
                  add_taxon data (t + 1)
        in 
        add_taxon data 0
    in 
    let process_genome data = 
        let num_genome = List.length (List.hd res) in 
        let num_chrom = List.length res in 

        let max_chrom_len = ref 0 in 
        let genome_arr = Array.init num_genome 
            (fun ti -> Array.init num_chrom 
                 (fun ci -> 
                      let chrom, name = List.nth (List.nth res ci) ti in 
                      max_chrom_len := max !max_chrom_len (Sequence.length chrom);
                      (chrom, name)
                 )
            )
        in 

        let chcode, data =  add_character num_genome !max_chrom_len data in 
        
        Array.fold_left 
            ~f:(fun data chrom_arr ->

                 let chrom_ls = Array.fold_left 
                     ~f:(fun chrom_ls (chrom, _) ->
                          let chrom_len = Sequence.length chrom in 
                          match chrom_len with
                          | 1 -> chrom_ls
                          | _ ->
                                let clean_chrom = Sequence.sub chrom 1 (chrom_len - 1) in
                                incr data.seg_code_gen;                          
                                let code = data.seg_code_gen in  
                                {seq = clean_chrom; code = !code}::chrom_ls
                     ) ~init:[] chrom_arr 
                 in 

                 let genome_data = {seq_arr = Array.of_list (List.rev chrom_ls)} in
                 
                 let _, taxon_name = chrom_arr.(0) in 
                 let data, tcode = 
                     process_taxon_code data taxon_name original_filename 
                 in
                 let tl = get_taxon_characters data tcode in 
                 let _ = 
                     Hashtbl.replace tl chcode 
                     (Dyna (chcode, genome_data), `Specified) in 
                 let _ =  
                     Hashtbl.replace data.taxon_characters tcode tl
                 in
                 data
            ) ~init:data genome_arr
    in 
    let data = 
        match data.use_annotated_sequences with  
        | false -> 
              if dyna_state = `Genome then process_genome data
              else List.fold_left ~f:single_loci_processor ~init:data res 
        | true ->  process_annotated_chrom data 
    in 
    data


let dna_lexer = Alphabet.Lexer.make_lexer false Alphabet.nucleotides

let check_if_taxa_are_ok file taxa = 
    let the_great_majority_is_acgt (lst, _) = 
        let base = ref 0
        and others = ref 0 in
        List.iter (function 1 | 2 | 4 | 8 -> incr base | _ -> incr others) lst;
        !base > (3 * !others)
    in
    let has_spaces x = Str.string_match (Str.regexp ".* .*") (trim x) 0 in
    (* We want to check if the names are unique and if there is a suspicious
    * name of a taxon *)
    let _ = List.fold_left ~f:(fun acc x ->
        let msg =
            ("@{<b>Warning!:@}@ There@ is@ a@ taxon@ name@ that@ has@ "
            ^ "spaces@ on@ it.@ This@ leaves@ the@ generated@ trees@ "
            ^ "unreadable!.@ If you@ want@ to@ continue,@ that's@ "
            ^ "your@ call...@ the@ file@ is@ " ^ file 
            ^ "@ and the@ taxon@ is@ " ^ x)
        in
        (* We'll see, if we can parse it with the dna parser, we will tell the
        * user that the name of the taxon looks suspicious. *)
        let _ =
            let name = Stream.of_string x in
            try 
                let l = dna_lexer name [] 0 in
                if the_great_majority_is_acgt l then
                    Status.user_message Status.Error 
                    ("@{<b>Warning!:@}@ There@ is@ a@ taxon@ name@ that@ is@ "
                    ^ "suspiciously@ simmilar@ to@ a@ DNA@ sequence@ in@ the@ "
                    ^ "file@ " ^ file ^ "!.@ The@ taxon@ is@ " ^ x)
                else if has_spaces x then
                    Status.user_message Status.Error msg
                else ()
            with
            | _ -> 
                    if has_spaces x then
                        Status.user_message Status.Error msg
                    else ()
        in
        if All_sets.Strings.mem x acc then begin
            Status.user_message Status.Error 
            ("@{<b>Warning!:@}@ There@ is@ a@ taxon@ duplicated@ in@ the@ "
            ^ "file@ " ^ file ^ "!.@ The@ duplicated@ taxon@ is@ " ^ x);
            acc
        end else All_sets.Strings.add x acc) ~init:All_sets.Strings.empty taxa
    in
    ()

let aux_process_molecular_file processor builder dyna_state data file = 
    let alphabet = 
        let alphabet = 
            match data.current_alphabet with
            | Nucleotides -> Alphabet.nucleotides
            | Aminoacids -> Alphabet.aminoacids
            | GeneralAlphabet (_, _, _, a) -> a
        in
        alphabet
    in
    begin try
        let ch = Parser.molecular_to_fasta file in
        let res = 
            try Parser.Fasta.of_channel (builder alphabet) ch with
            | Parser.Illegal_molecular_format fl ->
                    let file = FileStream.filename file in
                    let fl = { fl with Parser.filename = file } in
                    Parser.print_error_message fl;
                    raise (Parser.Illegal_molecular_format fl)
        in
        let res = List.filter (function [[]], _ | [], _ -> false | _ -> true) res
        in
        let _ = (* Output a message with the contents of the file *)
            let num_taxa = List.length res in
            let taxa_contents = 
                let file = FileStream.filename file in
                "@[The@ file@ " ^ file ^ "@ contains@ sequences@ of@ " ^
                string_of_int num_taxa ^ "@ taxa" 
            and sequence_contents = 
                if 0 = num_taxa then ""
                else begin
                    let lst, _ = List.hd res in
                    let add acc x = acc + (List.length (List.flatten x)) in
                    let len = List.fold_left ~f:add ~init:0 lst in
                    ",@ each@ sequence@ holding@ " ^
                    string_of_int len ^ "@ " ^ (if len > 1 then
                        "fragments.@]" else "fragment.@]@.")
                end
            in
            Status.user_message Status.Information (taxa_contents ^
            sequence_contents);
            check_if_taxa_are_ok (FileStream.filename file) 
            (let _, names = List.split res in names)
        in
        close_in ch;
        processor alphabet res
    with
    | Sys_error err ->
            let file = FileStream.filename file in
            let msg = "Couldn't@ open@ file@ " ^ file ^ "@ to@ load@ the@ " ^
                "dna@ sequences@ file.@ @ The@ system@ error@ message@ is@ "
                ^ err ^
                "." in
            output_error msg;
            data
    end

let process_molecular_file dyna_state data file = 
    aux_process_molecular_file 
    (fun alph parsed -> 
        process_parsed_sequences alph (FileStream.filename file) dyna_state data
        parsed)
    (fun x -> Parser.AlphSeq x) 
    dyna_state 
    data 
    file

let process_dna_sequences data (file_list, options) =
    let process_options data = function
        | Tcm a -> process_tcm data (`Remote a)
        | Fixed a -> 
                let a = 
                    match a with
                    | Some a -> Some (`Remote a)
                    | None -> None 
                in
                process_fixed_states data a
    in
    (* We set the options to whatever is required in the data *)
    let data = 
        match options with
        | None -> data 
        | Some a -> List.fold_left ~f:process_options ~init:data a
    in
    let f a b = process_molecular_file `Seq a b  in
    List.fold_left ~f ~init:data file_list

let process_ignore_taxon data taxon =
    let res = All_sets.Strings.add taxon data.ignore_taxa_set in
    { data with ignore_taxa_set = res; }

let process_ignore_file data file = 
    try
        let ch, file = FileStream.channel_n_filename file in
        let taxa = Parser.IgnoreList.of_channel ch in
        List.fold_left ~f:process_ignore_taxon ~init:data taxa
    with
    | Sys_error err ->
            let file = FileStream.filename file in
            let msg = "Couldn't open file " ^ file ^ " to load the " ^
            "ignore taxa. The system error message is " ^ err in
            Status.user_message Status.Error msg;
            data

let complement_taxa data taxa = 
    let remover acc taxon = All_sets.StringMap.remove taxon acc in
    let the_taxa = List.fold_left ~f:remover ~init:data.taxon_names taxa in
    let elements = 
        All_sets.StringMap.fold (fun name _ lst -> name :: lst) the_taxa []
    in
    elements

let code_taxon code data = 
    All_sets.IntegerMap.find code data.taxon_codes

let report included excluded =
    let len1 = List.length included
    and len2 = List.length excluded in
    let total = max len1 len2 in
    let arr = Array.make_matrix (total + 1) 2 "" in
    let add row position item =
        arr.(position).(row) <- item;
        position + 1
    in
    let total_included = List.fold_left ~f:(add 0) ~init:1 included
    and total_excluded = List.fold_left ~f:(add 1) ~init:1 excluded in
    arr.(0).(0) <- "Included";
    arr.(0).(1) <- "Excluded";
    Status.user_message Status.Information 
    "@[<v>@[Selected@ Terminals:@]@,";
    Status.output_table Status.Information arr;
    Status.user_message Status.Information 
    "@]@,%!";
    Status.user_message Status.Information 
    ("@[Total@ included:@ " ^ string_of_int (total_included - 1) ^ "@]@,");
    Status.user_message Status.Information 
    ("@[Total@ excluded:@ " ^ string_of_int (total_excluded - 1) ^ "@]@,")

let rec process_analyze_only_taxa meth data = 
    match meth with
    | `Names (dont_complement, taxa) ->
            let taxa = 
                warn_if_repeated_and_choose_uniquely taxa 
                "selected@ names" ""
            in
            let taxa = 
                if not dont_complement then taxa
                else complement_taxa data taxa 
            in
            List.fold_left ~f:process_ignore_taxon ~init:data taxa
    | `Missing (dont_complement, fraction) ->
            Status.user_message Status.Information "Here";
            let fraction = (float_of_int fraction) /. 100. in
            let characters = 
                float_of_int 
                (Hashtbl.fold (fun _ _ acc -> acc + 1)
                data.character_codes 0)
            in
            let process_taxon txn_lst =
                Hashtbl.fold (fun _ (_, item) spec ->
                    match item with
                    | `Specified -> spec + 1
                    | `Unknown -> spec) txn_lst 0
            in
            let add_or_remove_taxon code taxon_cs (included, excluded) =
                let spec = process_taxon taxon_cs in
                if fraction <= (float_of_int spec) /. characters then
                    included, (code_taxon code data) :: excluded
                else (code_taxon code data) :: included, excluded
            in
            let included, excluded =
                let included, excluded = 
                    Hashtbl.fold add_or_remove_taxon 
                    data.taxon_characters ([], [])
                in
                if dont_complement then included, excluded
                else excluded, included
            in
            report included excluded;
            process_analyze_only_taxa (`Names (false, excluded)) data

let process_analyze_only_file dont_complement data files =
    try
        let appender acc file = 
            try 
                let ch, file = FileStream.channel_n_filename file in
                let taxa = Parser.IgnoreList.of_channel ch in
                let taxa = 
                    warn_if_repeated_and_choose_uniquely taxa "terminals file" file
                in
                close_in ch;
                taxa @ acc
                with
                | Sys_error err ->
                        let file = FileStream.filename file in
                        let msg = 
                            "Couldn't open file " ^ file ^ " to load the " ^
                            "terminals file. The system error message is " ^ 
                            err 
                        in
                        failwith msg
        in
        let taxa = List.fold_left ~f:appender ~init:[] files in
        let taxa = List.map trim taxa in
        let ignored, taxa = 
            if dont_complement then complement_taxa data taxa, taxa
            else taxa, complement_taxa data taxa
        in
        report taxa ignored;
        List.fold_left ~f:process_ignore_taxon ~init:data ignored
    with
    | Failure msg ->
            Status.user_message Status.Error msg;
            data

let remove_taxa_to_ignore data = 
    let data = duplicate data in
    let process_data taxon data =
        try
            let tcode = 
                All_sets.StringMap.find taxon data.taxon_names 
            in
            Hashtbl.remove data.taxon_characters tcode;
            data
        with
        | _ -> data
    in
    let data = All_sets.Strings.fold process_data data.ignore_taxa_set data in
    find_code_for_root_if_removed data

let report_terminals_files filename taxon_files ignored_taxa =
    let files = All_sets.StringMap.empty 
    and fo = Status.user_message (Status.Output (filename, false, [])) in
    let make_per_file_set taxon files acc =
        let is_ignored = All_sets.Strings.mem taxon ignored_taxa in
        let process_all_files file acc =
            if All_sets.StringMap.mem file acc then
                let (included, excluded) = All_sets.StringMap.find file acc in
                if is_ignored then
                    All_sets.StringMap.add file (included, (taxon :: excluded)) 
                    acc
                else 
                    All_sets.StringMap.add file ((taxon :: included), excluded)
                    acc
            else 
                if is_ignored then
                    All_sets.StringMap.add file ([], [taxon]) acc
                else All_sets.StringMap.add file ([taxon], []) acc
        in
        All_sets.Strings.fold process_all_files files acc
    and print_file file_name (included, excluded) =
        fo ("@,@[<v 2>@{<u>Input File:@} " ^ file_name ^ "@,@[<v 0>");
        let output_list str lst = 
            let total = string_of_int (List.length lst) in
            fo ("@,@[Terminals " ^ str ^ " (" ^ total ^ ")@]@[<v 2>@,@[<v 0>");
            List.iter (fun x -> fo x; fo "@,") lst;
            fo "@]@]";
        in
        output_list "Included" included;
        output_list "Excluded" excluded;
        fo "@]@]@,"
    in
    let files = All_sets.StringMap.fold make_per_file_set taxon_files files in
    fo "@[<v 2>@{<b>Terminals Files@}:@,@[<v 0>";
    All_sets.StringMap.iter print_file files;
    fo "@]@]%!"

(* This function will output in channel [ch] what was recorded in the
* [data]. This will have to be changed to XML. *)
let to_channel ch data = 
    let print_synonyms a b = 
        output_string ch a;
        output_string ch " -> ";
        output_string ch b;
        output_string ch "\n"
    in
    let print_taxa a _ =
        output_string ch a;
        output_string ch "\n"
    in
    let print_ignored_taxa a = 
        output_string ch a;
        output_string ch "\n"
    in
    let print_characters _ = function
        | Static (a, fname) ->
                let str = Parser.Hennig.Encoding.to_string a in
                output_string ch fname;
                output_string ch " -> ";
                output_string ch str;
                output_string ch "\n"
        | Dynamic dspec ->
                output_string ch dspec.filename;
                output_string ch ", ";
                output_string ch dspec.fs;
                output_string ch ", ";
                output_string ch dspec.tcm;
                output_string ch "\n"
        | _ -> ()
    in
    output_string ch "List of synonyms: \n";
    All_sets.StringMap.iter print_synonyms data.synonyms;
    output_string ch "List of taxa: \n";
    All_sets.StringMap.iter print_taxa data.taxon_names;
    output_string ch "List of ignored taxa: \n";
    All_sets.Strings.iter print_ignored_taxa data.ignore_taxa_set;
    output_string ch "List of loaded characters: \n";
    Hashtbl.iter print_characters data.character_specs 


let add_character_spec spec code data =
    let data = duplicate data in
    (* We have to test whether or not it's a member already *)
    if not (Hashtbl.mem data.character_specs code) then begin
        Hashtbl.replace data.character_specs code spec 
    end else raise Illegal_argument;
    data

let taxon_code name data =
    All_sets.StringMap.find name data.taxon_names

let get_tcm code data = 
    match Hashtbl.find data.character_specs code with
    | Static (r, _) -> Parser.Hennig.Encoding.get_tcm r
    | _ -> failwith "Unexpected"



(** funtion to see if a cost matrix matches the cost matrix associated with 
 *  the list first_lst.
 *  Parameters are
 *  code_cm is a cost matrix (int array array) 
 *  first_lst is an int list
 *  data is a Parser.Data.d type and this contains the encoding specs which
 *  contain the cost matrices.
*)
let match_cost_matrix code_cm first_lst data =
    match first_lst with
    | hd :: _ -> 
            let hd_cm = get_tcm hd data in
            if code_cm = hd_cm then true
            else false
    | [] -> false

(** [classify code data g] - is used for Sankoff characters to produce an
 *  int list list. Every code that has the same cost matrix is placed
 *  in the same int list.  
 *  This function is placed here rather than in parser.ml because is needs
 *  to use Data.get_tcm function and if this is used in parser.ml
 *  then there is a circular dependency between poyParser.ml and parser.ml
 *  Parameters are
 *  code is an int  
 *  data is a Parser.Data.d type and this contains the encoding specs which
 *  contain the cost matrices.
 *  g is the int list list being built
 *)
let classify code data = 
    let code_cm = get_tcm code data in
    let rec builder lst = 
        match lst with
        | [] -> [[code]]
        | hd :: tl -> 
                if match_cost_matrix code_cm hd data then
                    (code :: hd) :: tl
                else hd :: builder tl
    in
    { data with sankoff = builder data.sankoff }

let categorize data = 
    (* We must return 7 lists of integers containing the codes for each class of
     * character *)

    (* We recategorize the data, so we must clear any already-loaded
       data *)
    let data = { data with
                     non_additive_8 = [];
                     non_additive_16 = [];
                     non_additive_32 = [];
                     non_additive_33 = [];
                     additive = [];
                     sankoff = [];
                     dynamics = [];
               } in                         
    let categorizer code spec data =
        match spec with
        | Static (enc, _) -> (* Process static characters *)
              if Parser.Hennig.Encoding.is_sankoff enc then
                  classify code data
              else if Parser.Hennig.Encoding.is_ordered enc && 
                  Parser.Hennig.Encoding.has_states 2 max_int enc then 
                      { data with additive = code :: data.additive }
              else if Parser.Hennig.Encoding.has_states 2 8 enc then
                  { data with non_additive_8 = code ::
                          data.non_additive_8 }
              else if Parser.Hennig.Encoding.has_states 9 16 enc then
                  { data with non_additive_16 = code :: 
                          data.non_additive_16 }
              else if Parser.Hennig.Encoding.has_states 17 32 enc then
                  { data with non_additive_32 = code :: 
                          data.non_additive_32 }
              else if not (Parser.Hennig.Encoding.is_ordered enc) 
                  && Parser.Hennig.Encoding.has_states 2 max_int enc
              then
                  { data with non_additive_33 = code :: 
                          data.non_additive_33 }
              else data
        | Dynamic _ -> { data with dynamics = code :: data.dynamics }
        | Set -> data
    in
    Hashtbl.fold categorizer data.character_specs data


let character_code name data = 
    Hashtbl.find data.character_names name

let code_character code data =
    Hashtbl.find data.character_codes code

let get_sequence_tcm seqcode data = 
    let chars = data.character_specs in
    try
        match Hashtbl.find chars seqcode with
        | Dynamic dspec -> dspec.tcm2d
        | _ -> failwith "Data.get_sequence_tcm: Not a dynamic character"
    with
    | Not_found as err ->
            let name = code_character seqcode data in
            let msg = "Could not find the code " ^ string_of_int seqcode ^ 
            " with name " ^ name in
            Status.user_message Status.Error msg;
            raise err

let get_sequence_alphabet seqcode data = 
    let chars = data.character_specs in
    try
        match Hashtbl.find chars seqcode with
        | Dynamic dspec -> dspec.alph
        | _ -> failwith "Data.get_sequence_alphabet: Not a dynamic character"
    with
    | Not_found as err ->
            let name = code_character seqcode data in
            let msg = "Could not find the code " ^ string_of_int seqcode ^ 
            " with name " ^ name in
            Status.user_message Status.Error msg;
            raise err


let get_files data = List.rev data.files

let add_file data contents file = 
    let file = (FileStream.filename file), contents in
    { data with files = file :: data.files }

let get_taxa data = 
    All_sets.IntegerMap.fold (fun _ name acc -> name :: acc) data.taxon_codes []

let get_used_observed code d =
    let a = 
        match Hashtbl.find d.character_specs code with
        | Static (a, _) -> a
        | _ -> failwith "Data.get_used_observed expects a static character"
    in
    match Parser.Hennig.Encoding.get_used_observed a with
    | Some x -> x
    | None -> failwith "Data.get_used_observed"

let synonyms_to_formatter d : Tags.output =
    let synonym t1 t2 acc : Tags.output Sexpr.t list =
        let t1 = `Single (Tags.Data.value, [], `String t1)
        and t2 = `Single (Tags.Data.value, [], `String t2) in
        `Single (Tags.Data.synonym, [], `Structured (`Set [t1; t2])) :: acc
    in
    let res = All_sets.StringMap.fold synonym d.synonyms [] in
    Tags.Data.synonyms, [], `Structured (`Set res)

let taxon_names_to_formatter d : Tags.output =
    let taxon_name code name acc =
        if All_sets.Strings.mem name d.ignore_taxa_set then acc
        else
            let attr = [
                (Tags.Data.name, name);
                (Tags.Data.code, string_of_int code);
            ]
            in
            `Single (Tags.Data.taxon, attr, `Structured `Empty) :: acc
    in
    let res = All_sets.IntegerMap.fold taxon_name d.taxon_codes [] in
    Tags.Data.taxa, [], `Structured (`Set res)

let files_to_formatter d : Tags.output =
    let file (f, contents) : Tags.output Sexpr.t =
        let contents = 
            let contents_to_output item =
                let tmp = 
                    match item with
                    | Characters -> Tags.Data.characters
                    | CostMatrix -> Tags.Data.cost_matrix
                    | Trees -> Tags.Data.trees
                in
                `Single (Tags.Data.file_contents, [], `String tmp)
            in
            List.map contents_to_output contents 
        and name = Tags.Data.filename, f in
        `Single ((Tags.Data.file, [name], `Structured (`Set contents)) :
            Tags.output)
    in
    Tags.Data.files, [], `Structured (`Set (List.map file d.files))

let ignored_taxa_to_formatter d : Tags.output = 
    let taxon x acc =
        `Single (Tags.Data.value, [], `String x) :: acc
    in
    let res = All_sets.Strings.fold taxon d.ignore_taxa_set [] in
    Tags.Data.ignored_taxa, [], `Structured (`Set res)

let ignored_characters_to_formatter d : Tags.output = 
    let character x = `Single (Tags.Data.value, [], `String x) in
    let res = List.map character d.ignore_character_set in
    Tags.Data.ignored_characters, [], `Structured (`Set res)

let states_set_to_formatter enc : Tags.output = 
    let set = Parser.Hennig.Encoding.get_set enc in
    let add item acc =
        `Single (Tags.Characters.state, [Tags.Characters.value,
        string_of_int item], `Structured `Empty) :: acc
    in
    let res = All_sets.Integers.fold add set [] in
    Tags.Characters.states, [], `Structured (`Set res)

let character_spec_to_formatter enc : Tags.output =
    match enc with
    | Static (enc, name) ->
            if Parser.Hennig.Encoding.is_ordered enc then
                Tags.Characters.additive,
                [
                    Tags.Characters.name, name;
                    Tags.Characters.min, string_of_int 
                    (Parser.Hennig.Encoding.get_min enc);
                    Tags.Characters.max, string_of_int (
                        Parser.Hennig.Encoding.get_max enc);
                    Tags.Characters.weight, string_of_int (
                        Parser.Hennig.Encoding.get_weight enc)
                ],
                `Structured `Empty
            else if not (Parser.Hennig.Encoding.is_sankoff enc) then
                Tags.Characters.nonadditive,
                [
                    Tags.Characters.name, name;
                    Tags.Characters.weight, string_of_int (
                        Parser.Hennig.Encoding.get_weight enc)
                ],
                `Structured (`Single (states_set_to_formatter enc))
            else if (Parser.Hennig.Encoding.is_sankoff enc) then 
                Tags.Characters.sankoff, 
                [
                    Tags.Characters.name, name;
                    Tags.Characters.weight, string_of_int
                        (Parser.Hennig.Encoding.get_weight enc)
                ], `Structured (`Single (states_set_to_formatter enc))
            else failwith "What is this?"
    | Dynamic dspec ->
            Tags.Characters.molecular, 
            [
                Tags.Characters.name, dspec.filename;
                Tags.Characters.fixed_states, dspec.fs;
                Tags.Characters.tcm, dspec.tcm;
            ],
            `Structured `Empty
    | Set -> failwith "TODO Set in Data.character_spec_to_formatter"

let characters_to_formatter d : Tags.output =
    let create code name acc =
        let enc = Hashtbl.find d.character_specs code in
        let res = Tags.Characters.character, 
        [ (Tags.Characters.name, name) ],
        `Structured (`Single (character_spec_to_formatter enc))
        in
        (`Single res) :: acc
    in
    let res = Hashtbl.fold create d.character_codes [] in
    Tags.Data.characters, [], `Structured (`Set res)

let to_formatter attr d : Tags.output =
    let syn = `Single (synonyms_to_formatter d)
    and names = `Single (taxon_names_to_formatter d)
    and ignored_taxa = `Single (ignored_taxa_to_formatter d)
    and ignored_char = `Single (ignored_characters_to_formatter d)
    and characters = `Single (characters_to_formatter d)
    and files = `Single (files_to_formatter d) in
    Tags.Data.data, attr, 
    `Structured 
    (`Set [names; syn; ignored_taxa; characters; ignored_char; files])

(** transform dyna_pam_ls which is a list of dynamic parameters
* taken from poyCommand into dyna_pam which is structured as a record*)
let set_dyna_pam dyna_pam_ls = 
    List.fold_left 
    ~f:(fun dyna_pam pam ->
        match pam with
        | `Inversion c -> {dyna_pam with re_meth = Some (`Inversion c)}
        | `Breakpoint c -> {dyna_pam with re_meth = Some (`Breakpoint c)}
        | `Chrom_Breakpoint c -> {dyna_pam with chrom_breakpoint = Some c}
        | `Circular c -> 
              if c then {dyna_pam with circular = Some 1}
              else {dyna_pam with circular = None}
        | `Locus_Indel_Cost c -> {dyna_pam with locus_indel_cost = Some c}
        | `Chrom_Indel_Cost c -> {dyna_pam with chrom_indel_cost = Some c}
        | `Chrom_Hom c -> {dyna_pam with chrom_hom = Some c}
        | `Sig_Block_Len c -> {dyna_pam with sig_block_len = Some c}
        | `Seed_Len c -> {dyna_pam with seed_len = Some c}
        | `Keep_Median c -> 
                {dyna_pam with keep_median = Some c}
        | `SwapMed c -> {dyna_pam with swap_med = Some c}    
        | `Approx c  -> {dyna_pam with approx = Some c}) 
    ~init:dyna_pam_default dyna_pam_ls

let get_dynas data dyna_code = 
    Hashtbl.fold
        (fun taxa_code ch_ls dyna_ls -> 
             let acs = 
                 try
                     match Hashtbl.find ch_ls dyna_code with
                     | ((Dyna _), _) as res -> Some res
                     | _ -> None
                 with  
                 | Not_found -> None
             in   
             match acs with 
             | Some acs ->
                   let dyna =   
                       match acs with   
                       | Dyna (code, dyna), _ -> dyna
                       | _, _ -> failwith "get dynamics"                          
                   in   
                   dyna :: dyna_ls  
             | None -> dyna_ls
        ) data.taxon_characters [] 

(** transform all annchroms whose codes are on the code_ls into breakinvs *)    
let create_alpha_c2_breakinvs (data : d) chcode =  

    let spec = Hashtbl.find data.character_specs chcode in  
    let c2, alpha = match spec with 
    | Dynamic dspec -> dspec.tcm2d, dspec.alph
    | _ -> failwith "Transfrom_annchroms_to_breakinvs: Not Dynamic" 
    in  
    let chrom_ls = get_dynas data chcode in 
        
    let max_code = List.fold_left  
        ~f:(fun max_code chrom -> 
                Array.fold_left ~f:(fun max_code seq -> max max_code seq.code 
                                   ) ~init:max_code chrom.seq_arr 
           ) ~init:0 chrom_ls 
    in  

    let gen_alpha = ref [] in 
    for code = 1 to max_code + 1 do 
        let char =  
            match code mod 2  with 
            | 1 -> string_of_int ( (code + 1)  / 2)  
            | 0 -> "~" ^ ( string_of_int (code / 2) ) 
            | _ -> failwith "compiler is error" 
        in  
        gen_alpha := (char, code)::!gen_alpha;         
    done;  

    let gen_gap_code = max_code + 2 in  
    gen_alpha := ("_", gen_gap_code)::!gen_alpha;  
    gen_alpha := ("~_", (gen_gap_code + 1))::!gen_alpha;     

    let gen_com_code = gen_gap_code + 2 in  
    gen_alpha := ("*", gen_com_code)::!gen_alpha;  
    gen_alpha := ("~*", (gen_com_code + 1))::!gen_alpha;  

    let max_code = gen_com_code + 1 in  
    let gen_alpha = 
        Alphabet.list_to_a (List.rev !gen_alpha) "_" "*"
        Alphabet.Sequential
    in 
    (** Finish creating alphabet*)      


    let all_seq_arr = List.fold_left  
        ~f:(fun all_seq_arr chrom -> Array.append all_seq_arr chrom.seq_arr 
           ) ~init:[||] chrom_ls  
    in  

    let gen_cost_mat = Array.make_matrix max_code max_code max_int in  

    let num_seq = Array.length all_seq_arr in  
    for idx1 = 0 to num_seq - 2 do 
        for idx2 = idx1 + 1 to num_seq - 1 do 

            let seq1 = all_seq_arr.(idx1).seq in 
            let code1 = all_seq_arr.(idx1).code in 
            
            let seq2 = all_seq_arr.(idx2).seq in 
            let code2 = all_seq_arr.(idx2).code in 
                
            let _, _, cost = Sequence.Align.align_2 ~first_gap:false 
                seq1 seq2 c2 Matrix.default 
            in 

            gen_cost_mat.(code1).(code2) <- cost;
            gen_cost_mat.(code1).(code2 + 1) <- cost; 
            gen_cost_mat.(code1 + 1).(code2) <- cost; 
            gen_cost_mat.(code1 + 1).(code2 + 1) <- cost; 

            gen_cost_mat.(code2).(code1) <- cost; 
            gen_cost_mat.(code2).(code1 + 1) <- cost; 
            gen_cost_mat.(code2 + 1).(code1) <- cost; 
            gen_cost_mat.(code2 + 1).(code1 + 1) <- cost;                                 
        done; 
    done; 
        
    let gap = Alphabet.get_gap alpha in
    Array.iter  
        (fun chrom ->  
             let seq = chrom.seq in   
             let code = chrom.code in   
             let gap_seq = Sequence.create 1 in  
             let gap_seq = Sequence.prepend_char gap_seq gap in
             let _, _, cost = Sequence.Align.align_2 ~first_gap:false seq
                 gap_seq c2 Matrix.default 
             in  

                 
             gen_cost_mat.(code).(gen_gap_code) <- cost; 
             gen_cost_mat.(code + 1).(gen_gap_code) <- cost; 

             gen_cost_mat.(gen_gap_code).(code) <- cost; 
             gen_cost_mat.(gen_gap_code).(code + 1) <- cost;                 
        ) all_seq_arr;  

    gen_cost_mat.(gen_gap_code).(gen_gap_code) <- 0; 
    print_newline (); 
        
    let gen_cost_ls = List.tl (Array.to_list gen_cost_mat) in 
    let gen_cost_ls = List.map  
        (fun cost_arr -> List.tl (Array.to_list cost_arr) ) gen_cost_ls 
    in  

    let gen_cost_mat = Cost_matrix.Two_D.of_list ~use_comb:false gen_cost_ls in  

    gen_alpha, gen_cost_mat 

let convert_dyna_taxon_data data (ch_ls : (int, cs) Hashtbl.t)
    tran_code_ls transform =
    let add chcode (ch_cs : cs) (acc_cs_ls : (int, cs) Hashtbl.t)
        : (int, cs) Hashtbl.t =
        match ch_cs with 
        | ((Dyna (chcode, dyna_data)) as ch, specified) ->  
              let new_ch = 
                  match (List.mem chcode tran_code_ls) with
                  | true -> 
                        let seq =  dyna_data.seq_arr.(0).seq in
                        let seq_code = dyna_data.seq_arr.(0).code  in
                        let specs = Hashtbl.find data.character_specs chcode in 
                        let specs = 
                            match specs with 
                            | Dynamic d -> d 
                            | _ -> failwith "Data.convert_dyna_taxon_data 1"
                        in 
                        let gap = Alphabet.get_gap specs.alph in 
                        let seq = 
                            match transform with 
                            | `Seq_to_Chrom _ | `Seq_to_Breakinv _ -> 
                                  (match (Sequence.get seq 0) = gap with
                                   | true -> Sequence.del_first_char seq  
                                   | false -> seq)  

                            | `Chrom_to_Seq _ | `Breakinv_to_Seq _ ->
                                    (match (Sequence.get seq 0) = gap with
                                    | true -> seq
                                    | false -> Sequence.prepend_char seq gap)

                            | `Annchrom_to_Breakinv _ ->
                                    let len = Array.length dyna_data.seq_arr in 
                                    Sequence.init 
                                    (fun idx -> dyna_data.seq_arr.(idx).code)
                                    len
                            | `Change_Dyn_Pam _ -> seq 
                        in 
                        let seq_arr = 
                            match transform with 
                            | `Change_Dyn_Pam _ -> dyna_data.seq_arr
                            | _ -> [|{seq=seq; code=seq_code}|]
                        in 
                        let dyna_data = {seq_arr = seq_arr} in
                        Dyna (chcode, dyna_data)
                  | false -> ch
              in
              Hashtbl.replace acc_cs_ls chcode (new_ch, specified);
              acc_cs_ls
        | _ -> 
                Hashtbl.replace acc_cs_ls chcode ch_cs;
                acc_cs_ls
    in  
    Hashtbl.fold add ch_ls (create_ht ())

let get_state default = function
    | `Seq_to_Chrom _ -> `Chromosome
    | `Seq_to_Breakinv _ 
    | `Annchrom_to_Breakinv _ -> `Breakinv
    | `Chrom_to_Seq _ | `Breakinv_to_Seq _ -> `Seq
    | `Change_Dyn_Pam _ -> default

let convert_dyna_spec data chcode spec transform_meth =  
    match spec with   
    | Dynamic _ ->
            let dspec = 
                match spec with
                | Dynamic x -> x
                | _ -> failwith "Impossible"
            in
        let _ =
            (* First check if the transformation is legal  *)
            match dspec.state, transform_meth with  
            | `Seq, `Seq_to_Chrom _
            | `Seq, `Seq_to_Breakinv _
            | `Annotated, `Annchrom_to_Breakinv _
            | `Chromosome, `Chrom_to_Seq  _
            | `Breakinv, `Breakinv_to_Seq _
            | _, `Change_Dyn_Pam _ -> ()
            | _, _ -> failwith "Illegal character transformation requested"
        in
        (match transform_meth with
        | transform_meth ->
            let (al, c2), pam = 
                (* Now we can transform *)
                match transform_meth with 
                | `Annchrom_to_Breakinv pam_ls -> 
                        create_alpha_c2_breakinvs data chcode,
                        set_dyna_pam pam_ls

                | `Seq_to_Chrom pam_ls
                | `Seq_to_Breakinv pam_ls
                | `Change_Dyn_Pam pam_ls
                | `Chrom_to_Seq pam_ls 
                | `Breakinv_to_Seq pam_ls ->
                        let pam = set_dyna_pam pam_ls in
                        (dspec.alph, dspec.tcm2d), pam
            in
            Dynamic { dspec with alph = al; tcm2d = c2; pam = pam; state = 
                get_state dspec.state transform_meth })
    | _ -> failwith "Convert_dyna_spec: Not a dynamic character" 


let rec get_code_from_name data name_ls = 
  let code_ls = List.fold_right 
      ~f:(fun name acc -> 
              try 
                  let code = Hashtbl.find data.character_names name in
                  code::acc
              with 
              | Not_found -> 
                      (* We will try to use a regular expression to match the
                      * requested item *)
                    let nname = Str.regexp name in
                    match Hashtbl.fold (fun stored_name code acc -> 
                        if Str.string_match nname stored_name 0 then code :: acc
                        else acc) data.character_names []
                    with
                    | [] -> 
                            failwith 
                            ("The@ character@ " ^ name ^ 
                            ",@ target@ of@ the@ transformation@ does@ not@ " ^
                            "exist.");
                    | r -> r @ acc
         ) name_ls ~init:[] 
  in
  code_ls

and get_code_with_missing dont_complement data fraction = 
    let taxa = 
        let tmp = 
            All_sets.StringMap.fold (fun _ _ acc -> acc + 1)
            data.taxon_names 0
        in
        float_of_int tmp
    in
    let count_occurrences data =
        let extract_info data =
            Hashtbl.fold (fun code _ acc -> 
                All_sets.IntegerMap.add code 0 acc) 
            data.character_codes All_sets.IntegerMap.empty
        in
        let add_counter _ x counter =
            match x with
            | Dyna (y, _), `Specified 
            | Stat (y, _), `Specified -> 
                    let cnt = All_sets.IntegerMap.find y counter in
                    All_sets.IntegerMap.add y (cnt + 1) counter
            | _, `Unknown -> counter
        in
        let add_taxon_to_character_counters _ taxon_cs_lst counters = 
            Hashtbl.fold add_counter taxon_cs_lst counters
        in
        Hashtbl.fold add_taxon_to_character_counters
        data.taxon_characters (extract_info data)
    in
    let fraction = (float_of_int fraction) /. 100. in
    let codes = 
        All_sets.IntegerMap.fold (fun code count acc ->
        if fraction <= ((float_of_int count) /. taxa) then code :: acc
        else acc) (count_occurrences data) []
    in
    if dont_complement then codes
    else 
        match complement_characters data (`Some codes ) with
        | `Some x -> x
        | _ -> failwith "Data.get_code_with_missing"
(**Give a list of characters, return their codes*)    
and get_code_from_characters_restricted kind (data : d) (chs : characters) = 
    let kind_lst = 
        match kind with
        | `Dynamic -> data.dynamics
        | `NonAdditive ->
                        data.non_additive_8 @
                        data.non_additive_16 @
                        data.non_additive_32 @
                        data.non_additive_33
        | `Additive -> data.additive
        | `Sankoff -> List.flatten data.sankoff
        | `AllDynamic -> data.dynamics
        | `AllStatic -> 
                        data.non_additive_8 @
                        data.non_additive_16 @
                        data.non_additive_32 @
                        data.additive @ data.non_additive_33 @
                        (List.flatten data.sankoff)
    in
    let items = 
        match chs with
        | `Some code_ls -> code_ls 
        | `Names name_ls -> get_code_from_name data name_ls
        | `All -> kind_lst
        | `AllDynamic | `AllStatic as m -> 
                get_code_from_characters_restricted m data `All
        | `Missing (dont_complement, fraction) ->
                get_code_with_missing dont_complement data fraction
    in
    List.filter (fun x -> List.exists (fun y -> y = x) kind_lst) items
and get_all_codes data =
    Hashtbl.fold (fun c _ acc -> c :: acc) data.character_codes  []
and get_chars_codes data = function
    | `All -> get_all_codes data 
    | `Some codes -> codes
    | `Names names ->
            let names = 
                warn_if_repeated_and_choose_uniquely names 
                "selected@ characters@ " ""
            in
            let get_code acc name =
                try 
                    (Hashtbl.find data.character_names name) :: acc 
                with
                | Not_found as err ->
                        let nname = Str.regexp name in
                        match
                            Hashtbl.fold (fun item code acc ->
                                if Str.string_match nname item 0 then 
                                    code :: acc
                                else acc)
                            data.character_names acc
                        with
                        | [] -> 
                                Status.user_message Status.Error
                                ("Could@ not@ find@ any@ character@ matching@
                                the@ expression@ " ^ name);
                                raise err
                        | r -> r @ acc
            in
            List.fold_left ~f:get_code ~init:[] names
    | `AllStatic | `AllDynamic as m -> 
            get_code_from_characters_restricted m data `All
    | `Missing (dont_complement, fraction) ->
            get_code_with_missing dont_complement data fraction
and complement_characters data characters = 
    let codes = get_chars_codes data characters in
    let res = Hashtbl.fold (fun x _ acc -> 
        if List.exists (fun y -> x = y) codes then acc
        else x :: acc) data.character_codes [] in
    `Some res

(**Give a list of characters, return their codes*)    
let rec get_code_from_characters_restricted kind (data : d) (chs : characters) = 
    let kind_lst = 
        match kind with
        | `Dynamic -> data.dynamics
        | `NonAdditive ->
                        data.non_additive_8 @
                        data.non_additive_16 @
                        data.non_additive_32 @
                        data.non_additive_33
        | `Additive -> data.additive
        | `Sankoff -> List.flatten data.sankoff
        | `AllDynamic -> data.dynamics
        | `AllStatic -> 
                        data.non_additive_8 @
                        data.non_additive_16 @
                        data.non_additive_32 @
                        data.additive @ data.non_additive_33 @
                        (List.flatten data.sankoff)
    in
    let items = 
        match chs with
        | `Some code_ls -> code_ls 
        | `Names name_ls -> get_code_from_name data name_ls
        | `All -> kind_lst
        | `AllDynamic | `AllStatic as m -> 
                get_code_from_characters_restricted m data `All
        | `Missing (dont_complement, fraction) ->
                get_code_with_missing dont_complement data fraction
    in
    List.filter (fun x -> List.exists (fun y -> y = x) kind_lst) items

let get_all_codes data =
    Hashtbl.fold (fun c _ acc -> c :: acc) data.character_codes  []

let get_code_from_characters_restricted_comp kind d ch =
    let dont_complement, chars = 
        match ch with
        | `Some (dont_complement, x) -> dont_complement, `Some x
        | `Names (dont_complement, x) -> dont_complement, `Names x
        | `Missing _ | `All | `AllDynamic | `AllStatic as x -> true, x
    in
    let chars = get_code_from_characters_restricted kind d chars in
    if dont_complement then chars
    else 
        match complement_characters d (`Some chars) with
        | `Some x -> x
        | _ -> failwith "Impossible?"


let get_chars_codes_comp data ch =
    let dont_complement, ch = 
        match ch with
        | `Some (x, y) -> x, `Some y
        | `Names (x, y) -> x, `Names y 
        | `Missing _ | `All | `AllStatic | `AllDynamic as x -> true, x
    in
    let codes = get_chars_codes data ch in
    if dont_complement then codes 
    else 
        match complement_characters data (`Some codes) with
        | `Some x -> x
        | _ -> failwith "Impossible?"

let get_tran_code_meth data meth = 
    let tran_code_ls, meth =
        let a, b = 
            match meth with
            | `Seq_to_Chrom (a, b) ->
                    a, `Seq_to_Chrom b
            | `Seq_to_Breakinv (a, b) ->
                    a, `Seq_to_Breakinv b
            | `Annchrom_to_Breakinv (a, b) ->
                    a, `Annchrom_to_Breakinv b
            | `Change_Dyn_Pam (a, b) ->
                    a, `Change_Dyn_Pam b
            | `Chrom_to_Seq (a, b) ->
                    a, `Chrom_to_Seq b
            | `Breakinv_to_Seq (a, b) ->
                    a, `Breakinv_to_Seq b
        in
        let dont_complement, codes =
            match a with
            | `Some (dont_complement, codes) ->
                    dont_complement, `Some codes
            | `Names (dont_complement, names) ->
                    dont_complement, `Names names
            | `Missing _ | `All | `AllDynamic | `AllStatic as x -> true, x
        in
        let codes = get_code_from_characters_restricted `AllDynamic data codes in
        let codes = 
            if dont_complement then codes
            else 
                match complement_characters data (`Some codes) with
                | `Some x -> x
                | _ -> failwith "Impossible?"
        in
        codes, b
    in
    tran_code_ls, meth

(** transform all sequences whose codes are on the code_ls into chroms *)    
let transform_dynamic meth data =
    let data = duplicate data in
    let tran_code_ls, meth = get_tran_code_meth data meth in 
    Hashtbl.iter
    (fun code spec ->
         if List.mem code tran_code_ls then 
             Hashtbl.replace data.character_specs code 
             (convert_dyna_spec data code spec meth)
         else ()) data.character_specs;
    let new_taxon_chs = 
        let new_tbl = create_ht () in
        Hashtbl.iter 
        (fun code ch_ls -> 
            let new_ls = convert_dyna_taxon_data data ch_ls tran_code_ls meth in
            Hashtbl.add new_tbl code new_ls) data.taxon_characters;
        new_tbl
    in 
    {data with taxon_characters = new_taxon_chs}










let intmap_filter f y =
    All_sets.IntegerMap.fold (fun a b acc ->
        if f a b then All_sets.IntegerMap.add a b acc
        else acc) y All_sets.IntegerMap.empty

let hashtbl_filter f y =
    Hashtbl.iter (fun a b ->
        if not (f a b) then Hashtbl.remove y a
        else ()) y;
    y

let process_ignore_character report data code_set =
    let data = duplicate data in
    let rep msg = 
        if report then
            Status.user_message (Status.Output (None, false, [])) msg
        else ()
    in
    rep "@[Characters@ excluded:@[<v 2>@,@[<v>";
    let compare x = not (All_sets.Integers.mem x code_set) in
    let compare1 code _ = not (All_sets.Integers.mem code code_set) in
    try
        (* Remove each character in the code set *)
        let new_cign =
            All_sets.Integers.fold (fun code new_cign ->
                let name = 
                    Hashtbl.find data.character_codes code 
                in
                rep (name ^ "@,");
                Hashtbl.remove data.character_names name;
                Hashtbl.remove data.character_codes code;
                Hashtbl.remove data.character_specs code;
                name :: new_cign) code_set 
                data.ignore_character_set
        in
        rep "@]@]@]@\n%!";
        let non_additive_8 = List.filter compare data.non_additive_8
        and non_additive_16 = List.filter compare data.non_additive_16
        and non_additive_32 = List.filter compare data.non_additive_32
        and non_additive_33 = List.filter compare data.non_additive_33 
        and additive = List.filter compare data.additive 
        and sankoff = List.map (fun x -> List.filter compare x) data.sankoff 
        and dynamics = List.filter compare data.dynamics in
        Hashtbl.iter (fun code lst ->
            Hashtbl.replace data.taxon_characters code 
            (hashtbl_filter compare1 lst)) data.taxon_characters;
        let sankoff = List.filter (function [] -> false | _ -> true) sankoff in
        { data with
        ignore_character_set = new_cign;
        non_additive_8 = non_additive_8;
        non_additive_16 = non_additive_16;
        non_additive_32 = non_additive_32;
        non_additive_33 = non_additive_33;
        additive = additive;
        sankoff = sankoff;
        dynamics = dynamics;
        }
    with
    | Not_found -> 
            rep "@]@]@]@\n%!";
            let msg = 
                "Could not find a character " ^
                ". I will ignore it and continue without processing it." 
            in
            Status.user_message Status.Error msg;
            data

let process_ignore_characters report data characters = 
    let codes = get_chars_codes data characters in
    let codes = 
        List.fold_left ~f:(fun acc x -> All_sets.Integers.add x acc)
        ~init:All_sets.Integers.empty codes
    in
    process_ignore_character report data codes

let process_analyze_only_characters_file report dont_complement data files =
    let codes = 
        List.fold_left ~f:(fun acc x ->
            let ch, x = FileStream.channel_n_filename x in
            let items = Parser.IgnoreList.of_channel ch in
            close_in ch;
            let items = 
                warn_if_repeated_and_choose_uniquely items "characters@ file@ "
                x
            in
            let codes = get_chars_codes data (`Names items) in
            acc @ codes) ~init:[] files
    in
    let items = 
        if dont_complement then  (`Some codes)
        else complement_characters data (`Some codes) 
    in
    process_ignore_characters report data items

let process_ignore_characters_file report data file =
    try
        let ch, file = FileStream.channel_n_filename file in
        let items = Parser.IgnoreList.of_channel ch in
        let items = List.map trim items in
        close_in ch;
        process_ignore_characters report data (`Names items)
    with
    | err ->
            let file = FileStream.filename file in
            let msg = "Error while attempting to read the " ^
            "ignore characters file " ^ file ^ ". I will continue " 
            ^ "without processing it." in
            Status.user_message Status.Error msg;
            data

let replace_name_in_spec name = function
    | Static (e, _) -> Static (e, name)
    | Dynamic dspec -> Dynamic { dspec with filename = name }
    | Set -> Set


let process_rename_characters data (a, b) = 
    let data = duplicate data in
    if Hashtbl.mem data.character_names b then
        raise Illegal_argument
    else begin
        let code = character_code a data in
        let spec = Hashtbl.find data.character_specs code in
        Hashtbl.replace data.character_names b code;
        Hashtbl.replace data.character_codes code b;
        Hashtbl.replace data.character_specs code (replace_name_in_spec b spec);
        data
    end

exception Invalid_Character of int



(** transform all sequences whose codes are on the code_ls into chroms 
 * each ia for one character*)    
let transform_chrom_to_rearranged_seq data meth tran_code_ls 
        (ia_ls : ((int * (int array list IntMap.t) list) list list) list) = 
    let data = duplicate data in

    let tran_code_ls, _ = get_tran_code_meth data meth in 

    let num_ia = ref 0 in  
    let t_ch_ia_map = List.fold_left 
        ~f:(fun (t_ch_ia_map : int array list FullTupleMap.t) ia  ->
             List.fold_left 
                 ~f:(fun t_ch_ia_map handle_ia ->
                      List.fold_left 
                          ~f:(fun t_ch_ia_map (t_id, t_ia) ->
                               List.fold_left 
                                   ~f:(fun t_ch_ia_map (ch_set_ia : int array list IntMap.t)->
                                           IntMap.fold  
                                            (fun chcode (t_ch_ia : int array list) t_ch_ia_map ->
                                                 num_ia := List.length t_ch_ia;
                                                 FullTupleMap.add (t_id,chcode) t_ch_ia t_ch_ia_map 
                                               ) ch_set_ia t_ch_ia_map 
                                   ) ~init:t_ch_ia_map t_ia
                          ) ~init:t_ch_ia_map handle_ia
                 ) ~init:t_ch_ia_map ia
           ) ~init:FullTupleMap.empty ia_ls  
    in  

    let data = List.fold_left 
        ~f:(fun data char_code ->
             let char_name = Hashtbl.find data.character_codes char_code in             
             let deled_data = process_ignore_characters true data (`Some [char_code]) in

(*
             print_endline "Deleted data";
             print deled_data;
*)   

             let seqs = IntMap.fold  
                 (fun (t_code : int) (t_name : string) seqs ->
                      try
                          let ia_ls = FullTupleMap.find (t_code, char_code) t_ch_ia_map in
                          let ia_ls = List.map 
                              (fun ia ->
                                   let seq = Sequence.of_code_arr ia Alphabet.gap in 
                                   let seq = Sequence.prepend_char seq Alphabet.gap in 
                                   seq
                              ) ia_ls
                          in 
(*
                          Printf.fprintf stdout "%s: " t_name;
                          List.iter (fun seq ->
                                         Sequence.print stdout seq Alphabet.nucleotides;
                                         Printf.fprintf stdout " | ";
                                    ) ia_ls; 
                          print_newline (); 
*)
                          ([[ia_ls]], t_name)::seqs                              
                      with Not_found -> seqs                              
                 ) data.taxon_codes []
             in                   
             let added_data = process_parsed_sequences Alphabet.nucleotides
                     char_name `Seq deled_data seqs;
             in 
                 

             added_data
           ) ~init:data tran_code_ls 
    in 
    
    categorize data

let assign_tcm_to_characters data chars file tcm =
    (* Get the character codes and filter those that are of the sequence class.
    * This allows simpler specifications by the users, for example, even though
    * morphological characters are loaded, an (all, create_tcm:(1,1)) will
    * operate properly in all the characters that are valid in the context. *)
    let data = duplicate data in
    let chars = get_chars_codes_comp data chars in
    let chars = List.filter (fun x -> (List.exists (fun y -> x = y)
    data.dynamics) ) chars in
    let tcm3 = Cost_matrix.Three_D.of_two_dim tcm in
    let chars_specs =
        List.fold_left 
        ~f:(fun acc x -> 
            let res = Hashtbl.find data.character_specs x in
            let acc = (res, x) :: acc in
            Hashtbl.remove data.character_specs x;
            acc
        ) 
        ~init:[] chars
    in
    let new_charspecs = 
        List.map 
        (function ((Dynamic dspec), code) ->
            (Dynamic { dspec with tcm = file; tcm2d = tcm; tcm3d = tcm3 }), 
            code
            | _, code -> raise (Invalid_Character code)) chars_specs
    in
    let files = 
        if List.exists (fun (x, _) -> x = file) data.files then data.files
        else (file, [CostMatrix]) :: data.files 
    in
    List.iter ~f:(fun (spec, code) -> 
        Hashtbl.replace data.character_specs code spec) 
    new_charspecs;
    { data with files = files }

let assign_tcm_to_characters_from_file data chars file =
    let tcm, file =
        match file with
        | None -> Cost_matrix.Two_D.default, ""
        | Some f -> 
                Parser.TransformationCostMatrix.of_file f, 
                (FileStream.filename f)
    in
    assign_tcm_to_characters data chars file tcm

let ( --> ) a b = b a

let classify_characters_by_alphabet_size data chars =
    let is_dynamic_character x = 
        (List.exists (fun y -> x = y) data.dynamics)
    in
    let make_tuple_of_character_and_size acc char =
        let size = 
            data 
            --> get_sequence_alphabet char
            --> Alphabet.simplified_alphabet 
            --> Alphabet.distinct_size
        in
        (char, size) :: acc
    in
    let classify_by_size list =
        let sets = 
            List.fold_left ~f:(fun acc (code, size) ->
                if All_sets.IntegerMap.mem size acc then
                    let prev = All_sets.IntegerMap.find size acc in
                    All_sets.IntegerMap.add size (code :: prev) acc
                else
                    All_sets.IntegerMap.add size [code] acc)
            ~init:All_sets.IntegerMap.empty list
        in
        All_sets.IntegerMap.fold (fun a b acc -> (a, `Some (true, b)) :: acc)
        sets []
    in
    chars 
    --> get_chars_codes_comp data 
    --> (List.filter ~f:(is_dynamic_character))
    --> List.fold_left ~f:make_tuple_of_character_and_size ~init:[]
    --> classify_by_size

let assign_transformation_gaps data chars transformation gaps = 
    let name = 
        ("Substitutions:" ^ string_of_int transformation ^ 
        ", Indels:" ^ string_of_int gaps)
    in
    let alphabet_sizes = classify_characters_by_alphabet_size data chars in
    List.fold_left ~f:(fun data (size, chars) ->
        let size = size - 1 in
        let tcm = 
            Cost_matrix.Two_D.of_transformations_and_gaps (size < 7) size 
            transformation gaps
        in
        assign_tcm_to_characters data chars name tcm) ~init:data alphabet_sizes

let get_tcm2d data c =
    match Hashtbl.find data.character_specs c with
    | Dynamic dspec -> dspec.tcm2d
    | _ -> failwith "Data.get_alphabet"

let codes_with_same_tcm codes data =
    (* This function assumes that the codes have already been filtered by class
    * *)
    let rec assign_matching acc ((code : int), tcm) =
        match acc with
        | (codes, assgn) :: tl when tcm == assgn ->
                ((code :: codes), assgn) :: tl
        | hd :: tl ->
                hd :: (assign_matching tl (code, tcm))
        | [] -> [([code], tcm)]
    in
    let codes = List.map ~f:(fun x -> x, get_tcm2d data x) codes in
    List.fold_left ~f:assign_matching ~init:[] codes

let rec assign_affine_gap_cost data chars cost =
    let codes = 
        get_code_from_characters_restricted_comp `AllDynamic data chars
    in
    let codes = codes_with_same_tcm codes data in
    let codes = List.map (fun (a, b) -> 
        let b = Cost_matrix.Two_D.clone b in
        Cost_matrix.Two_D.set_affine b cost;
        (true, a), b) codes
    in
    List.fold_left ~f:(fun acc (a, b) ->
        assign_tcm_to_characters acc (`Some a) "" b) ~init:data codes

let rec assign_prep_tail filler data chars filit =
    match filit with
    | `File x ->
            let ch = FileStream.open_in x in
            let lst = Cost_matrix.Two_D.load_file_as_list ch in
            let arr = Array.of_list lst in
            assign_prep_tail filler data chars (`Array arr)
    | `Array arr ->
            let codes = 
                get_code_from_characters_restricted_comp `AllDynamic data chars
            in
            let codes = codes_with_same_tcm codes data in
            let codes = List.map (fun (a, b) -> 
                let b = Cost_matrix.Two_D.clone b in
                filler arr b;
                (true, a), b) codes
            in
            List.fold_left ~f:(fun acc (a, b) ->
                assign_tcm_to_characters acc (`Some a) "" b) ~init:data codes

let assign_prepend data chars filit =
    assign_prep_tail Cost_matrix.Two_D.fill_prepend data chars filit

let assign_tail data chars filit =
    assign_prep_tail Cost_matrix.Two_D.fill_tail data chars filit

(** [process_complex_terminals data filename] reads a complex terminals file
    ([filename]) and adds the specification to [data].  It will raise
    exceptions if the reading fails. *)
let process_complex_terminals data filename =
    let groups = Parser.SetGroups.of_file filename in
    if groups <> []
    then begin
            let group_type = Parser.SetGroups.unify_list groups in
            let groups =
                List.map (Parser.SetGroups.coerce_to_type group_type)
                    groups in
            (* create ids and types *)
            { data with complex_schema = groups }
        end
    else { data with complex_schema = [] }

let get_pool data c =
    match Hashtbl.find data.character_specs c with
    | Dynamic dspec -> dspec.pool
    | _ -> failwith "Data.get_alphabet"

let get_alphabet data c =
    match Hashtbl.find data.character_specs c  with
    | Dynamic dspec -> dspec.alph
    | _ -> failwith "Data.get_alphabet"

let to_faswincladfile data filename =
    (*
    let by_name data x y = 
        let namex = Hashtbl.find data.character_codes x 
        and namey = Hashtbl.find data.character_codes y in
        String.compare namex namey 
    in
    *)
    let has_sankoff =
        match data.sankoff with
        | [] -> false
        | _ -> true
    in
    let fo = Status.user_message (Status.Output (filename, false,
    [StatusCommon.Margin 0])) in
    let all_chars = 
        [data.non_additive_8; data.non_additive_16; data.non_additive_32; 
        data.additive; (List.flatten data.sankoff)] 
    in
    let all_of_all = List.flatten all_chars in
    let all_of_all = List.sort (compare) all_of_all in
    let number_of_characters = List.length all_of_all in
    let number_of_taxa = 
        Hashtbl.fold (fun _ _ x -> x + 1) data.taxon_characters 0 
    in
    let int_list_to_set code data x = 
        (List.fold_left ~f:(fun acc x -> 
            let x = 
                try 
                    let used_observed = get_used_observed code data in
                    Hashtbl.find used_observed x 
                with
                | _ -> x
            in
            acc ^ string_of_int x ^ if has_sankoff then "." else "") ~init:"[" x) ^
        "]"
    in
    let get_bits elt =
        let rec elt_iter acc elt c =
            if elt = 0 then acc 
            else if (elt land 1) <> 0 then 
                elt_iter (c :: acc) (elt lsr 1) (c * 2)
            else elt_iter acc (elt lsr 1)  (c * 2)
        in
        elt_iter [] elt 1
    in
    let state_to_string code t =
        match t with
        | Parser.Ordered_Character (a, b, c) ->
                let used_observed = get_used_observed code data in
                if c then "?" 
                else if a = b then string_of_int (Hashtbl.find used_observed a)
                else int_list_to_set code data [a; b]
        | Parser.Unordered_Character (a, b) ->
                if b then "?"
                else 
                    let used_observed = get_used_observed code data in
                    let bits = get_bits a in
                    if 1 = List.length bits then 
                        string_of_int 
                        (Hashtbl.find used_observed (List.hd bits))
                    else int_list_to_set code data bits
        | Parser.Sankoff_Character (a, b) ->
                if 1 = List.length a then string_of_int (List.hd a)
                else int_list_to_set code data a
        | _ -> failwith "Fastwinclad files do not support sequences"
    in
    let produce_character fo taxon charset code =
        let _ =
            try
                match Hashtbl.find charset code with
                | (_, `Unknown) -> fo "?"
                | (spec, _) ->
                        match spec with
                        | Stat (_, t) -> 
                                fo (state_to_string code t)
                        | Dyna _ -> 
                                failwith 
                                "Fastwinclad files do not support sequences"
            with
            | Not_found ->
                    Status.user_message Status.Error
                    ("@[I@ could@ not@ find@ the@ character@ with@ code@ " ^ 
                    string_of_int code ^ ".@ This@ is@ a@ bug,@ so@ please@ " ^
                    "report@ it@ to@ Andres...");
                    fo "?"
        in
        fo " "
    in
    let output_taxon tid name = 
        if All_sets.Strings.mem name data.ignore_taxa_set then
            ()
        else begin
            fo "@[";
            fo name;
            fo " ";
            let _ =
                let charset = get_taxon_characters data tid in
                List.iter (produce_character fo tid charset) all_of_all
            in
            fo "@\n@]%!";
        end
    in
    let output_all_taxa () = 
        All_sets.IntegerMap.iter output_taxon data.taxon_codes;
        fo ";@\n";
    in
    let output_header () = 
        fo (if has_sankoff then "dpread@\n" else "xread@\n");
        fo (string_of_int number_of_characters);
        fo " ";
        fo (string_of_int number_of_taxa);
        fo "@\n";
    in
    let get_tcm code = 
        match Hashtbl.find data.character_specs code with
        | Static (enc, _) -> Parser.Hennig.Encoding.get_tcm enc 
        | _ -> failwith "Sequence characters are not supported in fastwinclad"
    in
    let output_weights (acc, pos) code = 
        match Hashtbl.find data.character_specs code with
        | Static (enc, _) ->
                let weight = Parser.Hennig.Encoding.get_weight enc in 
                if weight = 1 then (acc, pos + 1)
                else (acc ^ "ccode /" ^ string_of_int weight ^ " " ^ 
                string_of_int pos ^ ";@\n", pos + 1)
        | _ -> failwith "Sequence characters are not supported in fastwinclad"
    in
    let weights, _ = 
        List.fold_left ~f:output_weights ~init:("", 0) all_of_all 
    in
    let output_character_types () =
        (* We first output the non additive character types *)
        let unolen = 
            (List.length data.non_additive_8) + 
            (List.length data.non_additive_16) +
            (List.length data.non_additive_32) 
        and olen = List.length data.additive in
        fo ((if unolen > 0 then "cc - 0." ^ string_of_int (unolen - 1) ^ 
        ";@\n" else "") ^ (if olen > 0 then 
            ("cc + " ^ string_of_int unolen ^ "." ^ 
        string_of_int (unolen + olen - 1)) else "") ^ "@\n");
        (* Now we output the saknoff character types *)
        if has_sankoff then
            let output_matrix m = 
                Array.iter (fun x ->
                    (Array.iter (fun y -> 
                        fo (string_of_int y);
                        fo " ") x; fo "@\n")) m;
                        fo ";@\n"
            in
            let output_codes m =
                Array.iteri (fun pos _ -> 
                    fo (string_of_int pos);
                    fo " ") m.(0);
                fo "@\n"
            in
            let output_element position code =
                let tcm = get_tcm code in 
                fo ("costs [ " ^ string_of_int position ^ " $" ^
                string_of_int (Array.length tcm) ^ "@\n");
                output_codes tcm;
                output_matrix tcm;
                position + 1
            in
            let _ = 
                List.fold_left ~f:output_element 
                ~init:(unolen + olen) (List.flatten data.sankoff)
            in
            ()
        else ()
    in
    fo "@[<v 0>";
    output_header ();
    output_all_taxa ();
    output_character_types ();
    fo weights;
    fo "@\n";
    fo "@]"

let report_taxon_file_cross_reference chars data filename =
    let files_arr, taxa = 
        match chars with
        | None -> (* The user requested the data per file *)
                let files_arr = 
                    let filtered_files = 
                        let has_char (_, c) =
                            List.exists (function Characters -> true | _ ->
                                false) c
                        in
                        let filtered = List.filter has_char data.files in
                        List.map (fun (a, _) -> a) filtered
                    in
                    Array.of_list ("Terminal" :: filtered_files) in
                let taxa =
                    All_sets.StringMap.fold 
                     (fun taxon files arr ->
                         let new_arr = Array.mapi (fun pos file ->
                             if pos = 0 then taxon
                             else if All_sets.Strings.mem file files then "+"
                             else "-") files_arr
                         in
                         new_arr :: arr) data.taxon_files []
                in
                files_arr, taxa
        | Some chars -> (* The user requested the data for some characters *)
                let is_specified code_taxon code_char data = 
                    (* Establish if a character was parte of the input of a
                    * taxon or not *)
                    let taxon_specs = 
                        Hashtbl.find data.taxon_characters code_taxon
                        
                    in
                    try 
                        match Hashtbl.find taxon_specs code_char with
                        | (_, `Specified) -> true
                        | (_, `Unknown) -> false
                    with
                    | Not_found -> false
                in
                let codes = get_chars_codes_comp data chars in
                let codes_arr = Array.of_list codes 
                and chars_arr = 
                    let name x = Hashtbl.find data.character_codes x in
                    Array.of_list ("Terminal" :: List.map name codes)
                in
                let taxa = 
                    All_sets.IntegerMap.fold
                    (fun code taxon acc ->
                        let new_arr = Array.mapi (fun pos file ->
                            if pos = 0 then taxon
                            else if 
                                is_specified code (codes_arr.(pos - 1)) data 
                            then "+"
                            else "-") chars_arr
                        in
                        new_arr :: acc) data.taxon_codes []
                in
                chars_arr, taxa
    in
    let files_arr = Array.map (fun x ->
        "@{<u>" ^ Filename.basename x ^ "@}") files_arr
    in
    let fo = Status.Output (filename, false, []) in
    Status.user_message fo "@[<v 2>@{<b>File References:@}@,@[";
    Status.output_table fo 
    (Array.of_list (files_arr :: (List.rev taxa)));
    Status.user_message fo "@]@]@."



let find_max_seq_id data = 
    let max_seq_id = Hashtbl.fold  
        (fun key cs_ls max_seq_id ->
            Hashtbl.fold
            (fun _ cs max_seq_id -> 
                let csd, _ = cs in 
                match csd with
                | Dyna (_, dyna_data) ->
                        Array.fold_left 
                        ~f:(fun max_seq_id seq -> max max_seq_id seq.code
                ) ~init:max_seq_id dyna_data.seq_arr 
                | _ -> max_seq_id) 
            cs_ls 
            max_seq_id) 
        data.taxon_characters 
        0  
    in 
    max_seq_id + 2



let flush d = 
    Hashtbl.iter (fun _ item ->
        match  item with
        | Dynamic dspec -> Sequence.Pool.flush dspec.pool
        | _ -> ()) d.character_specs 

let set_weight weight spec =
    match spec with
    | Static (enc, str) ->
            Static (Parser.Hennig.Encoding.set_weight enc (int_of_float
            weight), str)
    | Dynamic enc ->
            Dynamic ({ enc with weight = weight })
    | _ -> spec

let set_weight_factor weight spec =
    match spec with
    | Static (enc, str) ->
            let new_weight =
                let prev_weight = 
                    float_of_int 
                    (Parser.Hennig.Encoding.get_weight enc)
                in
                int_of_float (weight *. prev_weight)
            in
            Static (Parser.Hennig.Encoding.set_weight enc
            new_weight, str)
    | Dynamic enc ->
            Dynamic ({ enc with weight = enc.weight *. weight })
    | _ -> spec

let aux_transform_weight meth data =
    let f = 
        match meth with
        | `ReWeight (chars, weight) ->
                let chars = 
                    let codes = get_chars_codes_comp data chars in
                    List.fold_left ~f:(fun acc x -> All_sets.Integers.add x acc)
                    ~init:All_sets.Integers.empty codes
                in
                (fun code spec ->
                    if All_sets.Integers.mem code chars then
                        set_weight weight spec
                    else spec)
        | `WeightFactor (chars, weight) ->
                let chars = 
                    let codes = get_chars_codes_comp data chars in
                    List.fold_left ~f:(fun acc x -> All_sets.Integers.add x acc)
                    ~init:All_sets.Integers.empty codes
                in
                (fun code spec ->
                    if All_sets.Integers.mem code chars then
                        set_weight_factor weight spec
                    else spec)
    in
    Hashtbl.iter (fun code char ->
        Hashtbl.replace data.character_specs code (f code char)) 
    data.character_specs

let transform_weight meth data = 
    let data = duplicate data in
    aux_transform_weight meth data;
    data

let file_exists data filename =
    List.exists (fun (x, _) -> x = FileStream.filename filename) data.files

let complement_taxa data taxa = 
    let taxa = 
        List.fold_left ~f:(fun acc x -> All_sets.Integers.add x acc)
        ~init:All_sets.Integers.empty taxa
    in
    All_sets.IntegerMap.fold (fun c _ acc -> 
        if All_sets.Integers.mem c taxa then acc
        else c :: acc) data.taxon_codes []

let make_fixed_states chars data =
    let data = duplicate data in
    let convert_and_process data code =
        let name = Hashtbl.find data.character_codes code in
        match Hashtbl.find data.character_specs code with
        | Dynamic dhs -> 
                let process_taxon tcode chars acc =
                    let tname = code_taxon tcode data in
                    let (tc, _) = Hashtbl.find chars code in
                    let seq =
                        match tc with
                        | Dyna (_, c) -> (c.seq_arr.(0)).seq
                        | _ -> failwith "Impossible?"
                    in
                    (seq, tname) :: acc
                in
                let taxa = 
                    Hashtbl.fold process_taxon 
                    data.taxon_characters []
                in
                let file = 
                    Parser.FixedStatesDict.create_dp_read_file "" taxa [] 
                    dhs.tcm2d 
                in
                Status.user_message Status.Information 
                ("I@ will@ store@ the@ fixed@ states@ dpread@ file@ of@ " ^
                "character@ " ^ name ^ "@ in@ " ^ file);
                begin try
                    let char_name = data --> code_character code in
                    let ch, file = 
                        FileStream.channel_n_filename (`Local file) 
                    in
                    let r = Parser.Hennig.of_channel ch in
                    close_in ch;
                    gen_add_static_parsed_file false data char_name r
                with
                | Sys_error err ->
                        let msg = "Couldn't@ open@ file@ " ^ file 
                        ^ "@ to@ load@ the@ " ^
                        "data.@ @ The@ system@ error@ message@ is@ "
                            ^ err ^
                        "." in
                        output_error msg;
                        data
                end
        | _ -> failwith "How could this happen?"
    in
    let codes = get_code_from_characters_restricted_comp `Dynamic data chars in
    let data = List.fold_left ~f:convert_and_process ~init:data codes in
    process_ignore_character false data (List.fold_left ~f:(fun acc x ->
        All_sets.Integers.add x acc) ~init:All_sets.Integers.empty codes)

let number_of_taxa d = 
    Hashtbl.fold  (fun _ _ num_taxa -> num_taxa + 1) d.taxon_characters 0  

let has_dynamic d = 
    match d.dynamics with
    | [] -> false
    | _ -> true


(** Functions to modify the taxon codes *)
let change_taxon_codes reorder_function data =
    let data = duplicate data in
    (* First we produce a hash table with an randomized reassignment of codes
    * for the taxa in data *)
    let htbl =
        let taxon_codes = 
            All_sets.StringMap.fold (fun _ code acc -> 
                code :: acc) data.taxon_names []
        in
        let chars = Array.of_list taxon_codes 
        and chars_org = Array.of_list taxon_codes in
        reorder_function chars;
        let htbl = Hashtbl.create 1667 in
        for i = 0 to (Array.length chars_org) - 1 do
            Hashtbl.add htbl chars_org.(i) chars.(i);
        done;
        htbl
    in
    (* Now that we have the assignment, we proceed to modify the contents of the
    * new data. *)
    let taxon_names = 
        All_sets.StringMap.fold (fun name old_code acc ->
            All_sets.StringMap.add name (Hashtbl.find htbl old_code) acc)
        data.taxon_names All_sets.StringMap.empty
    and taxon_codes =
        All_sets.IntegerMap.fold (fun old_code name acc ->
            All_sets.IntegerMap.add (Hashtbl.find htbl old_code) name acc)
        data.taxon_codes All_sets.IntegerMap.empty
    and taxon_characters =
        let res = Hashtbl.create 1667 in
        Hashtbl.iter (fun old_code contents ->
            Hashtbl.add res (Hashtbl.find htbl old_code) contents)
        data.taxon_characters;
        res
    in
    let root = 
        match data.root_at with
        | None -> None
        | Some code -> 
                try Some (Hashtbl.find htbl code) with
                | _ -> None
    in
    { data with taxon_names = taxon_names; taxon_codes = taxon_codes;
    taxon_characters = taxon_characters; root_at = root }, htbl

let randomize_taxon_codes = change_taxon_codes Array_ops.randomize 

let lexicographic_taxon_codes data = 
    let lexicographic_sort (a : int) b =
        let namea = code_taxon a data
        and nameb = code_taxon b data in
        String.compare namea nameb
    in
    change_taxon_codes (Array.stable_sort ~cmp:lexicographic_sort) data

module Sample = struct
    let characters_to_arr chars =
        Array.of_list (Hashtbl.fold (fun code spec acc ->
            (0, code, spec) :: acc) chars [])

    let bootstrap_spec ?(rand = Random.int) arr m =
        let n = Array.length arr in
        for i = 0 to m do
            let p = rand n in
            let (cnt, code, spec) = arr.(p) in
            arr.(p) <- (cnt + 1, code, spec);
        done;
        arr

    (** [jackknife_spec ar m] resamples [Array.length ar - m] characters without
    * replacement uniformly at random *)
    let jackknife_spec ar m =
        Array_ops.randomize ar;
        let n = Array.length ar in
        assert (n > m);
        for i = 0 to (n - m) - 1 do
            let (_, code, spec) = ar.(i) in
            ar.(i) <- (1, code, spec);
        done;
        ar

    let generate data perturb =
        let arr = characters_to_arr data.character_specs in
        let data = duplicate data in
        let arr =
            match perturb with
            | `Bootstrap -> 
                    bootstrap_spec arr (Array.length arr)
            | `Jackknife m ->
                    jackknife_spec arr m 
        in
        Array.iter (fun (cnt, code, spec) ->
            Hashtbl.replace data.character_specs code 
                    (set_weight_factor (float_of_int cnt) spec)) arr;
        data

end
