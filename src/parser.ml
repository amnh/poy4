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

let () = SadmanOutput.register "Parser" "$Revision: 2026 $"

(* A in-file position specification for error messages. *)
let ndebug = true
let debug_cterm = true
let odebug = Status.user_message Status.Information

(* This module provides smarter reading functionality *)
module R = FileStream.Pervasives

type fl = {
    filename : string;
    taxon : string;
    sequence : string;
    character : string;
    line : int;
}

exception Illegal_molecular_format of fl
exception Illegal_hennig86_format of string
exception Illegal_tree_format of string
exception Illegal_dictionary of string
exception Unsupported_file_format of string
exception Unknown_taxon of string
exception Unexpected of string

type t = 
    | Nucleic_Acids 
    | Proteins
    | Prealigned_Alphabet of Alphabet.a
    | AlphSeq of Alphabet.a
    | Inactive_Character 
    | Ordered_Character of (int * int * bool)
    | Unordered_Character of (int * bool)
    | Sankoff_Character of (int list * bool)
    | Genes of int array

(** [is_unknown t] tells observers whether this present character was listed as
    unknown by the user *)
let is_unknown t = match t with
| Nucleic_Acids
| Proteins
| Prealigned_Alphabet _
| Genes _
| AlphSeq _
| Inactive_Character -> false
| Unordered_Character (_, bool)
| Sankoff_Character (_, bool)
| Ordered_Character (_, _, bool) -> bool

type ft = 
    | Is_Hennig 
    | Is_Clustal
    | Is_Fasta 
    | Is_Poy 
    | Is_Genome 
    | Is_ASN1 
    | Is_Genbank 
    | Is_INSDSeq  
    | Is_GBSeq 
    | Is_TinySeq 
    | Is_XML 
    | Is_Nexus 
    | Is_NewSeq 
    | Is_Dictionary
    | Is_Fixed_States_Dictionary
    | Is_Transformation_Cost_Matrix
    | Is_Phylip
    | Is_Unknown 
    | Is_Trees
    | Is_ComplexTerminals


type taxon = string

type filename = [ `Local of string | `Remote of string ]

let stringify_channel ch = 
    let res = ref "" in
    try 
        while true do
            let line = input_line ch in
            if not ndebug then print_endline "Reading line: ";
            if not ndebug then print_endline line;
            res := !res ^ " " ^ line;
        done;
        ""
    with
    | End_of_file -> 
          if not ndebug then print_endline !res;
          !res


let anywhere_match regex line =
    try
        let _ = Str.search_forward regex line 0 in
        true
    with
    | _ -> false

let poy_file_regex = Str.regexp "\\(define\\|load\\|trees\\|names\\|ignore\\)"

(* Some general utilities for the parser *)

let test_file file = 
    let ch = R.open_in file in
    let line = R.input_line ch in
    let line2 = 
        try R.input_line ch with
        | _ -> ""
    in
    R.close_in ch;
    if anywhere_match (Str.regexp "^CLUSTAL") line then Is_Clustal
    else if anywhere_match (Str.regexp "xread") line then Is_Hennig
    (* treat dpread as a hennig file *)
    else if anywhere_match (Str.regexp "COMPLEX") line then
        Is_ComplexTerminals
    else if anywhere_match (Str.regexp "dpread") line then Is_Hennig
    else if anywhere_match poy_file_regex line then
        Is_Poy
    else if anywhere_match (Str.regexp "^\\([0-9]+\\s*\\)+") line then
        Is_Transformation_Cost_Matrix
    else if anywhere_match (Str.regexp "^>") line then 
        begin
            if anywhere_match (Str.regexp "[a-zA-Z]") line2 then
                Is_Fasta 
            else
                Is_Genome
        end
    else if anywhere_match (Str.regexp "Seq") line then Is_ASN1
    else if anywhere_match (Str.regexp "LOCUS") line then Is_Genbank
    else if anywhere_match (Str.regexp "#NEXUS") line then Is_Nexus
    else if anywhere_match (Str.regexp "<\\?xml") line then
        begin
            if anywhere_match (Str.regexp "<!DOCTYPE INSDSeq") line2 then
                Is_INSDSeq
            else if anywhere_match (Str.regexp "<!DOCTYPE GB") 
            line2 then
                Is_GBSeq
            else if anywhere_match (Str.regexp "<!DOCTYPE TSeq") 
            line2 then
                Is_TinySeq
            else 
                Is_XML
        end
    else if anywhere_match (Str.regexp "^[a-zA-Z0-9_]+") line 
            && anywhere_match (Str.regexp " *[0-9]+") line2 then
                Is_NewSeq
    else if 
        anywhere_match (Str.regexp "^[a-zA-Z._-]+ +[a-zA-Z._-]+\\s*") line
    then Is_Dictionary
    else if anywhere_match (Str.regexp "^[0-9]+ *[0-9]+") line then
        Is_Phylip
    else if anywhere_match (Str.regexp "^ *(") line then
        Is_Trees
    else Is_Unknown

let trim str = 
    Str.global_replace (Str.regexp " +") "" str

let true_trim str = 
    Str.global_replace (Str.regexp "\\(^ +\\)\\|\\( +$\\)") "" str

let _architecture = 32 

module type MOLECULAR = sig
    (** Interface for molecular kind of data parsers *)

    (** [of_channel x y] takes an input channel [y] with information in ascii
    * format of a molecular data of type [x] and outputs the list of sequences 
    * and
    * taxon names contained in the file. If the function finds an unexpected
    * character or an illegal condition for a specific file format, an
    * Illegal_molecular_format or Unsupported_file_format exception is raised. 
    * *)
    val of_channel : t -> in_channel -> 
        (Sequence.s list list list * taxon) list

    (** [to_channel x y z] writes in the output channel [x] the sequences and 
    * taxon list [y] using the alphabet [z] for the sequences in [y]. If the 
    * function finds
    * an illegal element in any of the sequences for the alphabet [z], raises an
    * Alphabet.Illegal_Code exception. There is no guarantee on the state of the
    * output file if the exception is raised. *)
    val to_channel : 
        out_channel -> (Sequence.s * taxon) list -> Alphabet.a -> unit

    val of_file : t -> filename -> 
        (Sequence.s list list list * taxon) list 
end

(** [alphabet_of_t t] returns the alphabet associated with this character
    type *)
let alphabet_of_t t =
    match t with
    | Nucleic_Acids -> Alphabet.nucleotides
    | Proteins -> Alphabet.aminoacids
    | Prealigned_Alphabet a
    | AlphSeq a -> a
    | _ -> failwith "alphabet_of_t"

(* Parser for fasta files *)
module Fasta = struct

    let empty_line = Str.regexp "^[ \t]*$"
    type read = Eof | Eot | Read of string
    let get_line () =
        fun ch ->
        try 
            let line = R.input_line ch in
            if Str.string_match empty_line line 0 then Eot else Read line
        with
        | _ -> Eof

    let is_taxon line = line.[0] = '>'
    exception Finished

    let split_subsequences pattern sequence =
        let divider = Str.regexp pattern in        
        let builder item (prev, acc)  =
            let items = Str.split_delim divider item in 
            let len = List.length items in 
            match items with
            | [h] -> (h :: prev), acc
            | [] -> prev, acc
            | hd :: tl ->
                  let acc, _ = 
                      List.fold_right 
                          (fun item (acc, idx) ->
                               if idx = len - 1 then  
                                   (item::prev)::acc, idx - 1  
                               else if idx = 0 then acc, idx - 1 
                               else ([item]::acc), idx - 1 
                          ) items (acc, len - 1) 
                  in  
                  [hd], acc 
        in  
        let lst =  
            match List.fold_right builder sequence ([], []) with 
            | [], tl -> tl 
            | hd, tl -> hd :: tl  
        in  
        lst

    let split_frag = split_subsequences "#"
    let split_loci = split_subsequences "|"
    let split_chromosomes = split_subsequences "@"

    let doprepend alph seq v = 
        try Sequence.prepend seq (Alphabet.match_base v alph) with
        | Alphabet.Illegal_Character c -> 
                if v <> " " then
                    let fl = 
                        { filename = ""; taxon = ""; sequence = ""; character = c; 
                        line = 0 }
                    in
                    raise (Illegal_molecular_format fl)
                else ()

    let process_sequence remove_gaps lexer alph lst =
        let lst = List.rev lst in
        let gap = Alphabet.get_gap alph in
        let seq, length =
            List.fold_right (fun x (lst, cnt) ->
                    let stream = Stream.of_string x in
                    lexer stream lst cnt) lst ([], 0)
        in
        (* Watch it, the sequence list is in reversed order *)
        let s = Sequence.create (length + 1) in
        List.iter (fun x -> 
            if x <> gap || not remove_gaps then
                Sequence.prepend s x
            else ()) seq;
        Sequence.prepend s gap;
        s

    let process_file_imp remove_gaps ch alph =
        (* Apparenty ocaml is not making the next tail-recursive function a
         * loop? for some reason I get stack overflow, so I will write it using a
         * loop and see how it goes. *)
        let res = ref [] 
        and sequence = ref []
        and lexer = Alphabet.Lexer.make_lexer true alph 
        and taxon = ref "" in
        let get_line = get_line () in
        let last_line_empty = ref true in
        try
            while true do
                match get_line ch with
                | Read line ->
                      if is_taxon line || !last_line_empty then begin
                          try
                              let tmps =  
                                  let sequence = List.rev !sequence in 
                                  let seqs2 = split_chromosomes sequence in
                                  let seqs3 = List.map split_loci seqs2 in
                                  let seqs4 = List.map 
                                      (fun ls2 -> List.map split_frag ls2) seqs3  
                                  in 
                                  let seqs4 = List.map  
                                      (fun s3 ->  
                                           List.map 
                                           (fun s2 -> List.map 
                                           (process_sequence remove_gaps lexer 
                                           alph) s2 ) s3 
                                      ) seqs4  
                                  in                    

                                  seqs4
                              in 
                            let result = tmps, !taxon in
                            res := result :: !res;
                            taxon := line;
                            if (!taxon).[0] = '>'
                            then
                                taxon := String.sub line 1 ((String.length line) - 1);

                            sequence := [];
                            last_line_empty := false;
                          with
                          | Illegal_molecular_format fl ->
                                  let fl = { fl with taxon = line } in
                                  raise (Illegal_molecular_format fl)
                        end else (sequence := line :: !sequence;)
                | Eot -> last_line_empty := true
                | Eof -> 
                        let tmps = 
                            let sequence = List.rev !sequence in
                            let seqs2 = split_chromosomes sequence in  
                            
                            let seqs3 = List.map split_loci seqs2 in 

                            let seqs4 = List.map  
                                (fun ls2 -> List.map split_frag ls2) seqs3   
                            in  

                            let seqs4 = List.map  
                                (fun s3 ->  
                                     List.map (fun s2 -> List.map (process_sequence remove_gaps lexer alph) s2 ) s3 
                                ) seqs4  
                            in           
                            seqs4
                        in
                        let result = tmps, !taxon in
                        res := result :: !res;
                        raise Finished
            done;
            []
        with
        | Finished -> 
                !res


    let of_channel_obj t ch =
        let res = 
            match t with
            | Nucleic_Acids 
            | AlphSeq _ 
            | Proteins -> process_file_imp true ch (alphabet_of_t t)
            | Prealigned_Alphabet _ ->
                    process_file_imp false ch (alphabet_of_t t)
            | _ -> 
                    let msg = 
                        "Unexpected error 01. Contact the AMNH programming team." 
                    in
                    raise (Unsupported_file_format msg)
        in
        List.fold_left (fun acc ((_, b) as item) -> 
            if b <> "" then item :: acc
            else acc) [] res

    let of_channel t ch =
        of_channel_obj t (new FileStream.stream_reader ch)

    let output ch alp (seq, name) = 
        let str = Sequence.to_string seq alp in
        output_string ch (">" ^ name ^ "\n");
        output_string ch (str ^ "\n")

    let to_channel ch l alp =
        List.iter (output ch alp) l

    let of_file t f =
        try 
            let ch = R.open_in f in
            let res = of_channel_obj t ch in
            R.close_in ch;
            res
        with
        | Illegal_molecular_format fl ->
                let f = FileStream.filename f in
                let fl = { fl with filename = f } in
                raise (Illegal_molecular_format fl)
end

module Poy = Fasta
(* Parser for Poy files. Currently the same as a Fasta file. *)


module ASN1 = struct
    let rec read_sequence ch out_seq =
         let line = input_line ch in
         if (String.contains line '}') then 
             begin
                 out_seq := !out_seq ^ line;
                 ()
             end
         else
             begin
                 out_seq := !out_seq ^ line;
                 read_sequence ch out_seq
             end
    
    let rec read_title ch out_seq =
         let line = input_line ch in
         if Str.string_match (Str.regexp 
                " *create-date") line 0 then ()
         else
             begin
                 out_seq := !out_seq ^ line;
                 read_title ch out_seq
             end
 
    let convert_to_fasta file =
        let ch, file = FileStream.channel_n_filename file and
        accession_str = ref "" and
        found_gi_id = ref 0 and
        outName, chout = Filename.open_temp_file "fasta" ".tmp" in
        try
            while true do
                let line = input_line ch in
                if Str.string_match (Str.regexp 
                " *\\(accession+ *\\) \\([a-zA-Z0-9\"]+\\)") line 0 then
                    begin
                        let accession_quotes = Str.matched_group 2 line in
                        accession_str := Str.global_replace (Str.regexp "[\"]+")
                            "" accession_quotes
                    end 
                else if (Str.string_match 
                (Str.regexp " *\\(gi+ *\\) \\([0-9]+\\)") line 0) 
                && !found_gi_id == 0 then
                    begin
                        output_string chout 
                        (">gi|" ^ (Str.matched_group 2 line) ^ "|" 
                        ^ !accession_str ^ "|" );
                        found_gi_id := 1;
                    end
                else if Str.string_match (Str.regexp 
                " *\\(title+\\) \\([a-zA-Z0-9\" ,.]+\\)") line 0 then
                    begin
                        let out_seq = ref "" in
                        out_seq := (Str.matched_group 2 line);
                        read_title ch out_seq;
                        let out_list = Str.split (Str.regexp "[\"]") 
                        !out_seq in 
                        output_string chout ((List.nth out_list 0) ^ "\n");
                    end 
                else if (Str.string_match (Str.regexp " *seq-data") ) line 0 
                    then
                        begin
                            let out_seq = ref "" in
                            read_sequence ch out_seq;
                            let out_list = Str.split (Str.regexp "[']") 
                            !out_seq in 
                            output_string chout (List.nth out_list 1);
                            output_string chout "\n\n";
                        end
                else if (Str.string_match (Str.regexp " *seq {") ) line 0 then
                    found_gi_id := 0
            done;
            open_in outName;
        with
        | End_of_file -> open_in outName 

end

module Genbank = struct
    let convert_to_fasta ?filename file =
        let ch, file = FileStream.channel_n_filename file and
        definition_str = ref "" and
        outName, chout = 
		match filename with
		| None -> Filename.open_temp_file "fasta" ".tmp"
		| Some f -> f, open_out f
	in
        try
            while true do
                let line = input_line ch in
                if Str.string_match (Str.regexp 
                " *\\(VERSION+ *\\) \\([a-zA-Z0-9.]+\\) *GI:\\([0-9]+\\)") 
                line 0 then 
                    output_string chout 
                    (">gi|" ^ (Str.matched_group 3 line) ^ "|" ^
                    (Str.matched_group 2 line) ^ "|" ^ !definition_str)
                else if Str.string_match (Str.regexp 
                " *\\(DEFINITION+ *\\) \\([a-zA-Z0-9 ,]+\\)") line 0 then 
                    definition_str :=  (Str.matched_group 2 line) ^ "\n"
                else
                    if Str.string_match 
                    (Str.regexp " *\\([0-9]+\\) \\([a-zA-Z ]+\\)+") line 0 then
                        let temp = Str.matched_group 2 line in
                        let temp2 = Str.global_replace (Str.regexp "[ ]+")
                        "" temp in
                        output_string chout (String.uppercase temp2) ;
                        output_string chout "\n";
            done;
            open_in outName;
        with
        | End_of_file -> (open_in outName) 

end

module INSDSeq = struct
    let convert_to_fasta file =
        let ch, file  = FileStream.channel_n_filename file and
        definition_str = ref "" and
        accession_str = ref "" and
        outName, chout = Filename.open_temp_file "fasta" ".tmp" in
        try
            while true do
                let line = input_line ch in
                if Str.string_match (Str.regexp 
                " *\\(<INSDSeq_accession-version>+\\)\\([a-zA-Z0-9. ,]+\\)") 
                line 0 then
                    accession_str := (Str.matched_group 2 line) ^ "|"  
                    ^ !definition_str
                else if Str.string_match (Str.regexp 
                " *\\(<INSDSeqid>gi|+\\)\\([a-zA-Z0-9 ,]+\\)") line 0 then
                    output_string chout (">gi|" ^ (Str.matched_group 2 line) ^ 
                    "|" ^ !accession_str)
                else if Str.string_match (Str.regexp 
                " *\\(<INSDSeq_definition>+\\)\\([a-zA-Z0-9 ,]+\\)") line 0 then
                    definition_str := (Str.matched_group 2 line) ^ "\n"
                else
                if Str.string_match (Str.regexp 
                " *\\(<INSDSeq_sequence>+\\)\\([a-zA-Z]+\\)") line 0 then
                    output_string chout ( (Str.matched_group 2 line) ^ "\n\n");
            done;
            open_in outName;
        with
        | End_of_file -> (open_in outName) 

end

module GBSeq = struct
    let convert_to_fasta file =
        let ch, file = FileStream.channel_n_filename file and
        definition_str = ref "" and
        accession_str = ref "" and
        outName, chout = Filename.open_temp_file "fasta" ".tmp" in
        try
            while true do
                let line = input_line ch in
                if Str.string_match (Str.regexp 
                " *\\(<GBSeq_accession-version>+\\)\\([a-zA-Z0-9. ,]+\\)") 
                line 0 then
                    accession_str := 
                    ((Str.matched_group 2 line) ^ "|"  ^ !definition_str)
                else if Str.string_match (Str.regexp 
                " *\\(<GBSeqid>gi|+\\)\\([a-zA-Z0-9 ,]+\\)") line 0 then
                    output_string chout (">gi|" ^ (Str.matched_group 2 line) ^ 
                    "|" ^ !accession_str)
                else if Str.string_match (Str.regexp 
                " *\\(<GBSeq_definition>+\\)\\([a-zA-Z0-9 ,]+\\)") line 0 then
                    definition_str := (Str.matched_group 2 line) ^ "\n"
                else if Str.string_match (Str.regexp 
                " *\\(<GBSeq_sequence>+\\)\\([a-zA-Z]+\\)") line 0 then
                    output_string chout ( (Str.matched_group 2 line) ^ "\n\n");
            done;
            open_in outName;
        with
        | End_of_file -> (open_in outName) 
end

module TinySeq = struct
    let convert_to_fasta file =
        let ch, file = FileStream.channel_n_filename file and
        outName, chout = Filename.open_temp_file "fasta" ".tmp" in
        try
            while true do
                let line = input_line ch in
                if Str.string_match (Str.regexp 
                " *\\(<TSeq_gi>+\\)\\([a-zA-Z0-9. ,]+\\)") line 0 then
                    output_string chout 
                    (">gi|" ^ (Str.matched_group 2 line) ^ "|" )
                else if Str.string_match (Str.regexp 
                " *\\(<TSeq_accver>+\\)\\([a-zA-Z0-9. ,]+\\)") line 0 then
                    output_string chout ( (Str.matched_group 2 line) ^ "|") 
                else if Str.string_match (Str.regexp 
                " *\\(<TSeq_defline>+\\)\\([a-zA-Z0-9 ,]+\\)") line 0 then
                    output_string chout ((Str.matched_group 2 line) ^ "\n")
                else if Str.string_match (Str.regexp 
                " *\\(<TSeq_sequence>+\\)\\([a-zA-Z]+\\)") line 0 then
                    output_string chout ( (Str.matched_group 2 line) ^ "\n\n");
            done;
            open_in outName;
        with
        | End_of_file -> (open_in outName) 

end

module XML = struct
    let convert_to_fasta file =
        let ch, file = FileStream.channel_n_filename file and
        accession_str = ref "" and
        found_gi_id = ref 0 and
        outName, chout = Filename.open_temp_file "fasta" ".tmp" in
        try
            while true do
                let line = input_line ch in
                if Str.string_match (Str.regexp 
                " *\\(<Textseq-id_accession>+\\)\\([a-zA-Z0-9 ,]+\\)")
                line 0 then
                    accession_str :=  (Str.matched_group 2 line) ^ "." 
                else if Str.string_match (Str.regexp 
                " *\\(<Textseq-id_version>+\\)\\([0-9]+\\)")
                line 0 then
                    accession_str := !accession_str ^
                     (Str.matched_group 2 line) ^ "|" 
                else if (Str.string_match (Str.regexp 
                " *\\(<Seq-id_gi>+\\)\\([0-9]+\\)")
                line 0) && !found_gi_id == 0 then
                    begin
                        output_string chout 
                        ( ">gi|" ^ (Str.matched_group 2 line) ^ "|" ^ 
                        !accession_str);
                        found_gi_id := 1;
                    end
                else if Str.string_match (Str.regexp 
                " *\\(<Seqdesc_title>+\\)\\([a-zA-Z0-9 ,]+\\)") line 0 then
                    output_string chout (Str.matched_group 2 line) 
                else if Str.string_match (Str.regexp 
                " *\\(<IUPACna>+\\)\\([a-zA-Z]+\\)") line 0 then
                    begin
                        output_string chout 
                        ( "\n" ^ (Str.matched_group 2 line) ^ "\n\n");
                    end
                else if Str.string_match (Str.regexp 
                " *<Seq-entry>") line 0 then
                        found_gi_id := 0;
            done;
            open_in outName;
        with
        | End_of_file -> (open_in outName) 

end
        
        
module Nexus = struct
    let rec read_header ch =
         let line = input_line ch in
         if Str.string_match (Str.regexp " *MATRIX")
                line 0 then ()
         else
             read_header ch
   
    let print_taxon_seq ch key value =
        output_string ch (key ^ "\n");
        output_string ch (value ^ "\n")

    let get_number_taxa ch =
         let line = String.uppercase (input_line ch) in
         if Str.string_match
         (Str.regexp " *\\(DIMENSIONS+\\) \\(NTAX=\\)\\([0-9]+\\)") line 0 then
             Str.matched_group 3 line
         else ""
    
    let get_number_char ch =
         let line = String.uppercase (input_line ch) in
         if Str.string_match
         (Str.regexp " *\\(DIMENSIONS+\\) \\(NCHAR=\\)\\([0-9]+\\)") line 0 then
             Str.matched_group 3 line
         else ""


    let rec read_aligned ch htable =
         let line = input_line ch in
         if Str.string_match 
         (Str.regexp " *\\([a-zA-Z0-9_.]+\\) \\([a-zA-Z0-9; ]+\\)") line 0 then
             begin
                 let key = Str.matched_group 1 line 
                 and temp_value = Str.matched_group 2 line in
                 let value = Str.global_replace (Str.regexp "[,; ]+")
                            "" temp_value in 
                 if (Hashtbl.mem htable key) then 
                     begin
                         let temp_seq = Hashtbl.find htable key in
                         Hashtbl.replace htable key (temp_seq ^ value);
                     end
                 else
                     Hashtbl.add htable key value;
                 if String.contains value ';' then ()
                 else read_aligned ch htable;
             end
         else if String.contains line ';' then ()
         else read_aligned ch htable
         

    let rec read_sequence ch out_seq =
         let line = input_line ch in
         if (String.contains line ',') then 
             begin
                 out_seq := !out_seq ^ line;
                 ()
             end
         else
             begin
                 out_seq := !out_seq ^ line;
                 read_sequence ch out_seq
             end
    
    let rec read_unaligned ch outseq =
         let line = input_line ch in
         if Str.string_match 
         (Str.regexp " *\\([a-zA-Z0-9_]+\\) \\([a-zA-Z0-9,; ]+\\)") line 0 then
             begin
                 outseq := !outseq ^ (">" ^ (Str.matched_group 1 line) ^ "\n");
                 outseq := !outseq ^ (Str.matched_group 2 line);
                 if String.contains !outseq ';' then ()
                 else
                     begin
                         if String.contains !outseq ',' then ()
                         else read_sequence ch outseq;
                         outseq := !outseq ^ "\n\n";
                         read_unaligned ch outseq;
                     end
             end

    let rec read_tree ch out_tree =
         let line = input_line ch in
         if (String.contains line ';') then 
             begin
                 out_tree := trim (!out_tree ^ line);
                 ()
             end
         else
             begin
                 out_tree := !out_tree ^ line;
                 read_tree ch out_tree
             end
             
    let read_nexus file =
        let ch, file = FileStream.channel_n_filename file and
        num_taxa = ref "" and
        num_char = ref "" in
        try
            while true do
                let line = String.uppercase (input_line ch) in
                if Str.string_match (Str.regexp " *\\(BEGIN TREES;+\\)")
                line 0 then
                    begin
                        let out_seq = ref "" in
                        read_tree ch out_seq;
                        let out_list = Str.split (Str.regexp "[=;]") 
                            !out_seq in
                        let tree_with_commas = List.nth out_list 1 in
                        let tree = Str.global_replace (Str.regexp "[,]+")
                            " " tree_with_commas in
                        print_endline tree 
                    end
                else if Str.string_match 
                (Str.regexp " *\\(BEGIN UNALIGNED;+\\)")line 0 then
                    begin
                        let out_seq = ref "" in
                        read_header ch;
                        read_unaligned ch out_seq;
                        out_seq :=  Str.global_replace (Str.regexp "[,; ]+")
                            "" !out_seq; 
                        let outName, chout = Filename.open_temp_file "fasta" ".tmp" in
                        output_string chout !out_seq;
                        
                    end
                else if Str.string_match 
                (Str.regexp " *\\(BEGIN TAXA;+\\)")line 0 then
                    num_taxa := get_number_taxa ch
                else if Str.string_match 
                (Str.regexp " *\\(BEGIN CHARACTERS;+\\)")line 0 then
                    begin
                        num_char := get_number_char ch;
                        read_header ch;
                        let htable = Hashtbl.create (int_of_string !num_taxa) in
                        read_aligned ch htable;
                        let outName, chout = Filename.open_temp_file "temp"
                        ".hen" in
                        output_string chout "xread\n";
                        output_string chout !num_char;
                        output_string chout (" " ^ !num_taxa ^ "\n");
                        Hashtbl.iter (print_taxon_seq chout) htable;
                        output_string chout ";";
                    end 
             done;
        with
        | End_of_file -> () 

end
        

module NewSeq = struct

    let to_fasta file =
        let ch, file = FileStream.channel_n_filename file and
        start_string = ref ">" and
        outName, chout = Filename.open_temp_file "fasta" ".tmp" in
        try
            while true do
                let line = input_line ch in
                if Str.string_match (Str.regexp 
                "\\(^[^ ]+\\)") line 0 then 
                    output_string chout 
                    (!start_string ^ (Str.matched_group 1 line) ^ "\n")
                else
                    if Str.string_match 
                    (Str.regexp " *\\([0-9]+\\) *\\([a-zA-Z ]+\\)+") line 0 then
                        let temp = Str.matched_group 2 line in
                        let temp2 = Str.global_replace (Str.regexp "[ ]+")
                        "" temp in
                        output_string chout ((String.uppercase temp2) ^ "\n");
                        start_string := "\n>"
            done;
            close_out chout;
            outName
        with
        | End_of_file -> 
                close_out chout;
                outName

    let convert_to_fasta file =
        let file = to_fasta file in
        open_in file

end

module Phylip = struct
    let convert_to_hennig file =
        let ch, file = FileStream.channel_n_filename file and
        outName, chout = Filename.open_temp_file "temp" ".hen" and
        index = ref 0 and
        num_taxa = ref "" and
        num_char = ref "" in
        let line = input_line ch in
        if Str.string_match (Str.regexp 
        "\\(^[0-9]+\\) *\\([0-9]+\\)") line 0 then 
            begin
                num_taxa := (Str.matched_group 1 line); 
                num_char := (Str.matched_group 2 line);
            end;
        print_endline !num_char;
        print_endline !num_taxa;
        let num_taxa_int = int_of_string !num_taxa in
        let sequence_array = Array.make num_taxa_int "" and
        taxa_array = Array.make num_taxa_int "" in
        try
            while true do
                let line = input_line ch in
                if Str.string_match 
                (Str.regexp " *\\([a-zA-Z]+\\) +\\([a-zA-Z ]+\\)+") 
                line 0 then
                    begin
                        taxa_array.(!index) <- (Str.matched_group 1 line);
                        sequence_array.(!index) <- (Str.matched_group 2 line);
                        index := (!index + 1) mod num_taxa_int;
                    end
                else if Str.string_match (Str.regexp " *\\([a-ZA-Z]+\\)") 
                line 0 then
                    begin
                        let temp_string = sequence_array.(!index) in
                        sequence_array.(!index) <- 
                            temp_string ^ (Str.matched_group 1 line);
                            index := (!index + 1) mod num_taxa_int;
                    end
                else if Str.string_match (Str.regexp " +") line 0 then
                    index := 0;
            done;
            open_in outName;
        with
        | End_of_file -> 
                output_string chout "xread\n";
                output_string chout (!num_taxa ^ " " ^ !num_char ^ "\n");
                for i = 0 to num_taxa_int - 1 do
                    output_string chout (taxa_array.(i) ^ "\n");
                    output_string chout (sequence_array.(i) ^ "\n");
                done;  
                output_string chout ";";
                (open_in outName) 

      (*  output_string chout "xread\n";
        output_string chout !num_char;
        output_string chout (" " ^ !num_taxa ^ "\n"); *)
 
    
end

module Tree = struct
    (* Parser for trees in (a (b c)) format *)

    type 'a t = Leaf of 'a | Node of 'a t list * 'a
    (* A simple representation of a tree *)

    exception Trailing_characters

    let rec map fn = function
        | Leaf d -> Leaf (fn d)
        | Node (list, d) ->
              Node (List.map (map fn) list, fn d)

    let gen_aux_of_stream_gen do_stream stream =
        let taxon_name x = not (FileStream.is_taxon_delimiter x) in
        let close_squared_parenthesis = [ ']' ] in
        let ignore_cost_bracket () =
            ignore (stream#read_excl close_squared_parenthesis);
            ignore (stream#getch);
            ()
        in
        let get_cost_bracket () =
            let res = stream#read_excl close_squared_parenthesis in
            ignore (stream#getch);
            res
        in
        let read_taxon_name () = 
            stream#read_while taxon_name
        in
        let rec read_branch acc =
            stream#skip_ws_nl;
            match stream#getch with
            | '(' -> 
                    let res = read_branch [] in
                    read_branch (res :: acc)
            | ')' -> 
                    Node (acc, "")
            | '[' -> 
                    ignore_cost_bracket ();
                    read_branch acc
            | v when taxon_name v ->
                    stream#putback v;
                    let taxon = read_taxon_name () in
                    read_branch ((Leaf taxon) :: acc)
            | v ->
                    let character = stream#get_position in
                    let ch = Char.escaped v in
                    if List.exists (fun x -> x = ch) [","; ";"] then
                        read_branch acc
                    else
                        let _ = 
                            let msg = 
                                "I@ will@ use@ the@ character@ " ^ ch ^ 
                                " in@ position@ " ^ string_of_int character ^ 
                                "@ as@ a@ taxon@ name@ separator" 
                            in
                            Status.user_message Status.Information msg;
                        in
                        read_branch acc
        in
        let rec read_tree acc1 acc2 =
            try
                stream#skip_ws_nl;
                match stream#getch with
                | '(' -> 
                        let res = 
                            try read_branch [] with
                            | End_of_file -> 
                                    let msg = "Unexpected end of file" in
                                    raise (Illegal_tree_format msg)
                        in
                        read_tree acc1 ((res, "") :: acc2)
                | '*'
                | ';' -> 
                        let acc1 = acc2 :: acc1 in
                        (try read_tree acc1 [] with
                        | End_of_file -> acc1)
                | '[' -> 
                        let contents = 
                            try get_cost_bracket () with
                            | End_of_file ->
                                    let msg = "Unexpected end of file" in
                                    raise (Illegal_tree_format msg)
                        in
                        let acc2 = 
                            match acc2 with
                            | (h, _) :: t -> (h, contents) :: t
                            | [] -> 
                                    let msg = "Unexpected cost spec" in
                                    raise (Illegal_tree_format msg)
                        in
                        read_tree acc1 acc2
                | v -> 
                        let character = stream#get_position in
                        let ch = Char.escaped v in
                        let message = "Unexpected character " ^ ch ^ 
                        " in position " ^ string_of_int character in
                        failwith message
            with
            | End_of_file -> 
                    match acc2 with
                    | [] -> acc1
                    | _ -> acc2 :: acc1
        in
        let read_tree_str =
            let acc2 = ref None in
            let rec tree_generator () =
                stream#skip_ws_nl;
                match stream#getch with
                | '(' -> 
                        let res = 
                            try read_branch [] with
                            | End_of_file -> 
                                    let msg = "Unexpected end of file" in
                                    raise (Illegal_tree_format msg)
                        in
                        (match !acc2 with
                        | None -> 
                                acc2 := Some (res, "");
                                tree_generator ()
                        | Some _ -> raise (Illegal_tree_format "Tree ignored!"))
                | '*'
                | ';' -> 
                        (match !acc2 with
                        | None -> raise (Illegal_tree_format "No trees to
                        read?")
                        | Some tree ->
                                acc2 := None;
                                tree)
                | '[' -> 
                        let contents = 
                            try get_cost_bracket () with
                            | End_of_file ->
                                    let msg = "Unexpected end of file" in
                                    raise (Illegal_tree_format msg)
                        in
                        (match !acc2 with
                        | Some (h, _) -> 
                                acc2 := None;
                                (h, contents) 
                        | None -> 
                                let msg = "Unexpected cost spec" in
                                raise (Illegal_tree_format msg))
                | v -> 
                        let character = stream#get_position in
                        let ch = Char.escaped v in
                        let message = "Unexpected character " ^ ch ^ 
                        " in position " ^ string_of_int character in
                        failwith message
            in
            tree_generator
        in
        if not do_stream then `Trees (read_tree [] [])
        else `Stream (read_tree_str)

    let gen_aux_of_stream str = 
        match gen_aux_of_stream_gen false str with
        | `Trees t -> t
        | `Stream _ -> assert false

    let aux_of_stream stream =
        let trees = gen_aux_of_stream stream in
        List.map (List.map (fun (a, _) -> a)) trees

    let aux_of_string str =
        let str = Str.global_replace (Str.regexp "\\[[^]]*\\]") "" str in
        let res = Str.full_split (Str.regexp "[(), ]") str in
        let process = function
            | Str.Delim a -> Str.Delim (trim a)
            | a -> a
        in
        let res = List.map (process) res in
        let builder ((state, readed) as next) item =
            match item, state with 
            | Str.Delim "(", _ -> (readed :: state), []
            | Str.Delim ")", hd :: tl -> tl, (Node (readed, "")) :: hd
            | Str.Delim ";", _ | Str.Delim "", _ | Str.Delim " ", _  -> next
            | Str.Delim str, _ when str.[0] = '[' -> next
            | Str.Text x, _ -> (state, (Leaf x) :: readed)
            | Str.Delim ")", [] -> raise (Illegal_tree_format "Unexpected )")
            | Str.Delim x, _ -> 
                    raise (Illegal_tree_format ("Unexpected character." ^ x))
        in
        let _, res = List.fold_left (builder) ([], []) res in
        res


    let gen_of_string f str = 
        try
            let stream = new FileStream.string_reader str in
            f stream
        with
        | Trailing_characters -> 
                raise (Illegal_tree_format "Trailing characters in tree.")

    let of_string str = gen_of_string aux_of_stream str

    let gen_of_channel f ch =
        try
            let stream = new FileStream.stream_reader ch in
            f stream;
        with
        | End_of_file -> failwith "Unexpected end of file"

    let of_channel ch = gen_of_channel aux_of_stream ch

    let gen_of_file f file =
        try
            let ch = FileStream.open_in file in
            f ch
        with
        | Failure msg ->
                let file = FileStream.filename file in
                let msg = file ^ ": " ^ msg in
                raise (Illegal_tree_format msg)
        | (Sys_error err) as e ->
                let file = FileStream.filename file in
                let msg = "Couldn't@ open@ the@ trees@ file@ " ^ file ^ 
                ".@ The@ error@ message@ is@ @[" ^ err ^ "@]"in
                Status.user_message Status.Error msg;
                raise e

    let stream_of_file file =
        let ch = new FileStream.stream_reader (FileStream.open_in file) in
        match gen_aux_of_stream_gen true ch with
        | `Stream s -> s
        | `Trees _ -> assert false

    let of_file file = gen_of_file of_channel file

    let of_file_annotated = gen_of_file (gen_of_channel gen_aux_of_stream) 

    let of_channel_annotated = gen_of_channel gen_aux_of_stream 

    let of_string_annotated = 
        gen_of_string gen_aux_of_stream 

    let cannonic_order tree =
        let rec build_cannonic_order = function
            | Leaf d -> (Leaf d, d)
            | Node (chld , cnt) ->
                    let res = List.map build_cannonic_order chld in
                    let nch = List.sort (fun (_, a) (_, b) -> compare a b) res in
                    let _, b = List.hd nch in
                    let nch = List.map (fun (x, _) -> x) nch in
                    Node (nch, cnt), b
        in
        let tree, _ = build_cannonic_order tree in
        tree

end

(** [lor_list_withhash l hash] returns the logical or of the hash values of all
    of the values in [l] *)
let lor_list_withhash l hash =
    let process f =
        let proc x y = x lor (f y) in
        List.fold_left proc 0 l
    in
    match hash with
    | Some hash -> process (fun x -> Hashtbl.find hash x)
    | None -> process (fun x -> x)

module Hennig = struct
    (* A hennig86 file parser *)

    (* The default range for an ordered type *)
    let default_ordered = ( 9999, -10 )

    type options = 
        | Weight of (int * int) 
        | Deactivate of int 
        | Activate of int
        | Ordered of int 
        | Unordered of int
        | Sankoff of (int array array * int)
        | Tree of (string Tree.t) list
        | Unkown_option of string

    (* Set of regexes to match all the supported commands in a Hennig86 file *)
    let tree_re = Str.regexp "tread +\\((.*)\\)"

    (* The general idea of ccost is:
       ccode ((costspec)+(charspec)+)+; *)
    let multi_cc = Str.regexp
        "cc[a-z]* *\\([^a-z].*\\)"
    let costs = Str.regexp 
        "cos[a-z]* *\\[ *\\([0-9.]+\\) *\\$ *\\([0-9]+\\) *\\([0-9 ]+\\)"

    (** [build_list_between_intervals lower higher []] builds a list of
        integers from [lower] to [higher], inclusive *)
    let rec build_list_between_intervals lower higher thelist =
        if higher < lower then thelist
        else build_list_between_intervals lower (higher-1) (higher::thelist)

    let build_list_from_option str characters =
        if str = "." then (* All the values should be changed *)
            build_list_between_intervals 0 (characters - 1) []
        else if String.contains str '.' then begin (*range of values *) 
            let temp = Str.split (Str.regexp "\\.") str in
            let lower = int_of_string (List.nth temp 0) 
            and higher = int_of_string (List.nth temp 1) in
            build_list_between_intervals lower higher []
        end else begin
            let res = Str.split (Str.regexp " +") str in
            List.map (int_of_string) res 
        end
 
    let rec process_an_option str characters res =
        let res = build_list_from_option str characters in
        if List.for_all (fun x -> x < characters && (-1) < x) res then res
        else 
            raise (Illegal_hennig86_format ("Wrong character index in " ^ str))
        
    let match_num = 
        Str.regexp " *\\([1-90\\-]+\\) *\\(.*\\)"

    (* used for dpread files to read in the cost matrix - read the string
    *  and store as a list of integers *)   
    let rec load_all_integers str l =
        match Str.string_match match_num str 0 with
        | true ->
                let v = Str.matched_group 1 str
                and r = Str.matched_group 2 str in
                let v = Pervasives.int_of_string v in
                load_all_integers r (v :: l);
        | false ->
                List.rev l 

    (* used for dpread files this function takes the list of integers from
    *  load_all_integers function and converts to a matrix (cost matrix) *)
    let convert_list_to_matrix size lst =
        let matrix = Array.make_matrix size size 0 in
        for i = 0 to size - 1 do 
            for j = 0 to size - 1 do
                matrix.(i).(j) <- List.nth lst (size + i*size + j);
            done;
        done;
        matrix

    let process_ccode string n_chars =
        let r = new FileStream.string_reader string in
        let read_num def =
            try
                let str = r#read_while FileStream.is_num in
                if str = ""
                then def
                else int_of_string str
            with End_of_file -> def in
        let rec read_verb ~act ~add ~w ~acc =
            match r#getch_safe with
            | None -> List.rev acc
            | Some ch -> match ch with
              | '[' -> read_verb ~act:`Active ~add ~w ~acc
              | ']' -> read_verb ~act:`Inactive ~add ~w ~acc
              | '+' -> read_verb ~act ~add:`Additive ~w ~acc
              | '-' -> read_verb ~act ~add:`Nonadditive ~w ~acc
              | '*' -> read_verb ~act:`None ~add:`None ~w:`None ~acc
              | '/' ->
                    (* see whether we can read an integer weight argument *)
                    (* if not, default to 1... *)
                    let weight =
                        try r#skip_ws_nl; r#read_int
                        with _ -> 1 in
                    read_verb ~act ~add ~w:(`Set weight) ~acc
              | ' ' | '\t' | '\010' | '\013' ->
                    read_verb ~act ~add ~w ~acc
              | c -> begin
                    r#putback c;
                    read_object ~act ~add ~w ~acc
                end
        and read_object ~act ~add ~w ~acc =
            (* this can be [int].[int], where the first int defaults to 0, and
               the last defaults to n_chars - 1.
               to complicate things further, we should ignore opening and
               closing parentheses... *)
            let acc = try
                r#skip_ws_nl;
                while r#match_prefix "(" do r#skip_ws_nl done;
                let nfrom = read_num 0 in
                let nto =
                    if r#match_prefix "."
                    then read_num (n_chars - 1)
                    else nfrom in
                let chars = build_list_between_intervals nfrom nto [] in
                (* add all the properties *)
                let acc = match act with
                | `None -> acc
                | `Active -> 
                        List.fold_left
                      (fun acc i -> (Activate i) :: acc) acc chars
                | `Inactive -> 
                        List.fold_left
                      (fun acc i -> (Deactivate i) :: acc) acc chars in
                let acc = match add with
                | `None -> acc
                | `Additive -> 
                        List.fold_left
                      (fun acc i -> (Ordered i) :: acc) acc chars
                | `Nonadditive -> 
                        List.fold_left
                      (fun acc i -> (Unordered i) :: acc) acc chars in
                let acc = match w with
                | `None -> acc
                | `Set w -> List.fold_left
                      (fun acc i -> (Weight (w, i)) :: acc) acc chars in
                (* skip closing paren *)
                (try ignore (r#read_while (FileStream.is_or [FileStream.is_ws_nl;
                                                          FileStream.is_char ')']))
                 with End_of_file -> ());
                acc
            with End_of_file -> acc in
            read_verb ~act ~add ~w ~acc
        in read_verb ~act:`None ~add:`None ~w:`None ~acc:[]
                
   
    let process_single_command taxa_data x characters =
        try
            if Str.string_match multi_cc x 0 then begin
                let res = Str.matched_group 1 x in
                let res = process_ccode res characters in
                res
            end
            else if Str.string_match costs x 0 then begin
                let res = Str.matched_group 1 x in
                let size = int_of_string (Str.matched_group 2 x) in
                let matrix_string = Str.matched_group 3 x in
                if not ndebug then print_endline matrix_string;
                let matrix_list = load_all_integers matrix_string [] in
                let matrix = convert_list_to_matrix size matrix_list in 
                let res = process_an_option res characters [] in 
                List.map (fun x -> Sankoff (matrix, x)) res
            end
            else if Str.string_match tree_re x 0 then begin
                let tree = Str.matched_group 1 x in
                let trees = Tree.of_string tree in
                (* convert them to taxa *)
                try
                    let m t = Tree.map
                        (fun str ->
                             let name =
                                 try
                                     let i = int_of_string str in
                                     let (name, _) = List.nth taxa_data i in
                                     name
                                 with _ -> str in
                             name)
                        t in
                    let trees = List.map (List.map m) trees in
                    List.map (fun t -> Tree t) trees
                with _ -> []
            end else [Unkown_option x]
        with
        | _ -> 
                let msg = "Illegal command in Hennig86 file. " ^ x in
                raise (Illegal_hennig86_format msg)

    let process_options taxa_data opts y = 
        let single_option_processor x =
            match process_single_command taxa_data x y with
            | [Unkown_option str] as res -> 
                    Status.user_message Status.Error ("@[Parser: Unknown Hennig86 \
                    command:@ @[" ^ str ^ "@]@ Ignoring@]");
                    res
            | res -> 
                    res
        in
        let my_filter = function | [Unkown_option _] -> false | _ -> true in
        let res = List.map (single_option_processor) opts in
        let res = List.filter (my_filter) res in
        List.flatten res

    (* returns int list list *)
    let read_data_xread r n_chars =
        let rec line acc n_reading =
            if n_reading <= n_chars
            then match r#getch with
            | ' '
            | '\n'
            | '.' -> line acc n_reading (* ignore these *)
            | '[' -> multi acc [] n_reading
            | '?'
            | '-' -> line ([-1] :: acc) (succ n_reading)
(*             | '0' .. '9' as c -> *)
            | c ->                      (* not only number chars *)
                  line ([(Char.code c) - (Char.code '0')] :: acc)
                      (succ n_reading)
(*             | c -> raise *)
(*                   (Illegal_hennig86_format *)
(*                        ("Illegal character in data segment of xread")) *)
            else
                List.rev acc
        and multi std_acc multi_acc n_reading =
            match r#getch with
            | ' '
            | '\n'
            | '.' -> multi std_acc multi_acc n_reading (* ignore these *)
(*             | '0' .. '9' as c -> *)
            | ']' -> line (List.rev multi_acc :: std_acc) (succ n_reading)
(*             | c -> raise *)
(*                   (Illegal_hennig86_format *)
(*                        ("Illegal character in data segment of xread")) *)
            | c ->                      (* not only number chars *)
                  multi std_acc
                      (((Char.code c) - (Char.code '0')) :: multi_acc)
                      n_reading
        in line [] 1

    let read_data_dpread r n_chars =
        let clear fn n_reading acc str =
            match str with
            | "" -> fn n_reading acc ""
            | str ->
                  let int =
                      try (int_of_string str)
                      with _ -> raise (Illegal_hennig86_format
                                           ("Not an integer in dpread: "
                                            ^ str)) in
                  fn (succ n_reading) ([int] :: acc) ""
        in
        let clear' fn n_reading acc str =
            match str with
            | "" -> fn n_reading acc ""
            | str ->
                  let int =
                      try (int_of_string str)
                      with _ -> raise (Illegal_hennig86_format
                                           ("Not an integer in dpread: "
                                            ^ str)) in
                  fn (succ n_reading) (int :: acc) ""
        in
        let rec line n_reading acc str =
            if n_reading <= n_chars
            then match r#getch with
            | ' '
            | '\n'
            | '.' -> clear line n_reading acc str
            | '[' -> clear
                  (fun n_reading acc str -> multi n_reading acc 0 [] str)
                        n_reading acc str
            | '-'
            | '?' -> clear
                  (fun n_reading acc str ->
                       line (succ n_reading) ([-1] :: acc) "")
                        n_reading acc str
            | c -> line n_reading acc (str ^ Char.escaped c)
            else List.rev acc
        and multi r_n r_acc m_n m_acc str =
            match r#getch with
            | ' '
            | '\n'
            | '.' -> clear' (multi r_n r_acc) m_n m_acc str
            | ']' -> clear'
                  (fun m_n m_acc str ->
                       line (succ r_n) ((List.rev m_acc) :: r_acc) str)
                        m_n m_acc str
            | c -> multi r_n r_acc m_n m_acc (str ^ Char.escaped c)
        in line 1 [] ""

    let rec read_taxa ?(acc=[]) r is_dpread taxa chars =
        r#skip_ws_nl;
        (* if we're done, check for correctness *)
        if r#match_prefix ";"
        then begin
            if taxa = List.length acc then begin
                if not ndebug then print_endline "Read all taxa";
                List.rev acc
            end
            else raise (Illegal_hennig86_format "Number of taxa in data matrix\
 doesn't match reported number in heading of Hennig86 file")
        end
        else begin
            (* read the name *)
            let name = r#read_while (FileStream.is_or
                                         [FileStream.is_alpha; FileStream.is_num;
                                          FileStream.is_char '_';
                                          FileStream.is_char '-';
                                          FileStream.is_char '.';
                                          FileStream.is_char '"';
                                         ]) in
            if not ndebug then print_endline ("Name: " ^ name);
            r#skip_ws_nl;
            let data : int list list =
                if is_dpread
                then read_data_dpread r chars
                else read_data_xread r chars in
            let acc = (name, data) :: acc in
            read_taxa ~acc r is_dpread taxa chars
        end

    let rec extract_options ?(acc=[]) r =
        let maybe_cons s acc =
            if s = ""
            then acc
            else s :: acc in
        let rec rs str =
            let c =
                try Some r#getch
                with End_of_file -> None in
            match c with
            | Some ';' -> str, true
            | Some '\010'
            | Some '\013' -> rs (str ^ " ")
            | Some c -> rs (str ^ Char.escaped c)
            | None -> str, false
        in
        let option, more =
            try r#skip_ws_nl; rs ""
            with End_of_file -> "", false in
        if not ndebug then print_endline ("Read option " ^ option);
        if more
        then extract_options ~acc:(maybe_cons option acc) r
        else List.rev (maybe_cons option acc)

    (* Parses the dataset from a Hennig86 file. This is the basic parsing
    * procedure of the data contents of the file *)
    let parse_file characters taxa line is_dpread = 
        let taxa_data = read_taxa line is_dpread taxa characters in
        let names, data = List.split taxa_data in
        let opts = extract_options line in
        let options = process_options taxa_data opts characters in
        let trees, options = List.partition
            (function Tree _ -> true | _ -> false) options in
        let trees = List.map (function Tree a -> a | _ -> assert false) trees in
        names, data, options, trees

    type ordtype = Is_ordered | Is_unordered | Is_sankoff

    let print_ordtype x =
        match x with
        | Is_ordered -> print_string "Ordered"
        | Is_unordered -> print_string "Unordered"
        | Is_sankoff -> print_string "Sankoff"
        
    type mapping_type = 
        | Do_Nothing 
        | Do_Ordered of (int, int) Hashtbl.t 
        | Do_Unordered of (int, int) Hashtbl.t
        | Do_Sankoff 
        (* Each Hashtbl is a mapping of observed numbers in the file and 
        * integer codes assigned by POY for internal representation *)

    type encoding_spec = 
        { (* Encoding specifications for a given character *)
            max : int;
            min : int;
            set : All_sets.Integers.t;
            weight : int;
            active : bool;
            ordered : ordtype;
            cost_matrix : int array array;
            observed_used : mapping_type;
            used_observed : mapping_type;
        }

    let dna_encoding = 
        let codes = 
            [(1, 1); (2, 2); (4, 3); (8, 4) ; (16, 5)]
        in
        let dna_set = 
            List.fold_left (fun x (y, _) -> 
                All_sets.Integers.add y x) 
            All_sets.Integers.empty codes
        and used_observed = 
            let htb = Hashtbl.create 5 in
            List.iter (fun (a, b) -> Hashtbl.add htb a b) codes;
            htb
        in
        {
            max = 16;
            min = 1;
            set = dna_set;
            weight = 1;
            active = true;
            ordered = Is_unordered;
            cost_matrix = [||];
            observed_used = Do_Nothing;
            used_observed = Do_Unordered used_observed;
        }

    (* A taxon name code counter and an association list for taxa names and
    * codes *)
    let counter_names = ref 0 
    let names = ref []


    (* A character counter and an association list for characters and codes *)
    let counter_characters = ref 0 
    let characters = ref [] 

    (* A homologous character counter and association list as in the previous *)
    let counter_characters_hom = ref 0
    let characters_hom = ref []

    let clear_characters () =
        counter_characters_hom := 0;
        counter_characters := 0;
        characters := [];
        characters_hom := [];
        ()

    let clear_taxa () =
        counter_names := 0;
        names := [];
        ()
        
    let print_character_specs ch =
        output_string ch "Character\tMax\tMin\tSet\tWeight\tActive\tOrdered\n";
        let printer (b, a) =
            let print = output_string ch in
            let tab () = print "\t" in
            let print_int x = print (string_of_int x); tab () in
            let print_bool x = print (if x then "true" else "false"); tab () in
            print_int a;
            print_int b.max;
            print_int b.min;
            All_sets.Integers.iter print_int b.set;
            print_int b.weight;
            print_bool b.active;
            print_ordtype b.ordered;
            print "\n"
        in
        List.iter printer !characters

    (* A generic function to associate a name with a code, used for either
    * characters or taxa names *)
    let some_code name lst count f =
            try
                f name !lst
            with 
            | Not_found -> 
                    let n_code = !count in
                    incr count;
                    lst := (name, n_code) :: !lst;
                    n_code

    (* Symmetric to the previous. *)
    let code_some code lst  =
        let rec finder = function
            | (name, mcode) :: tl when code = mcode -> name
            | _ :: tl -> finder tl
            | [] -> raise Not_found
        in
        finder !lst

    let taxon_code name = 
        some_code name names counter_names List.assoc

    let code_taxon code =
        code_some code names

    let character_code spec =
        let a = some_code spec characters_hom counter_characters_hom List.assq
        and b = some_code spec characters counter_characters List.assoc in
        a, b

    let code_character code =
        code_some code characters

    let code_character_hom code =
        code_some code characters_hom

    let default_encoding_specs _ =
        { 
            max = -1;
            min = Pervasives.max_int;
            set = All_sets.Integers.empty;
            weight = 1;
            active = true;
            ordered = Is_ordered;
            cost_matrix = [||];
            observed_used = Do_Nothing;
            used_observed = Do_Nothing;
        }

    let update_encoding_specs curr_specs data =
        let update_data x y = 
            let max = max x.max y
            and min = if (y < 0 ) then x.min else min x.min y
            and set = if (y < 0 ) then x.set else All_sets.Integers.add y x.set 
            in
            { x with max = max; min = min; set = set; }
        in
        let do_update_encoding_specs x y = List.fold_left (update_data) x y
        in
        List.map2 (do_update_encoding_specs) curr_specs data

    let handle_array_position_error_and_message x = 
        let position, command = 
            match x with
            | Weight (_, a) -> a, "reweight"
            | Deactivate a -> a, "deactivate"
            | Activate a -> a, "activate"
            | Ordered a -> a, "make ordered"
            | Unordered a -> a, "make unordered"
            | Sankoff (_, a) -> a, "make sankoff"
            | _ -> failwith "Impossible state?"
        in
        Status.user_message Status.Error
        ("@[<v 2>@[Illegal@ character@ position:@]@,@[You@ asked@ me@ to@ " ^
        command ^ "@ the@ character@ " ^ string_of_int position ^ ",@ but@ " ^
        "@ that@ position@ does@ not@ exists@ in@ the@ input@ matrix.@ " ^
        "I@ am@ cancelling@ the@ processing@ of@ this@ file.@]@]");
        failwith "Illegal character number in input file"

    let update_options_in_specs arr x = 
        try
            match x with
            | Weight (a, b) -> arr.(b) <- { arr.(b) with weight = a }
            | Deactivate a -> arr.(a) <- { arr.(a) with active = false }
            | Activate a -> arr.(a) <- { arr.(a) with active = true }
            | Ordered a -> arr.(a) <- { arr.(a) with ordered = Is_ordered }
            | Unordered a -> arr.(a) <- { arr.(a) with ordered = Is_unordered }
            | Sankoff (b, a) -> arr.(a) <- 
                { arr.(a) with ordered = Is_sankoff; cost_matrix = b }
            | Tree _ -> ()
            | Unkown_option _ -> 
                    let msg = "Yet another impossible error" in
                    raise (Illegal_hennig86_format msg)
        with
        | Invalid_argument _ -> handle_array_position_error_and_message x

    (* Given a data set that was read from the Hennig86 matrix, the encoding
     * options of the characters and the character ranges are considered to build a set
     * of encoding parameters for each character *)
    let calc_encoding_specs data_matrix options =
        match data_matrix with
        | hd :: _ -> 
              let empty = List.map (default_encoding_specs) hd in
              let param = 
                  List.fold_left (update_encoding_specs) empty data_matrix 
              in
              let param = Array.of_list param in
              List.iter (update_options_in_specs param) options;
              param
        | [] -> [||]

    let list_list_to_matrix x = Array.of_list (List.map (Array.of_list) x)

    let objectify_channel ch = new FileStream.stream_reader ch

    (* Checks whether the given channel contains a possible hennig86 file and
     * extracts the header information (number of characters, taxa and the rest of the
     * text in a single long string). The regular expression reg checks the general
     * format and breaks down the different parts of the file. Note that the options at
     * the end of the file are not processed yet. *)
    let is_hennig ch =
        let r = objectify_channel ch in
        r#skip_ws_nl;
        let is_dpread =
            if r#match_prefix "xread"
            then false
            else if r#match_prefix "dpread"
            then true
            else raise (Illegal_hennig86_format "Illegal heading") in
        r#skip_ws_nl;
        let comment =
            if r#match_prefix "'"
            then begin
            (* skip the comment *)
                let c = (r#read_excl ['\'']) in
                ignore (r#match_prefix "'");
                Some c
            end
            else None
        in
        (* read the taxon and character counts *)
        if not ndebug then begin
            print_endline ("Reading " ^ (if is_dpread then "dpread" else
                                             "xread")
                           ^ " file with comment "
                           ^ (match comment with
                              | None -> ""
                              | Some c -> c))
        end;
        r#skip_ws_nl; let a = r#read_int in
        r#skip_ws_nl; let b = r#read_int in
        if not ndebug then begin
            print_endline ("Reading " ^ (if is_dpread then "dpread" else
                                             "xread")
                           ^ " file with "
                           ^ string_of_int a
                           ^ " characters and "
                           ^ string_of_int b
                           ^ " taxa, with comment "
                           ^ (match comment with
                              | None -> ""
                              | Some c -> c))
        end;
        a, b, r, is_dpread


    (* Like a regular array mapping but taking two arrays a *)
    let map2 a b c = 
        let b = Array.to_list b
        and c = Array.to_list c in
        let res = List.map2 a b c in
        Array.of_list res


    let make_mapping x = 
        match x with
        | { active = true; ordered = Is_unordered; weight = w; set = s } -> 
                let size = All_sets.Integers.cardinal s in
                if size <= _architecture then begin
                    let lst = All_sets.Integers.elements s in
                    let lst = List.sort (compare) lst in
                    let observed_used = Hashtbl.create size 
                    and used_observed = Hashtbl.create size in
                    let process x y =
                        Hashtbl.add observed_used y x;
                        Hashtbl.add used_observed x y;
                        x * 2
                    in
                    let _ = List.fold_left (process) 1 lst in
                    (Do_Unordered observed_used), (Do_Unordered used_observed)
                end else begin
                    let msg = "The number of states is bigger than POY's \
                               supported size." in
                    raise (Illegal_hennig86_format msg)
                end
        | { active = true; ordered = Is_ordered; weight = w; set = s } -> 
                let size = All_sets.Integers.cardinal s in
                let lst = All_sets.Integers.elements s in
                let lst = List.sort (compare) lst in 
                let res = Hashtbl.create size in
                let process y = Hashtbl.add res y y in
                List.iter (process) lst;
                (Do_Ordered res), (Do_Ordered res)
        | { active = true; ordered = Is_sankoff; weight = w; set = s } ->
                Do_Sankoff, Do_Sankoff  
        | { active = false } -> Do_Nothing, Do_Nothing

    let single_encoding_appl spec (data, unknown) =
        try 
            if not spec.active then Inactive_Character
            else 
                match spec.ordered with
                | Is_unordered ->
                        let a = 
                            match spec.observed_used with
                            | Do_Unordered a -> a
                            | _ -> failwith "Unexpected"
                        in
                        Unordered_Character (lor_list_withhash data (Some a), unknown)
                | Is_ordered ->
                    let processor ((min_x, max_x) as x) y = 
                        let y_is_less_than_min = y < min_x
                        and y_is_greater_than_max = y > max_x in
                        match y_is_less_than_min, y_is_greater_than_max with
                        | false, false -> x
                        | true, false -> y, max_x
                        | false, true -> min_x, y
                        | true, true -> y, y (* This is impossible! *)
                    in
                    let st = default_ordered in
                    let min, max = List.fold_left (processor) st data in
                    Ordered_Character (min, max, unknown)
                | Is_sankoff ->
                        Sankoff_Character (data, unknown)
        with
        | _ -> 
                let msg = "This truly is an unexpected error." in
                raise (Illegal_hennig86_format msg)

    let encode specs data = 
        Array.map (map2 (single_encoding_appl) specs) data

    let make_list names data = 
        let res = Array.to_list data in
        List.combine res names

    let merge_arrays a b =
        assert (Array.length a == Array.length b);
        let len = Array.length a in
        let first (x, _) = x
        and second (_, x) = x in
        let a = Array.init len (fun x -> first a.(x))
        and c = Array.init len (fun x -> second a.(x)) in
        Array.init len 
            (fun i -> {b.(i) with observed_used = a.(i); 
            used_observed = c.(i)})

    let correct_wildcard data specs =
        let nd = Array.init (Array.length data)
            (fun i ->
                 let a = data.(i) in
                 Array.init (Array.length a)
                     (fun j ->
                          match a.(j) with
                          | [-1] -> 
                                let x = specs.(j) in
                                let lst = match x.ordered with
                                | Is_ordered -> x.min :: x.max :: []
                                | Is_sankoff
                                | Is_unordered ->
                                      All_sets.Integers.elements x.set in
                                lst, true
                          | a -> a, false)) in
        nd
        
    let of_channel ch =
        let parser_status = Status.create "Parser" (Some 7) 
        "Parsing input files" in
        Status.report parser_status;
        let characters, taxa, text, is_dpread = is_hennig ch in
        Status.full_report ~adv:1 parser_status;
        let names, data, options, trees =
            parse_file characters taxa text is_dpread in
        Status.full_report ~adv:2 parser_status;
        let encoding_specs = calc_encoding_specs data options
        and data_matrix = list_list_to_matrix data in
        Status.full_report ~adv:3 parser_status;
        let specs = Array.map (make_mapping) encoding_specs in
        Status.full_report ~adv:4 parser_status;
        let encoding_specs = merge_arrays specs encoding_specs in
        Status.full_report ~adv:5 parser_status;
        let new_data_matrix = correct_wildcard data_matrix encoding_specs in
        Status.full_report ~adv:6 parser_status;
        let res = encode encoding_specs new_data_matrix in
        Status.full_report ~adv:7 parser_status;
        if not ndebug then begin
            print_string ("I loaded a hennig file with " ^ string_of_int
            (Array.length encoding_specs) ^ " characters.")
        end;
        Status.finished parser_status;
        encoding_specs, make_list names res, trees

    let of_file f =
        let ch = FileStream.open_in f in
        let res = of_channel ch in
        close_in ch;
        res
    
        


    let split_ordered encoding_specs taxa = 
        (* ordered and unordered characters index location in the 
        * encoding_specs array and each taxon in the list of taxa *)
        let indices_ordered, indices_unordered = 
            let ordered = ref []
            and unordered = ref []
            and len = Array.length encoding_specs in
            for i = len - 1 downto 0 do
                if encoding_specs.(i).ordered = Is_ordered then 
                    ordered := i :: !ordered
                else unordered := i :: !unordered
            done;
            (Array.of_list !ordered), (Array.of_list !unordered)
        in
        let number_ordered = Array.length indices_ordered
        and number_unordered = Array.length indices_unordered in
        (* Given a single taxon, divide its characters in ordered and unordered
        * *)
        let splitter (characters, name) = 
            let ordered_chars = 
                try Array.make number_ordered characters.(0) with _ -> [||]
            in
            let unordered_chars = 
                try Array.make number_unordered characters.(0) with _ -> [||]
            in
            for i = number_ordered - 1 downto 0 do
                ordered_chars.(i) <- characters.(indices_ordered.(i));
            done;
            for i = number_unordered - 1 downto 0 do
                unordered_chars.(i) <- characters.(indices_unordered.(i));
            done;
            (ordered_chars, name), (unordered_chars, name)
        in
        (* Extract an array elements from specs stored in its index 
        * positions as stored in the indices array *)
        let extract indices specs = 
            try
                let len = Array.length indices in
                let res = Array.make len specs.(0) in
                for i = len - 1 downto 0 do
                    res.(i) <- specs.(i);
                done;
                res
            with
            | _ -> [||]
        in
        let divided = List.map (splitter) taxa 
        and ordered_specs = extract indices_ordered encoding_specs
        and unordered_specs = extract indices_unordered encoding_specs in
        let all_ordered, all_unordered = List.split divided in
        (ordered_specs, all_ordered), (unordered_specs, all_unordered)

        
    module Encoding = struct
        type s = encoding_spec
        let dna_encoding = dna_encoding
        (* Encoding specification *)
        let default = default_encoding_specs
        let get_min x = x.min
        let get_max x = x.max
        let set_min x y = { x with min = y }
        let set_max x y = { x with max = y }
        let get_set x = x.set
        let set_set x s = { x with set = s }
        let set_tcm x m = { x with cost_matrix = m }
        let get_weight x = x.weight
        let set_weight x v = { x with weight = v }
        let is_active x = x.active
        let is_ordered x = x.ordered = Is_ordered
        let set_unordered x = { x with ordered = Is_unordered }
        let set_sankoff x mat = { 
            x with 
            cost_matrix = mat;
            ordered = Is_sankoff 
        }
        let is_sankoff x = x.ordered = Is_sankoff
        let get_observed_used x = 
            match x.observed_used with 
            | Do_Ordered a | Do_Unordered a -> Some a 
            | Do_Nothing -> None
            | Do_Sankoff -> None
        let get_used_observed x = 
            match x.used_observed with 
            | Do_Ordered a | Do_Unordered a -> Some a 
            | Do_Nothing -> None
            | Do_Sankoff -> None
        let set_used_observed item x = 
            {x with used_observed = item }
        let to_string enc = 
            let active = if enc.active then "Active, " else "Inactive, "
            and ordered = if enc.ordered = Is_ordered then "Additive, " 
            else "Non Additive, "
            and weight = "Weight: " ^ string_of_int enc.weight in
            active ^ ordered ^ weight
        let has_states min max x = 
            let cardinal = All_sets.Integers.cardinal x.set in
            cardinal >= min && cardinal <= max
        let get_tcm enc = enc.cost_matrix
        let get_set enc = enc.set
        let gap_encoding gapcost =
            let res = default () in
            let res = set_min res 1 in
            let res = set_max res 2 in
            let res = set_weight res gapcost in
            let set = 
                List.fold_left (fun acc x -> All_sets.Integers.add x acc) 
                All_sets.Integers.empty [1;2] 
            in
            let res = set_set res set in
            let res = set_unordered res in
            let used_observed = 
                let htb = Hashtbl.create 2 in
                Hashtbl.add htb 1 1;
                Hashtbl.add htb 2 2;
                htb
            in
            { res with used_observed = Do_Unordered used_observed }
    end

   let convert_to_Phylip_format_file hennig_filename phylip_filename = 
       let fprintf = Printf.fprintf in 
       let ch = FileStream.open_in hennig_filename in 
       let encode_arr, taxon_ls, _ = of_channel ch in 
       close_in ch;

       let phy_file = open_out phylip_filename in
       let num_taxon = List.length taxon_ls in 
       let num_char = Array.length encode_arr in 
       fprintf phy_file "%i %i\n" num_taxon num_char;


        let elt_iter elt =
            let rec elt_iter acc elt c =
                if elt = 0 then acc 
                else if (elt land 1) <> 0 then 
                    elt_iter (c :: acc) (elt lsr 1) (c * 2)
                else elt_iter acc (elt lsr 1)  (c * 2)
            in
            elt_iter [] elt 1
        in

        let name_f = ref 0 in 
        List.iter (fun  (cont_arr, name) -> 
            fprintf phy_file "T%i     " (!name_f + 10000);                   
           name_f := !name_f + 1;
           Array.iteri (fun index state ->
                match state with
                | Unordered_Character (key, _) ->       
                      let key_ls = elt_iter key in 
                      let ht_opt = Encoding.get_used_observed encode_arr.(index) in 
                      let state_ls = 
                          match ht_opt with
                          | None -> failwith "Hashtable is NONE"
                          | Some ht -> 
                                List.map (fun state_key -> 
                                              Hashtbl.find ht state_key) key_ls
                      in 

                      let dna_code = List.fold_left 
                          (fun acc state ->
                               match state with 
                               | 0 -> acc + 1 
                               | 1 -> acc + 2 
                               | 2 -> acc + 4 
                               | 3 -> acc + 8 
                               | _ -> acc + 16) 0 state_ls 
                      in
(*
                      if List.length state_ls = 1 then 
                          Printf.fprintf stdout "%i"  (List.hd state_ls)
                      else begin
                          Printf.fprintf stdout "[";
                          List.iter (Printf.fprintf stdout "%i") state_ls;
                          Printf.fprintf stdout "]";
                      end;
*)
                      let new_dna_code = 
                          match dna_code > 16 with
                          | true -> dna_code - 16
                          | false ->  dna_code
                      in 

                      if new_dna_code = 16 then fprintf phy_file "-"
                      else fprintf phy_file "%s"  
                          (Alphabet.match_code new_dna_code Alphabet.nucleotides)
                | _ -> () ) cont_arr;  
               
           fprintf phy_file "\n") taxon_ls; 
        
        close_out phy_file;
        print_endline "End of converting from Hennig format to Phtlip format"
 

    (* If taxa lists are added to the functionality (and they will), this is the
    * place. Simply add a parameter with the set of taxa that will be used in
    * the analysis and match it in the execution (below all these functions). I
    * don't need it now, so Ron can attack that feature later. *)
    let merger (all_hennig_files : (Encoding.s array * (t array * string) list)
    list) =
        (* Some internal functions necessary *)
        (* [codify_taxa files] checks the parsed hennig files contained in the
        * [files] list, see if all the files are actually shared or not, and
        * request codes for each one from the general parser taxon code
        * generator. The taxon names are then replaced by integers. *)
        let all_exist name =
            try 
                let _ = taxon_code name in
                ()
            with
            | Not_found -> raise (Unknown_taxon name)
        in
        let codify_taxa all_hennig_files =
            let extract_taxa (_, it) = 
                let _, b = List.split it in 
                b
            in
            let replace_names_with_codes (a, it) =
                let b, c = List.split it in
                a, List.combine b (List.map taxon_code c)
            in
            match List.map extract_taxa all_hennig_files with
            | hd :: tl ->
                    (* First verify that all the files share the same taxa
                    * *)
                    let _ = List.map taxon_code hd in
                    List.iter (List.iter all_exist) tl;
                    (* Assign to each taxon a particular code from the
                    * module taxon code assigned. *)
                    List.map replace_names_with_codes all_hennig_files
            | [] -> []
        in
        (* Merges all the hennig files in one long list array with all the
        * characters. The function assumes that all the taxa already have codes
        * and all the files have the same taxa. *)
        let merge_all_files files = 
            (* We need to guarantee that all the files have the taxa sorted *)
            let sort_taxa (a, b) = 
                let b = Array.of_list b in
                Array.sort (fun (_, x) (_, y) -> x - y) b;
                a, Array.to_list b
            in
            (* now we need something able to merge a pair of files once they are
            * sorted. *)
            let merge_pair (fa, lsta) (fb, lstb) =
                let rec merger_of_taxa a b =
                    match a, b with
                    | (ch_a, nm_a) :: tla, (ch_b, nm_b) :: tlb 
                        when nm_a = nm_b ->
                            let ntl = merger_of_taxa tla tlb in
                            ((Array.append ch_a ch_b), nm_a) :: ntl
                    | [], [] -> []
                    | _, _ -> 
                            raise (Unexpected "merge_all_files with unequal \
                            length lists.");
                in
                Array.append fa fb, merger_of_taxa lsta lstb
            in
            (* So sort and merge them all now *)
            match List.map sort_taxa files with
            | hd :: tl -> List.fold_left merge_pair hd tl
            | [] -> [||], []
        in
        let assign_character_code (chars, b) =
            (Array.map character_code chars), b
        in
        (* We don't have an index of equivalent taxa, so the names
        must match in all the files. *)
        let all_files = codify_taxa all_hennig_files in
        let all_files = merge_all_files all_files in
        assign_character_code all_files

    let general_character_filter f = 
        let res = List.filter (fun (a, b) -> f a) !characters in
        let _, b = List.split res in
        b

    (* Return the character codes with states between min and max *)
    let character_states_minmax min max = 
        let filter = fun x -> x.min >= min && x.max <= max in
        general_character_filter filter
    
    (* Filter characters with the number of states between min and max *)
    let character_states_number min max = 
        let filter = 
            fun x -> 
                let cardinal = All_sets.Integers.cardinal x.set in
                cardinal >= min && cardinal <= max
        in
        general_character_filter filter

    let character_additive () = 
        let filter = fun x -> x.ordered = Is_ordered in
        general_character_filter filter

    let character_nonadditive () =
        let filter = fun x ->  x.ordered = Is_unordered in
        general_character_filter filter

    let character_active () =
        let filter = fun x -> x.active in
        general_character_filter filter


    let character_inactive () =
        let filter = fun x -> not x.active in
        general_character_filter filter

    let character_complement lst = 
        let filter = 
            fun (_, c) -> not (List.exists (fun x -> c = x) lst)
        in
        let res = List.filter filter !characters in
        let _, a = List.split res in
        a

    let character_additive_minmax min max=
        let filter = fun x -> x.ordered = Is_ordered
        &&  x.min >= min && x.max <= max in
        general_character_filter filter
    
    let character_nonadditive_minmax min max=
        let filter = fun x -> x.ordered = Is_unordered
        &&  x.min >= min && x.max <= max in
        general_character_filter filter
    
    let character_active_minmax min max=
        let filter = fun x -> x.active &&  x.min >= min && x.max <= max in
        general_character_filter filter

    let character_inactive_minmax min max=
        let filter = fun x -> not x.active &&  x.min >= min && x.max <= max in
        general_character_filter filter

    let filter_matrix its m = 
        let (codes, characters) = m in
        let _, selected = 
            Array.fold_left begin
                fun (pos, accu) (hom_code, spec_code) ->
                    if List.exists (fun x -> x = spec_code) its then (pos + 1, pos ::
                        accu)
                    else (pos + 1, accu)
            end (0, []) codes 
        in
        let selected = List.rev selected in
        let len = List.length selected in
        (* Now we need a couple of functions, one to select the appriate columns
        * in a particular taxon row. *)
        let filter_taxon = fun (chars, tcode) ->
            if 0 < Array.length chars then begin
                let new_chars = Array.make len chars.(0) in
                let _ = List.fold_left begin
                        fun x y -> new_chars.(x) <- chars.(y); x + 1 
                    end 0 selected  
                in
                new_chars, tcode
            end else ([||], tcode)
        in
        (* In the same way we now need to filter out the character codes *)
        let filter_codes = fun arr ->
            if 0 < Array.length arr then begin
                let new_codes = Array.make len arr.(0) in
                let _ = List.fold_left begin
                    fun x y -> new_codes.(x) <- arr.(y); x + 1
                end 0 selected 
                in
                new_codes
            end else [||]
        in
        filter_codes codes, List.map filter_taxon characters

    let flatten_matrix (codes, mtx) =
        let codes = Array.map (fun (x, _) -> x) codes in
        List.map begin
            fun (arr, taxon) ->
                Array.mapi begin
                    fun pos c ->
                        c, codes.(pos)
                end arr, taxon
        end mtx

    let categorize_chars (chararray, taxon_index) =
        let characters_hom = !characters_hom in
        let has_states min max x = 
            let cardinal = All_sets.Integers.cardinal x.set in
            cardinal >= min && cardinal <= max
        in
        let has_more_states min x =
            let cardinal = All_sets.Integers.cardinal x.set in
            cardinal >= min
        in
        let is_additive x =
            (x.ordered = Is_ordered) in
        let is_nonadditive x =
            (x.ordered = Is_unordered) in
        let filter_characters filter list =
            List.filter (fun (a, b) -> filter a) list in
        let get_codes list =
            let _, b = List.split list in
            b in
        
        let chars_additive = filter_characters is_additive characters_hom in
        let chars_nonadd = filter_characters is_nonadditive characters_hom in
        let chars_nonadd_8 = filter_characters (has_states 0 8) chars_nonadd in
        let chars_nonadd_16 = filter_characters (has_states 9 16) chars_nonadd
        in
        let chars_nonadd_32 = filter_characters (has_states 17 31) chars_nonadd
        in
        let chars_nonadd_33plus = filter_characters (has_more_states 32)
            chars_nonadd in

        let codes_add, codes_nonadd8, codes_nonadd16, codes_nonadd32, codes_nonadd33 =
            (get_codes chars_additive,
             get_codes chars_nonadd_8,
             get_codes chars_nonadd_16,
             get_codes chars_nonadd_32,
             get_codes chars_nonadd_33plus) in

        let array_filter array filter =
            let list = Array.to_list array in
            let list = List.filter filter list in
            let array = Array.of_list list in
            array in

        let filter_array_codes codes =
            array_filter chararray (fun (char, code) -> List.mem code codes) in

        let final_filter codes =
            (filter_array_codes codes, taxon_index) in

        (final_filter codes_add,
         final_filter codes_nonadd8,
         final_filter codes_nonadd16,
         final_filter codes_nonadd32,
         final_filter codes_nonadd33)
         
     
end

module GrappaParser = struct
    
    (* Given a text in_channel ch, make it a long string where the newlines
    * are replaced with single spaces. *)
    let stringify_channel ch = 
        let res = ref "" in
        try 
            while true do
                res := !res ^ " " ^ (input_line ch);
            done;
            ""
        with
        | End_of_file -> !res
   
    let rec make_list_of_genes input_list output_list =
        match input_list with
        | [] -> List.rev output_list
        | head :: tail -> 
                let blank_space = Str.regexp " +" in
                let name = List.hd (Str.split blank_space head ) in
                let genes = Str.string_after head (String.length name) in
                let genelist = Str.split blank_space genes in
                let thegenes = Array.of_list 
                   (List.map int_of_string genelist) in
                make_list_of_genes tail (thegenes::output_list)
 
    let of_channel ch =
        let line = stringify_channel ch
        and reg = Str.regexp " *>" in 
        if Str.string_match reg line 0 then
            begin
                let name_gene_list = Str.split reg line in
                let res = make_list_of_genes name_gene_list [] in 
                let res_array = Array.of_list res in
                Array.map (fun x -> Genes x) res_array
            end
        else
            raise (Unsupported_file_format "Not a proper Grappa file") 
            
            
end        

module TransformationCostMatrix = struct

    let of_channel = Cost_matrix.Two_D.of_channel

    let of_file ?(use_comb = true) file =
        let ch = FileStream.Pervasives.open_in file in
        let res = of_channel ~use_comb:use_comb ch in
        ch#close_in;
        res

    let of_channel_nocomb = Cost_matrix.Two_D.of_channel_nocomb

    let of_list = Cost_matrix.Two_D.of_list

end

module Dictionary = struct

    let of_channel ch = 
        let input_handler = new FileStream.stream_reader ch in
        let rec reader hash counter = 
            try
                let line = input_handler#read_line in
                match Str.split (Str.regexp "\\s+") line with
                | [] | [_] -> 
                        let msg = 
                            ("Line " ^ string_of_int counter ^ ": " ^ line)
                        in
                        raise (Illegal_dictionary msg)
                | hd :: tl ->
                        List.iter (fun x -> Hashtbl.add hash x hd) tl;
                        reader hash (counter + 1)
            with
            | _ -> hash
        in
        reader (Hashtbl.create 1000) 1

end

module FixedStatesDict = struct

    let of_channel t ch = 
        let rec reader lexer alph acc counter =
            try
                let line = input_line ch in
                let acc = (Fasta.process_sequence true lexer alph [line]) :: acc in
                reader lexer alph acc (counter + 1)
            with
            | End_of_file -> acc
        in
        match t with
        | Nucleic_Acids -> 
                let lexer = Alphabet.Lexer.make_lexer true Alphabet.nucleotides in
                reader lexer Alphabet.nucleotides [] 1
        | Proteins -> 
                let msg = "The current list of protein codes is empty. \
                Request an update or contact the POY maintainer." in
                raise (Unsupported_file_format msg)
        | AlphSeq x ->
                let lexer = Alphabet.Lexer.make_lexer true x in
                reader lexer x [] 1 
        | _ -> 
                let msg = 
                    "Unknown file format for a Fixed states dictionary. "
                in
                raise (Unsupported_file_format msg)

    let of_file t f =
        let ch = FileStream.open_in f in
        let res = of_channel t ch in
        close_in ch;
        res

    module OderedSequence = struct
        type t = Sequence.s
        let compare = Sequence.compare
    end

    module SeqMap = Map.Make (OderedSequence)

    let create_dp_read_file ?filename file taxa lst tcm = 
        (* We report to the user what we intend to do *)
        let outName, ch = 
            match filename with
            | Some v -> v, open_out v
            | None -> Filename.open_temp_file file ".tmp" 
        in
        let len = List.length taxa in
        (* Print the header of the file *)
        Printf.fprintf ch "dpread\n";
        Printf.fprintf ch "\'Poy file generated for fixed states analysis \
        from\nthe original file %s.\'\n" file;
        Printf.fprintf ch "1 %d\n" len;
        (* We first load the fixed states *)
        let res = List.fold_left (fun (code, acc) seq ->
            match SeqMap.mem seq acc with
            | false -> code + 1, SeqMap.add seq code acc
            | true -> code, acc) (0, SeqMap.empty) lst 
        in
        (* Now we load the observations and print them out in the file *)
        let len, map = List.fold_left (fun (code, acc) (seq, taxon) ->
            match SeqMap.mem seq acc with
            | false -> 
                    Printf.fprintf ch "%s %d\n" taxon code;
                    code + 1, SeqMap.add seq code acc 
            | true -> 
                    let tc = SeqMap.find seq acc in
                    Printf.fprintf ch "%s %d\n" taxon tc;
                    code, acc) res taxa 
        in
        (* Print the transformation cost matrix *)
        Printf.fprintf ch ";\ncost [ 0 $ %d\n" len;
        let asc_lst = SeqMap.fold (fun a b acc -> (b, a) :: acc) map [] in
        let asc_lst = List.sort (fun (a, _) (b, _) -> a - b) asc_lst in
        (* A function to print out the necessary header of the transformation
        * cost matrix in the dpread file *)
        let rec print_table_header x =
            if x < len then begin
                Printf.fprintf ch "%d " (x mod 10);
                print_table_header (x + 1)
            end else 
                Printf.fprintf ch "\n";
        in
        print_table_header 0;
        let len = List.length asc_lst in
        let st = Status.create "Parser" (Some len) ("Converting " ^ file ^ 
        " to dpread file \"" ^ outName ^ "\"") in
        List.iter (fun (_, x) ->
            List.iter (fun (_, y) ->
                let c = Sequence.Align.cost_2 x y tcm Matrix.default in
                Printf.fprintf ch "%d " c;) asc_lst;
                (* Report to the user *)
                Status.full_report ~adv:(1 + Status.get_achieved st) st;
                Printf.fprintf ch "\n";) asc_lst;
        Status.finished st;
        close_out ch;
        outName
end

module IgnoreList = struct
    let of_channel ch = 
        let input_handler = new FileStream.stream_reader ch in
        let res = ref [] in
        try
            while true do
                let input = input_handler#read_line in
                res := input :: !res;
            done;
            List.rev !res
        with
        | _ -> List.rev !res
end

module Alphabet = struct
    let of_file fn orientation init3D = 
        let file = FileStream.Pervasives.open_in fn in
        let alph = FileStream.Pervasives.input_line file in
        let default_gap = "_" in
        let elts = ((Str.split (Str.regexp " +") alph) @ [default_gap]) in
        let alph = Alphabet.of_string ~orientation:orientation
            elts default_gap None in
        let tcm = TransformationCostMatrix.of_channel_nocomb
            ~orientation:orientation file in
        let tcm3 = 
            match init3D with
            | true -> Cost_matrix.Three_D.of_two_dim tcm
            | false  ->  Cost_matrix.Three_D.default 
        in 
        file#close_in;
        alph, tcm, tcm3
end

(** Module to read and process complex terminal definition files *)
module SetGroups = struct
    type set_type =
            | Group 
            | Any of float
            | Tree of float             (* origin/loss cost *)
    type 'a ct =                        (* this is like a sexpr *)
            | Elt of 'a
            | Set of 'a * set_type * 'a ct list
    type t = string ct

    (* What constitutes a taxon name *)
    let taxon_name = FileStream.is_not FileStream.is_ws_nl

    let set_type_to_string = function
        | Group -> "G:"
        | Any f -> "A(" ^ string_of_float f ^ "):"
        | Tree f -> "T(" ^ string_of_float f ^ "): "
    let rec to_string (groups : t) : string =
        match groups with
        | Elt taxon -> taxon
        | Set (name, t, list) ->
              "{"
              ^ set_type_to_string t
              ^ (String.concat " " (List.map to_string list)) ^ "}"

    (** [of_file fn] reads a CT definition from a file *)
    let of_file fn : t list =
        let r = new FileStream.file_reader fn in
        let line = r#read_line in
        if not (anywhere_match (Str.regexp "COMPLEX") line) then begin
            Status.user_message Status.Error 
            ("Illegal@ Complex@ Terminal@ File:@ The@ file@ doesn't@ start@ " ^
            "with@ a@ line@ containing@ the@ COMPLEX@ keyword.");
            failwith "Illegal Complex file format";
        end else
            let rec read (curr : t list) : t list =
                let read_taxon () =
                    r#read_while taxon_name in
                r#skip_ws_nl;
                if r#match_prefix "}"
                then List.rev curr
                else
                try
                    let taxon = read_taxon () in
                    if taxon = ""
                    then failwith "unexpected in group parser 000001";

                    (* check whether this is a group *)
                    r#skip_ws_nl;
                    if r#match_prefix "{"
                    then begin
                            (* try matching a specification prefix *)
                            let set_type =
                                if r#match_prefix "G:"
                                then Group
                                else if r#match_prefix "A("
                                then begin
                                        let cost = r#read_float in
                                        if not (r#match_prefix "):")
                                        then failwith "Expected close parenthesis and colon in any cost spec.";
                                        Any cost
                                end else if r#match_prefix "T("
                                then begin
                                        let cost = r#read_float in
                                        if not (r#match_prefix "):")
                                        then failwith "Expected close parenthesis and colon in tree cost spec.";
                                        Tree cost
                                    end
                                else Any 0. in
                            let set_elts = read [] in
                            let set = Set (taxon, set_type, set_elts) in
                            read (set :: curr)
                        end
                    else read (Elt taxon :: curr)
                with End_of_file -> List.rev curr
            in 
            read []

    let unify_names n1 n2 =
        if n1 = n2
        then n1
        else n1 ^ " " ^ n2

    (** [unify a b] unifies two set specification types, or raises an exception
        if they cannot be unified.  We do this because an [`Any_Of] set could be matched
        with another type, in which case that second object should be implicitly
        converted to an [`Any_Of] *)
    let rec unify a b = match a, b with
    | Elt _, Elt _ -> a
    | Set (n1, s1, c1), Set (n2, s2, c2) ->
          let n = unify_names n1 n2 in
          begin
              match s1, s2 with
              | Group, Group ->
                    if List.length c1 = List.length c2
                    then begin
                            let types = List.map2 unify c1 c2 in
                            Set (n, Group, types)
                        end
                    else failwith "unify: mismatched cardinality of grouping sets"
              | Any v1, Any v2 ->
                    (* We need to unify a type for all of the elements *)
                    if v1 <> v2 
                    then failwith "unify: trees with different origin/loss costs"
                    else
                        let utype = unify_list (c1 @ c2) in
                        Set (n, Any v1, [utype])
              | Tree f1, Tree f2 ->
                    (* Verify that the floats are the same *)
                    if f1 <> f2
                    then failwith "unify: trees with different origin/loss costs"
                    else begin
                            let utype = unify_list (c1 @ c2) in
                            Set (n, Tree f1, [utype])
                        end
              (* If Any is set against another type, we insert an implicit Any *)
              | Any v, _ ->
                    let utype = unify_list (b :: c1) in
                    Set (n, Any v, [utype])
              | _, Any v ->
                    let utype = unify_list (a :: c2) in
                    Set (n, Any v, [utype])
              | _ ->
                    failwith "unify: incompatible set types"
          end
    | Set (sn, Any j, sc), Elt eltn
    | Elt eltn, Set (sn, Any j, sc) ->
          Set (sn, Any j, [Elt eltn])
    | _ -> failwith "unify: cannot unify element with set"
    and unify_list = function
        | [] -> failwith "unify_list: empty list"
        | l :: ls -> List.fold_left unify l ls

    (** [coerce_to_type utype set] puts a type into canonical form *)
    let rec coerce_to_type utype set = match utype, set with
    | Elt _, Elt _ -> set
    | Set (_, Group, subtypes), Set (n, Group, subelts) ->
          Set (n, Group, List.map2 coerce_to_type subtypes subelts)
    | Set (_, Any v, subtype), Set (n, Any _, subelts) ->
          let subtype = match subtype with
          | [s] -> s
          | _ -> assert false in
          Set (n, Any v, List.map (coerce_to_type subtype) subelts)
    | Set (_, Tree f, subtype), Set (n, Tree f', subelts) ->
          let subtype = match subtype with
          | [s] -> s
          | _ -> assert false in
          assert(f = f');
          Set (n, Tree f, List.map (coerce_to_type subtype) subelts)
    | Set (n, Any v, subtype), elt ->
          let subtype = match subtype with
          | [s] -> s
          | _ -> assert false in
          Set (n, Any v, [coerce_to_type subtype elt])
    | _ -> assert false
end

module ClustalSeq = struct
    let process_line filename hash taxa line_number line =
        if 0 = String.length line || ' ' = line.[0] then 
            taxa
        else 
            match Str.split (Str.regexp " +") line with
            | [taxon; sequence] -> 
                    Hashtbl.add hash taxon sequence;
                    if All_sets.Strings.mem taxon taxa then taxa
                    else All_sets.Strings.add taxon taxa 
            | _ -> 
                    raise (Unexpected
                    ("Unexpected line " ^ string_of_int line_number ^ 
                    " in Clustal file " ^ filename ^ ": " ^ line))

    let merge_lines hash ch taxon = 
        let seqs  = Hashtbl.find_all hash taxon in
        output_string ch ">";
        output_string ch taxon;
        output_string ch "\n";
        List.fold_right (fun x () ->
            output_string ch x; ()) 
        seqs ();
        output_string ch "\n\n"

    let convert_to_fasta file =
        let ch, file = FileStream.channel_n_filename file in
        let first_line = input_line ch 
        and outName, chout = Filename.open_temp_file "fasta" ".tmp" in
        if "CLUSTAL" = String.sub first_line 0 7 then
            let hash = Hashtbl.create 97 
            and line_number = ref 0 
            and taxa = ref All_sets.Strings.empty in
            try
                while true do
                    let nt = 
                        process_line file hash !taxa !line_number
                        (input_line ch)
                    in
                    taxa := nt;
                    incr line_number;
                done;
                open_in outName
            with
            | End_of_file ->
                    All_sets.Strings.iter (merge_lines hash chout) !taxa;
                    close_out chout;
                    open_in outName
        else 
            raise (Unexpected 
            ("The first line of a fasta file has to begin with the word \
            CLUSTAL"))

end

let print_error_message fl =
    let msg = "Unexpected@ character@ in@ file@ " ^ fl.filename ^ 
    "@ in@ taxon@ " ^ fl.taxon ^ ".@ The@ character@ '" ^ fl.character ^ "' " ^ 
    "is@ illegal@ in@ this@ file@ format." in
    Status.user_message Status.Error msg

let molecular_to_fasta file =
    match test_file file with
    | Is_Clustal -> ClustalSeq.convert_to_fasta file
    | Is_GBSeq -> GBSeq.convert_to_fasta file
    | Is_TinySeq -> TinySeq.convert_to_fasta file
    | Is_INSDSeq -> INSDSeq.convert_to_fasta file
    | Is_XML -> XML.convert_to_fasta file
    | Is_ASN1 -> ASN1.convert_to_fasta file
    | Is_Genbank -> Genbank.convert_to_fasta file
    | Is_NewSeq -> NewSeq.convert_to_fasta file
    | _ -> FileStream.open_in file

