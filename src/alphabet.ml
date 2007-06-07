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

let () = SadmanOutput.register "Alphabet" "$Revision: 1865 $"

(* $Id: alphabet.ml 1865 2007-06-07 16:58:32Z andres $ *)

exception Illegal_Character of string
exception Illegal_Code of int

type kind = 
    | Sequential
    | Simple_Bit_Flags
    | Extended_Bit_Flags

type a = {
    string_to_code : int All_sets.StringMap.t;
    code_to_string : string All_sets.IntegerMap.t;
    gap : int;
    all : int;
    size : int;
    kind : kind;
}

(* The alphabet type *)

(* Some constant alphabets  *)

(* Nucleic Acids. Each is assigned a different bit in an integer for better
* representation of different combinations of bases (e.g. adenine or citosine =
* 3 = 1 + 2 = adenine lor citosine). *)
let adenine = 1
let citosine = 2
let guanine = 4
let timine = 8 
let gap = 16
let uracile = timine

(* Amino Acids. Each is assigned a unique number *)
let alanine = 1
let arginine = 2
let asparagine = 3
let aspartic = 4
let cysteine = 5
let glutamine = 6
let glutamic = 7
let glycine = 8
let histidine = 9
let isoleucine = 10
let leucine = 11
let lysine = 12
let methionine = 13
let phenylalanine = 14
let proline = 15
let serine = 16
let threonine = 17
let tryptophan = 18
let tyrosine = 19
let valine = 20
let aa_gap = 21
let unspecified = 22
let all_aminoacids = 22



let list_to_a lst gap all kind = 
    let add (s2c, c2s, cnt) (a, b) =
        All_sets.StringMap.add a b s2c,
        (if All_sets.IntegerMap.mem b c2s then c2s
        else All_sets.IntegerMap.add b a c2s),
        cnt + 1
    in
    let empty = All_sets.StringMap.empty, All_sets.IntegerMap.empty, 0 in
    let s2c, c2s, cnt = List.fold_left add empty lst in
    let gap_code = All_sets.StringMap.find gap s2c    
    and all_code = All_sets.StringMap.find all s2c in
    { string_to_code = s2c; code_to_string = c2s; gap = gap_code; all = all_code;
      size = cnt; kind = kind }

(* The alphabet limited to the four bases *)
let dna =
    list_to_a 
    [
        ("A", adenine);
        ("C", citosine);
        ("G", guanine);
        ("T", timine);
        ("_", gap);
        ("X", (adenine lor citosine lor guanine lor timine lor gap))
    ] "_" "X" Simple_Bit_Flags

(* The alphabet of accepted IUPAC codes (up to N), and other codes used in the
* POY file format (_ up to |). *)
let nucleotides = 
    list_to_a
    [ 
        ("A", adenine); 
        ("C", citosine); 
        ("G", guanine);
        ("T", timine); 
        ("U", uracile);
        ("M", adenine lor citosine); 
        ("R", adenine lor guanine); 
        ("W", adenine lor timine); 
        ("S", citosine lor guanine); 
        ("Y", citosine lor timine); 
        ("K", guanine lor timine); 
        ("V", adenine lor citosine lor guanine); 
        ("H", adenine lor citosine lor timine); 
        ("D", adenine lor guanine lor timine); 
        ("B", citosine lor guanine lor timine); 
        ("N", adenine lor citosine lor timine lor guanine); 
        ("X", adenine lor citosine lor timine lor guanine); 
        ("-", gap); 
        ("_", gap); 
        ("1", 17);
        ("2", 18);
        ("3", 19);
        ("4", 20);
        ("5", 21);
        ("6", 22);
        ("7", 23);
        ("8", 24);
        ("9", 25);
        ("0", 26);
        ("!", 27);
        ("@", 28);
        ("$", 29);
        ("%", 30);
        ("*", 31);
        ("?", 31);
        ("a", adenine); 
        ("c", citosine); 
        ("g", guanine);
        ("t", timine); 
        ("u", uracile);
        ("m", adenine lor citosine); 
        ("r", adenine lor guanine); 
        ("w", adenine lor timine); 
        ("s", citosine lor guanine); 
        ("y", citosine lor timine); 
        ("k", guanine lor timine); 
        ("v", adenine lor citosine lor guanine); 
        ("h", adenine lor citosine lor timine); 
        ("d", adenine lor guanine lor timine); 
        ("b", citosine lor guanine lor timine); 
        ("x", adenine lor citosine lor timine lor guanine); 
        ("n", adenine lor citosine lor timine lor guanine); 
    ] "_" "*" Extended_Bit_Flags

(* The list of aminoacids *)
let aminoacids =
    list_to_a
    [
        ("A", alanine); 
        ("R", arginine); 
        ("N", asparagine); 
        ("D", aspartic); 
        ("C", cysteine); 
        ("Q", glutamine); 
        ("E", glutamic); 
        ("G", glycine); 
        ("H", histidine); 
        ("I", isoleucine); 
        ("L", leucine); 
        ("K", lysine); 
        ("M", methionine); 
        ("F", phenylalanine); 
        ("P", proline); 
        ("S", serine); 
        ("T", threonine); 
        ("W", tryptophan); 
        ("Y", tyrosine); 
        ("V", valine); 
        ("X", all_aminoacids); 
        ("_", aa_gap);
        ("-", aa_gap);
        ("a", alanine); 
        ("r", arginine); 
        ("n", asparagine); 
        ("d", aspartic); 
        ("c", cysteine); 
        ("q", glutamine); 
        ("e", glutamic); 
        ("g", glycine); 
        ("h", histidine); 
        ("i", isoleucine); 
        ("l", leucine); 
        ("k", lysine); 
        ("m", methionine); 
        ("f", phenylalanine); 
        ("p", proline); 
        ("s", serine); 
        ("t", threonine); 
        ("w", tryptophan); 
        ("y", tyrosine); 
        ("v", valine); 
        ("x", all_aminoacids); 
    ] "_" "X" Sequential

let match_base x alph =
    try
        let x = String.uppercase x in
        All_sets.StringMap.find x alph.string_to_code 
    with
    | Not_found -> raise (Illegal_Character x)



let find_base = match_base

let match_code x alph =
    try All_sets.IntegerMap.find x alph.code_to_string with
    | Not_found -> raise (Illegal_Code x)

let find_code = match_code

let of_string ?(orientation = false) x gap all =
    let rec builder alph counter = function
        | h :: t -> 
              if orientation then 
                  builder ((h, counter) :: ("~" ^ h, counter + 1) :: alph)
                      (counter + 2) t
              else 
                  builder ((h, counter):: alph) (counter + 1)  t

        | [] -> List.rev alph
    in
    let res = builder [] 1 x in
    let alpha = list_to_a res gap all Sequential in 
    alpha

let size a = a.size

let rnd a = 
    fun () ->
        let it = Random.int a.size in
        let finder a _ (cnt, res) =
            if cnt = it then (cnt + 1, Some a)
            else (cnt + 1, res)
        in
        match All_sets.IntegerMap.fold finder a.code_to_string (0, None) with
        | _, Some x -> x
        | _, None -> failwith "Alphabet.rnd"


let get_all a = a.all

let get_gap a = a.gap

let to_list a =
    All_sets.StringMap.fold (fun a b acc -> (a, b) :: acc) a.string_to_code []

module Lexer = struct
    (** A module to make a stream processor for a given alphabet type *)
    module OrderedChar = struct
        type t = char
        let compare (a : char) (b : char) = Pervasives.compare a b
    end

    module CM = Map.Make (OrderedChar)

    type p = Code of int | Unfinished of p CM.t

    let make_lexer issue_warnings a = 
        let rec add_stream stream code acc =
            match acc with
            | Code x -> failwith "This alphabet is not prefix free"
            | Unfinished set ->
                    try
                        let c = Stream.next stream in
                        if CM.mem c set then 
                            let nacc = CM.find c set  in
                            match add_stream stream code nacc with
                            | Code _ -> failwith "This alphabet is not prefix free"
                            | res -> Unfinished (CM.add c res set)
                        else 
                            let nacc = Unfinished CM.empty in
                            let res = add_stream stream code nacc in
                            let nacc = CM.add c res set in
                            Unfinished nacc
                    with
                    | Stream.Failure -> Code code
        in
        let lst = All_sets.StringMap.fold (fun a b acc ->
            (Stream.of_string a, b) :: acc) a.string_to_code []
        in
        let lexer = List.fold_left (fun acc (a, b) ->
            add_stream a b acc) (Unfinished CM.empty) lst
        in
        fun stream lst len ->
            let rec single_processor acc = function
                | Code x -> 
                        x :: acc
                | Unfinished x ->

                        let c = Stream.next stream in
                        try single_processor acc (CM.find c x) with
                        | Not_found as err ->
                                if issue_warnings then begin
                                    Status.user_message Status.Error 
                                    ("I@ could@ not@ find@ the@ character@ " ^ 
                                    String.make 1 c ^ "@ in@ position@ " ^
                                    string_of_int (Stream.count stream));
                                    Status.user_message Status.Error
                                    ("I@ found@ an@ illegal@ character@ in@ " ^
                                    "the@ " ^ "last@ file@ I@ was@ reading.");
                                end else ();
                                raise err
            in
            let rec full_processor acc cnt =
                match Stream.peek stream with
                | Some v ->
                        begin match v with
                        | ' ' | '\010' | '\012' | '\014' | '\015' ->
                                Stream.junk stream;
                                full_processor acc cnt
                        | _ ->
                                let res = single_processor acc lexer in
(*                                Printf.fprintf stdout " "; *)
                                full_processor res (cnt + 1)
                        end
                | None -> acc, cnt
            in
            let res, cnt = full_processor lst len in
            res, cnt
end


let print alpha = 
    All_sets.IntegerMap.iter (fun code char -> 
                       Printf.fprintf stdout "%6i %s\n" code char)
    alpha.code_to_string;
    print_newline ()

let kind alpha = alpha.kind

let simplified_alphabet alph =
    match alph.kind with
    | Simple_Bit_Flags
    | Sequential -> alph
    | Extended_Bit_Flags ->
            (* We need to extract those numbers that only have one bit on *)
            let gap = get_gap alph
            and all = get_all alph in
            let has_one_bit_or_all v =
                if v = all then true
                else
                    let rec has_only_one_bit_on v =
                        if v = 1 then true
                        else if 0 <> (1 land v) then false
                        else has_only_one_bit_on (v lsr 1)
                    in
                    has_only_one_bit_on v
            in
            let add_those_who_have_it v name acc =
                if has_one_bit_or_all v then (name, v) :: acc
                else acc
            in
            let list = 
                All_sets.IntegerMap.fold add_those_who_have_it 
                alph.code_to_string []
            in
            list_to_a list (find_code gap alph) 
            (try find_code all alph with _ -> "*") Simple_Bit_Flags

let distinct_size alph =
    All_sets.IntegerMap.fold (fun _ _ acc -> acc + 1) alph.code_to_string 0

(*    code_to_string : string All_sets.IntegerMap.t;    *)
(* vim: set et sw=4 tw=80: *)
