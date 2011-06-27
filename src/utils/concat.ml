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

(** Concat takes a list of files (with wildcards), and concatenates common
    species and prints them to a file seperated by '$' symbol. *)

let a = Alphabet.nucleotides

let remove_gaps s2' =
    let remove_gaps gap seq base = 
        if base <> gap then 
            let _ = Sequence.prepend seq base in
            seq
        else seq
    in
    let res = 
        Sequence.fold_right 
            (remove_gaps (Alphabet.get_gap a))
            (Sequence.create (Sequence.length s2'))
            (s2')
    in
    res



let load_file file (acc: (Sequence.s list * Parser.taxon) list) = 
    (* let kind = Parser.test_file file in *)
    let datas : (Sequence.s list list list * Parser.taxon) list = 
        Parser.Fasta.of_file Parser.Nucleic_Acids (`Local file)
    in
    List.fold_left
        (fun new_acc (sss,taxon_name) ->
            let flat_s = (* list list list *)
                List.fold_left 
                    (fun ps ss -> (* list list *)
                        List.fold_left
                            (fun ps s -> (* list *)
                                Sequence.concat (ps::s))
                            ps ss)
                    (Sequence.make_empty a)
                    sss
            in
            let added = ref false in
            let data = 
                List.map
                    (fun ((acc_ss,acc_taxon) as acc) ->
                        if acc_taxon = taxon_name then begin
                            added := true;
                            (flat_s::acc_ss,acc_taxon)
                        end else
                            acc)
                    new_acc
            in
            if not !added then (([flat_s],taxon_name)::new_acc)
            else data)
        acc
        datas

let () =
    let seperated: string = Sys.argv.(1) in
    let _,files =
        Array.fold_left
            (fun (i,acc) (name) ->
                if i < 2 then (i+1,acc)
                else (i+1, (acc) @ (PoyParser.explode_filenames [(`Local name)])))
            (0,[])
            Sys.argv
    in
    Printf.printf "Found %d files\n" (List.length files);
    let data = 
        List.fold_left
            (fun datas filename -> load_file filename datas)
            []
            files
    in
    Printf.printf "Found %d taxa\n" (List.length data);
    List.iter
        (fun (ss,t) -> 
            Printf.printf "Writing: %s\n" (t^".fasta");
            let out = open_out (t^".fasta") in
            output_string out (">"^t^"\n");
            let first = ref true in
            List.iter
                (fun s ->
                    let s = remove_gaps s in
                    let () = 
                        if !first then first := false 
                        else output_string out seperated
                    in
                    Sequence.print out s a)
                ss;
            close_out out)
        data
