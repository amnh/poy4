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

let () = SadmanOutput.register "Arguments" "$Revision: 1800 $"

let just_exit = ref false
let dump_file = ref "ft_output.poy"
let input : string list ref = ref []

let parse_list = [
    ("-e", Arg.Unit (fun () -> just_exit := true), "Exit upon error");
    ("-d", Arg.String (fun str -> dump_file := str), "Dump filename in case of \
    error");
    ("-no-output-xml", Arg.Unit (fun () -> SadmanOutput.do_sadman := false),
    "Do not generate the output.xml file.")
]

let anon_fun str =
    input := str :: !input

let usage = 
    "poy [OPTIONS] filename ..."
