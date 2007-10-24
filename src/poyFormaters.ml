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

let () = SadmanOutput.register "PoyFormaters" "$Revision: 2399 $"

exception Illegal_formater of string


let is_xml_filename filename =
    match filename with 
    | None -> false
    | Some filename -> Filename.check_suffix filename ".xml" 

let sort_matrix mtx = 
    let splitter str = 
        match Str.split (Str.regexp ":") str with
        | [_] | [] -> str, ""
        | lst ->
                let rec last = function
                    | [h] -> [], h
                    | h :: t -> 
                            let a, b = last t in
                            h :: a, b
                    | [] -> 
                            failwith "this case was already filtered"
                in
                let a, b = last lst in 
                String.concat ":" a, b
    in
    let comparator a b =
        let _, r = 
            Array.fold_left (fun (p, c) v -> 
                if c <> 0 then (p + 1, c)
                else 
                    let vpre, vpost = splitter v 
                    and bpre, bpost = splitter b.(p) in
                    let comparison = 
                        let init = String.compare vpre bpre in
                        if  0 <> init then init
                        else 
                            try 
                                let v = int_of_string vpost
                                and b = int_of_string bpost in
                                v - b
                            with
                            | _ -> 0
                    in
                    (p + 1, comparison)) (0, 0) a
        in
        r
    in
    Array.iteri (fun x str -> mtx.(0).(x) <- " " ^ str) mtx.(0);
    Array.sort comparator mtx

let build_contents_row ((_, attributes, contents) : Tags.output)  =
    let mapper (_, value) = StatusCommon.escape value in
    match contents with
    | `Structured _ -> (* We will ignore the structured contents for a table *)
            let res = Array.of_list attributes in
            Array.map mapper res
    | `String v ->
            let res = 
                Array.of_list (List.rev ( (v, v) :: (List.rev attributes)))
            in
            Array.map mapper res

let build_names_row ?(contents_name = " ") (_, attributes, _) =
    let mapper (name, _) = "@{<b>" ^ name ^ "@}" in
    let res = 
        Array.of_list (List.rev ((contents_name, contents_name) :: 
            (List.rev attributes))) 
    in
    Array.map mapper res

type t = [ `String of string | `Structured of Tags.output Sexpr.t ]

let build_set (contents : t) = 
    let res = Buffer.create 16 in
    let rec add_items (_, _, x) =
        match x with
        | `String v -> 
                Buffer.add_string res v;
                Buffer.add_string res ", ";
        | `Structured x ->
                Sexpr.leaf_iter add_items x
    in
    let _ =
        match contents with
        | `Structured `Empty -> Buffer.add_string res " ";
        | `Structured todo ->
                Buffer.add_string res "{";
                Sexpr.leaf_iter add_items todo;
                Buffer.add_string res "}";
        | `String v -> 
                Buffer.add_string res v
    in
    (Buffer.contents res)

let build_table_with_contents_as_set ?(cn = " ") (lst :
    Tags.output list) = 
    let mapper ((a, b, contents) : Tags.output) : Tags.output = 
        (a, b, `String (build_set contents))
    in
    match lst with
    | h :: _ ->
            let res = List.map (function x -> build_contents_row (mapper x)) lst 
            and names = build_names_row ~contents_name:cn h in
            Array.of_list (names :: res)
    | [] -> [|[||]|]

let character_type_as_attribute (tag, attributes, contents) =
    let t = "Type" in
    let attr = 
        if tag = Tags.Characters.additive then 
            "Additive"
        else if tag = Tags.Characters.nonadditive then 
            "Non Additive"
        else if tag = Tags.Characters.sankoff then 
            "Sankoff"
        else if tag = Tags.Characters.molecular then 
            "Molecular"
        else failwith "Unexpected"
    in
    (tag, ((t, attr) :: attributes), contents)

let filter_tag tag (item : Tags.output) : Tags.output list =
    let rec build acc ((mytag, _, contents) as item) =
        let nacc = 
            if tag  = mytag then 
                item :: acc
            else acc
        in
        match contents with
        | `Structured x ->
                Sexpr.fold_left build nacc x
        | `String _ -> nacc
    in
    List.rev (build [] item)

let rec build_values_as_list st (_, _, contents) =
    match contents with
    | `Structured x -> Sexpr.leaf_iter (build_values_as_list st) x
    | `String x -> 
            Status.user_message st (StatusCommon.escape x);
            Status.user_message st ";@ "
    
let output_rows st matrix = 
    Status.user_message st (string_of_int ((Array.length matrix) - 1))

let output_characters st (characters : Tags.output)=
    let output_table_of_list title cn (lst : Tags.output list) =
        let matrix = build_table_with_contents_as_set ~cn:cn lst in
        sort_matrix matrix;
        Status.user_message st ("@[<v 4>@{<b>" ^ title ^ 
        "@}@\n@[<v 0>@\n@{<u>Total@} ");
        output_rows st matrix;
        Status.user_message st "@\n";
        Status.output_table st matrix;
        Status.user_message st "@]@]@\n"
    in
    (* For each tag we will output the appropriate character *)
    let additive = filter_tag Tags.Characters.additive characters
    and nonadditive = filter_tag Tags.Characters.nonadditive characters
    and sankoff = filter_tag Tags.Characters.sankoff characters
    and molecular = filter_tag Tags.Characters.molecular characters in
    Status.user_message st "@[<v 4>@{<b>Characters@}@\n@[<v 0>";
    output_table_of_list "Non Additive" "Set" nonadditive;
    output_table_of_list "Additive" "Range" additive;
    output_table_of_list "Sankoff" "Set" sankoff;
    output_table_of_list "Molecular" " " molecular;
    Status.user_message st "@]@]@\n"

let output_taxa st (_, _, taxa) =
    match taxa with
    | `Structured x ->
            let lst = Sexpr.to_list x in
            let mtx = build_table_with_contents_as_set lst in
            sort_matrix mtx;
            Status.user_message st 
            "@[<v 4>@{<b>Taxa@}@\n@[<v 0>@\n@{<u>Total@} ";
            output_rows st mtx;
            Status.user_message st "@\n";
            Status.output_table st mtx;
            Status.user_message st "@]@]@\n"
    | `String _ -> ()

(* [output_files st c] outputs the list of files from the output c as a list of
* elements separated by semicolon. All the contents, all the values are
* converted into a list. *)
let output_list title st c =
    Status.user_message st ("@[<v 4>@{<b>" ^ title ^ "@}@\n@[");
    build_values_as_list st c;
    Status.user_message st "@]@]@\n"

(* This is a general purpose formatter for attributes.*)
let format_attributes st attributes = 
    let format_attribute (a, b) =
        Status.user_message st "@[";
        Status.user_message st (a);
        Status.user_message st " : ";
        Status.user_message st (b);
        Status.user_message st "@]@\n";
    in
    Status.user_message st "@[<v 0>@\n";
    List.iter format_attribute attributes;
    Status.user_message st "@]"

let rec aux_data_to_status st ((tag, attributes, contents) as c : Tags.output) =
    if tag = Tags.Data.characters then output_characters st c
    else if tag = Tags.Data.taxa then output_taxa st c
    (*
    else if tag = Tags.Data.files then output_list "Files" st c
    *)
    else if tag = Tags.Data.ignored_taxa then output_list "Ignored taxa" st c
    else if tag = Tags.Data.ignored_characters then output_list "Ignored characters" st c
    (* TODO else if tag = Tags.Data.synonyms then output_synonyms st c *)
    else begin
        let str = 
            match contents with
            | `Structured _ ->  "@[<v 4>@ "
            | `String _ -> "@[@ "
        in
        Status.user_message st "@{<b>";
        Status.user_message st tag;
        Status.user_message st "@}";
        Status.user_message st str;
        Status.user_message st "@\n";
        format_attributes st attributes;
        Status.user_message st "@ ";
        begin match contents with
        | `Structured sexpr ->
                Sexpr.leaf_iter (aux_data_to_status st) sexpr
        | `String value ->
                let value = StatusCommon.escape value in
                Status.user_message st value;
        end;
        Status.user_message st "@]@\n@]@\n"
    end

(* [data_to_status filename tag] outputs the contents of Data.to_formatter in a table
* format presented in the ncurses and non-ncurses interface. *)
let data_to_status filename tag =
    StatusCommon.Files.set_margin filename 0;
    let st = Status.Output (filename, false, []) in
    if is_xml_filename filename then
        Tags.to_xml (Status.user_message st) tag
    else begin
        aux_data_to_status st tag;
        Status.user_message st "%!"
    end

let get_name_class_and_cost attr = 
    let get_them ((a, cclass, b) as acc) (tag, value) =
        if tag = Tags.Characters.name then
            (Some value), cclass, b
        else if tag = Tags.Characters.cclass then 
            a, (Some value), b
        else if tag = Tags.Characters.cost then
            a, cclass, (Some value)
        else acc
    in
    match List.fold_left get_them (None, None, None) attr with
    | Some name, Some cclass, Some cost -> name, cclass, cost 
    | _, _, _ -> raise (Illegal_formater "get_name_class_and_cost")

let get_recost attr = 
    try
        let (_, recost) = 
            List.find (fun (tag, _) -> tag = Tags.Characters.recost) attr
        in
        recost
    with Not_found -> raise (Illegal_formater "No rearrangement cost")


let get_ref_code attr = 
    try
        let (_, ref_code) = 
            List.find (fun (tag, _) -> tag = Tags.Characters.ref_code) attr
        in
        ref_code
    with Not_found -> raise (Illegal_formater "No reference code")


let get_map attr = "Available only in XML format"


let min_and_max ((x, y) as acc) (a, _, c) = 
    match c with
    | `String c ->
            let c = StatusCommon.escape c in
            if a = Tags.Characters.min then
                (Some (c), y)
            else if a = Tags.Characters.max then
                (x, Some (c))
            else acc
    | `Structured _ ->
            raise (Illegal_formater "min_and_max")

let addcs_to_formater (tag, attr, cont) = 
    if tag = Tags.Characters.additive then begin
        let name, cclass, cost = get_name_class_and_cost attr in
        let recost = "0." in 
        let chrom_ref = "-" in 
        let map = "-" in 
        let minmax = 
            match cont with
            | `Structured x -> Sexpr.fold_left min_and_max (None, None) x
            | `String _ -> raise (Illegal_formater "addcs_to_formater")
        in
        match minmax with
        | Some min, Some max -> 
                let c = Buffer.create 10 in
                Buffer.add_string c "[";
                Buffer.add_string c min;
                Buffer.add_string c ", ";
                Buffer.add_string c max;
                Buffer.add_string c "]";
                [|name; cclass; cost; recost; chrom_ref; map; Buffer.contents c|]
        | _ -> raise (Illegal_formater "addcs_to_formater 2")
    end else raise (Illegal_formater "addcs_to_formater 3")

let nonaddcs_to_formater (tag, attr, cont) =
    if tag = Tags.Characters.nonadditive then begin
        let (_, name) = 
            List.find (fun (a, _) -> a = Tags.Characters.name) attr
        in
        let (_, cclass) = 
            List.find (fun (a, _) -> a = Tags.Characters.cclass) attr
        in
        let (_, cost) =
            try List.find (fun (a, _) -> a = Tags.Characters.cost) attr 
            with Not_found -> 
                let str = " " in
                str, str
        in
        let recost = "0." in 
        let chrom_ref = "0." in 
        let map = "-" in 
        let states = 
            match cont with
            | `Structured x -> (* This is what we expect *)
                    let c = Buffer.create 10 in
                    Buffer.add_string c "{";
                    Sexpr.leaf_iter (fun (_, _, x) ->
                        match x with
                        | `String x -> 
                                Buffer.add_string c x;
                                Buffer.add_string c ",";
                        | `Structured _ ->
                                raise (Illegal_formater "nonaddcs_to_formater"))
                        x;
                    Buffer.add_string c "}";
                    StatusCommon.escape (Buffer.contents c)
            | `String _ -> raise (Illegal_formater "nonaddcs_to_formater 2")
        in


        [| name; cclass; cost; recost; chrom_ref; map; states |]
    end else raise (Illegal_formater "nonaddcs_to_formater")


let sankcs_to_formater (tag, attr, cont) =
    if tag = Tags.Characters.sankoff then
        nonaddcs_to_formater (Tags.Characters.nonadditive, attr,cont)
    else raise (Illegal_formater "sankcs_to_formater")

let seq_to_formater ((tag, attr, cont) : Tags.output) =
    if tag = Tags.Characters.sequence then begin
        let name, cclass, cost = get_name_class_and_cost attr in
        let chrom_ref = "-" in 
        let recost = "0." in 
        let map = "-" in 
        match cont with
        | `String v ->
                let v = StatusCommon.escape v in
                [|name; cclass; cost; recost; chrom_ref; map; v|]
        | `Structured _ -> 
                raise (Illegal_formater "seq_to_formater")
    end else raise (Illegal_formater ("seq_to_formater"))


let breakinv_to_formater ((tag, attr, cont) : Tags.output) =
    if tag = Tags.Characters.breakinv then begin
        let name, cclass, cost = get_name_class_and_cost attr in
        let breakinv_ref = "-" in 
        let recost = get_recost attr in 
        let map = "-" in 

        let _, _, cont = match cont with 
        | `Structured cont -> List.hd (List.rev (Sexpr.to_list cont)) 
        | `String _  -> "", [], cont
        in 

        match cont with
        | `String v ->
                let v = StatusCommon.escape v in
                [|name; cclass; cost; recost; breakinv_ref; map; v|]
        | `Structured _ -> 
                raise (Illegal_formater "breakinv_to_formater")
    end else raise (Illegal_formater ("breakinv_to_formater"))


let chrom_to_formater ((tag, attr, cont) : Tags.output) =
    if tag = Tags.Characters.chromosome then begin
        let name, cclass, cost = get_name_class_and_cost attr in
        let recost = get_recost attr in 
        let chrom_ref = get_ref_code attr in 
        let map = get_map attr in 


        let _, _, cont = match cont with 
        | `Structured cont -> List.hd (List.rev (Sexpr.to_list cont)) 
        | `String _  -> "", [], cont
        in 

        match cont with
        | `String v ->
                let v = StatusCommon.escape v in
                [|name; cclass; cost; recost; chrom_ref; map; v|]
        | `Structured _ -> 
                raise (Illegal_formater "chrom_to_formater")
    end else raise (Illegal_formater ("chrom_to_formater"))


let genome_to_formater ((tag, attr, cont) : Tags.output) =
    if tag = Tags.Characters.genome then begin
        let name, cclass, cost = get_name_class_and_cost attr in
        let recost = get_recost attr in 
        let genome_ref = get_ref_code attr in 
        let map = get_map attr in 


        let _, _, cont = match cont with 
        | `Structured cont -> List.hd (List.rev (Sexpr.to_list cont)) 
        | `String _  -> "", [], cont
        in 

        match cont with
        | `String v ->
                let v = StatusCommon.escape v in
                [|name; cclass; cost; recost; genome_ref; map; v|]
        | `Structured _ -> 
                raise (Illegal_formater "genome_to_formater")
    end else raise (Illegal_formater ("genome_to_formater"))


let annchrom_to_formater ((tag, attr, cont) : Tags.output) =
    if tag = Tags.Characters.annchrom then begin
        let name, cclass, cost = get_name_class_and_cost attr in
        let recost = get_recost attr in 
        let chrom_ref = get_ref_code attr in
        let map = get_map attr in 

        let _, _, cont = match cont with 
        | `Structured cont -> List.hd (List.rev (Sexpr.to_list cont)) 
        | `String _  -> "", [], cont
        in 

        match cont with
        | `String v ->
                let v = StatusCommon.escape v in
                [|name; cclass; cost; recost; chrom_ref; map; v|]
        | `Structured _ -> 
                raise (Illegal_formater "annchrom_to_formater")
    end else raise (Illegal_formater ("annchrom_to_formater"))


let node_character_to_formater ((tag, _, _) as v) =
    if tag = Tags.Characters.sequence then seq_to_formater v
    else if tag = Tags.Characters.chromosome then chrom_to_formater v
    else if tag = Tags.Characters.genome then genome_to_formater v
    else if tag = Tags.Characters.breakinv then breakinv_to_formater v
    else if tag = Tags.Characters.annchrom then annchrom_to_formater v
    else if tag = Tags.Characters.sankoff then sankcs_to_formater v
    else if tag = Tags.Characters.nonadditive then nonaddcs_to_formater v
    else if tag = Tags.Characters.additive then addcs_to_formater v
    else raise (Illegal_formater "node_character_to_formater")

let node_to_formater st ((tag, attr, cont) : Tags.output) =
    if tag = Tags.Nodes.node then begin
        let (_, name) = 
            List.find (fun (a, _) -> a = Tags.Characters.name) attr
        and (_, child1_name) = 
            List.find (fun (a, _) -> a = Tags.Nodes.child1_name) attr
        and (_, child2_name) = 
            List.find (fun (a, _) -> a = Tags.Nodes.child2_name) attr
        in
        let (_, cost) =
            try List.find (fun (a, _) -> a = Tags.Characters.cost) attr 
            with Not_found -> " ", " "
        in
        let (_, recost) =
            try List.find (fun (a, _) -> a = Tags.Characters.recost) attr 
            with Not_found -> " ", " "
        in
        let lst = 
            match cont with
            | `Structured x -> Sexpr.to_list x
            | `String _ -> raise (Illegal_formater "node_to_formater 2")
        in
        let lst = List.map node_character_to_formater lst in
        let lst = [|"@{<u>Characters@}"; "@{<u>Class@}"; "@{<u>Cost@}"; "@{<u>Rearrangement Cost@}"; "@{<u>Chrom Ref@}"; "@{<u>Median Map@}"; "@{<u>States@}"|] :: lst
        in
        Status.user_message st ("@\n@\n@[<v 0>@{<b>" ^ name ^ "@}@\n");
        Status.user_message st ("@[<v 0>@{<u>Cost " ^ cost ^ "@}@\n");
        Status.user_message st ("@[<v 0>@{<u>Rearrangement cost " ^ recost ^ "@}@\n");
        Status.user_message st ("@[<v 0>@{<u>Children: " ^ child1_name ^
        " " ^ child2_name ^ "@}@\n");
        Status.output_table st (Array.of_list lst);
        Status.user_message st ("@\n");
        Status.user_message st "@]@]";

    end else raise (Illegal_formater "node_to_formater")

let forest_to_formater st ((tag, attr, cont) as v) =
    let (_, cost) = 
        try List.find (fun (a, _) -> a = Tags.Characters.cost) attr 
        with Not_found -> " ", " "
    in
    let (_, recost) = 
        try List.find (fun (a, _) -> a = Tags.Characters.recost) attr 
        with Not_found -> " ", " "
    in
    match cont with
    | `Structured x ->
            let rec do_nodes ((tag, _, cont) as x) =
                if tag = Tags.Nodes.node then node_to_formater st x
                else 
                    match cont with
                    | `Structured v -> Sexpr.leaf_iter do_nodes v
                    | `String _ -> ()
            in
            Status.user_message st ("@[<v 4>@{<b>Tree@}@\n@{<u>Tree cost: @}");
            Status.user_message st (cost ^ "@\n");
            Status.user_message st ("@{<u>Tree rearrangement cost: @}");
            Status.user_message st (recost ^ "@\n@\n");
            do_nodes v;
            Status.user_message st ("@]");
    | `String _ -> 
            raise (Illegal_formater "forest_to_formater")

let trees_to_formater st ((tag, _, _) as r) =
    if tag = Tags.Trees.forest || tag = Tags.Trees.tree then
        forest_to_formater st r
    else raise (Illegal_formater "trees_to_formater")


let trees_to_formater filename fo_ls tree = 
    StatusCommon.Files.set_margin filename 0;
    let st = Status.Output (filename, false, fo_ls) in
    if is_xml_filename filename then
        Tags.to_xml (Status.user_message st) tree
    else trees_to_formater st tree
