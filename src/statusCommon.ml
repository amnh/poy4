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

let () = SadmanOutput.register "StatusCommon" "$Revision: 1875 $"

(* The common files for all the status interfaces. *)

external string_to_format : string -> ('a, 'b, 'c) format = "%identity"

type formatter_output = | Margin of int

module Files = struct

    (* A module to handle filenames *)
    let hd_previous_results = ref []
    let tl_previous_results = ref []
    let prefix = ref ""

    let is_prefix x y =
        (String.length x <= String.length y) &&
        (x = (String.sub y 0 (String.length x)))

    let dirname str = 
        let _ =
            match Sys.os_type with
            | "Win32" ->
                    begin match Str.split (Str.regexp ":\\\\") str with
                    | h :: t -> prefix := h ^ ":\\\\"
                    | [] -> prefix := ""
                    end
            | _ -> ()
        in
        Filename.dirname str

    let update_hd_tl_for_prefix str =
        let dir = dirname str in
        let filename = Filename.basename str in
        let prefixes = List.filter (is_prefix filename) (Array.to_list
        (Sys.readdir dir)) in
        let lst = List.sort String.compare prefixes in
        tl_previous_results := lst

    let rec find_next_match str = 
        match !tl_previous_results with
        | h :: t -> 
                hd_previous_results := h :: !hd_previous_results;
                tl_previous_results := t;
                h
        | [] -> 
                tl_previous_results := List.rev !hd_previous_results;
                hd_previous_results := [];
                ""

    (* An implementation of complete_filename in OCaml that matches the behavior
    * of readline's function. In this way we simplify possible interoperability
    * between interfaces. *)
    let complete_filename str state =
        if state = 0 then update_hd_tl_for_prefix str;
        find_next_match str
        let counter = ref 0
        let last_filename = ref ""

    (* If the [str] is the same as the previous request, the state remains 1
    * otherwise 0 *)
    let complete_filename str = 
        if str = !last_filename then 
            counter := 1
        else counter := 0;
        last_filename := str;
        !prefix ^ complete_filename str !counter


    let opened_files = Hashtbl.create 7

    let close_all_opened_files () =
        let closer _ (ch, _) =
            close_out ch
        in
        Hashtbl.iter closer opened_files 

    let assign_formatter_output f fo_ls = 
        List.iter (fun fo -> 
                       match fo with
                       | Margin m -> 
                             Format.pp_set_margin f m
                  ) fo_ls

    let get_margin filename =         
        match filename with
        | None -> Format.get_margin ()
        | Some filename ->
              try  
                  let _, fo =  Hashtbl.find opened_files filename in   
                  Format.pp_get_margin  fo ()   
              with  Not_found -> Format.get_margin () 



    let set_margin filename margin = 
        match filename with
        | None -> ()
        | Some filename ->
              try 
                  let _, fo = Hashtbl.find opened_files filename in  
                  Format.pp_set_margin fo margin 
              with Not_found -> ()


    let openf ?(mode = `Append) name fo_ls = 
        if Hashtbl.mem opened_files name then 
            let _, f = Hashtbl.find opened_files name in 
            let _ = assign_formatter_output f fo_ls in
            f
        else 
            (let file_options = 
                match mode with
                | `Append ->
                        [Pervasives.Open_wronly; Pervasives.Open_append;
                        Pervasives.Open_creat; Pervasives.Open_text] 
                | `New ->
                        [Pervasives.Open_wronly; Pervasives.Open_trunc;
                        Pervasives.Open_creat; Pervasives.Open_text] 
            in
            let ch = open_out_gen file_options 0o644 name in
            let f = Format.formatter_of_out_channel ch in
            assign_formatter_output f fo_ls;
            Hashtbl.add opened_files name (ch, f);
            f)

    let flush () = 
        Hashtbl.iter (fun _ (ch, f) -> 
            Format.pp_print_flush f ();
            flush ch) opened_files 


    let closef name () =
        if Hashtbl.mem opened_files name then
            (let ch, _ = Hashtbl.find opened_files name in
            Hashtbl.remove opened_files name;
            close_out ch)
        else ()

    let _ = 
        at_exit close_all_opened_files;
end

module Tables = struct

    let generate_string row_num item x = 
        let tag = 
            if 0 = item mod 2 then
                if 0 = row_num mod 2 then
                    "@{"
                else "@{<c:black_whit>"
            else 
                if 0 = row_num mod 2 then
                    "@{<c:red>"
                else "@{<c:red_whit>"
        in
        let x = if x = "" then " " else x in
        string_to_format (tag ^ x ^ "@}")

    let output_row row_num f row before after =
        Array.iteri (fun col x ->
            before ();
            let string = 
                if 0 = row_num mod 2 then string_to_format "@{" 
                else string_to_format "@{<c:black_whit>"
            in
            Format.fprintf f string;
            (Format.fprintf f (generate_string row_num col x) : unit);
            Format.fprintf f "@}";
            after ()) row

    (* Formatting and outputing tables *)
    let output f do_close closer v = 
        (* We need to set the tabs first *)
        let widths = Array.create (Array.length v.(0)) 0 in
        Array.iter (Array.iteri (fun p x -> 
            widths.(p) <- max (String.length x) widths.(p))) v;
        let first_row = Array.map (fun x -> String.make (x + 1) ' ') widths in
        Format.pp_open_tbox f ();
        output_row 0 f first_row (Format.pp_set_tab f) (fun () -> ());
        Format.pp_force_newline f ();
        Array.iteri (fun item x -> 
            output_row item f x (Format.pp_print_tab f) (fun () -> ());
        ) v;
        Format.pp_close_tbox f ();
        if do_close then closer ();

end

let information_output = ref None

let set_information_output (filename : string option) =
    information_output := filename

let redirect_information () = 
    match !information_output with 
    | None -> false
    | Some _ -> true

let information_redirected () = !information_output

let redirect_filename () =
    match !information_output with
    | None -> failwith "No redirection"
    | Some f -> f

let output_status_to_formatter formatter maximum achieved header suffix =
    let output_to_formatter formatter =
        match maximum, achieved with
        | None, 0 ->
                Format.fprintf formatter "@[%s\t@;@[%s@]@ @]@." header 
                suffix
        | None, n ->
                Format.fprintf formatter
                "@[%s\t%d\t@;@[%s@]@ @]@." header achieved
                suffix
        | Some max, _ ->
                Format.fprintf formatter
                "@[%s\t%d of %d@;@[%s@]@ @]@." header achieved
                max suffix
    in
    output_to_formatter formatter;
    match information_redirected () with
    | Some filename ->
            let f = Files.openf filename [] in
            output_to_formatter f
    | None -> ()
