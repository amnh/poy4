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

(* $Id: fileStream.ml 2169 2007-09-01 22:49:51Z andres $ *)
(* Created Thu Apr 20 16:41:14 2006 (Illya Bomash) *)

(** simple input streams as objects, with helper functions for parsing *)

class type greader = object
    val mutable character_number : int
    val mutable stored : char list
    method get : char
    method get_position : int
    method getch : char
    method getch_safe : char option
    method match_prefix : string -> bool
    method putback : char -> unit
    method read_excl : char list -> string
    method read_float : float
    method read_incl : char list -> string
    method read_int : int
    method read_line : string
    method read_while : (char -> bool) -> string
    method skip_ws : unit
    method skip_ws_nl : unit
    method close_in : unit
end

(** [string_of_charlist cl] makes a string from a list of characters *)
let string_of_charlist cl =
    let len = List.length cl in
    let str = String.create len in
    ignore (List.fold_left
                (fun n elt ->
                     String.set str n elt;
                     succ n) 0 cl);
    str

(** [string_of_charlist_rev cl] makes a string from a list of characters in
    reverse order *)
let string_of_charlist_rev cl =
    let len = List.length cl in
    let str = String.create len in
    ignore (List.fold_left
                (fun n elt ->
                     String.set str n elt;
                     pred n) (pred len) cl);
    str

(** {2 Predicates} *)

(** [is_or] composes a list of predicates using "logical or" semantics.
    Intended to be used with the predicate functions listed below, and passed
    to the function [read_while]. *)
let is_or list c =
    List.exists (fun fn -> fn c) list

let is_not fn c =
    not (fn c)

(** [is_alpha c]: is [c] an English alphabet character? *)
let is_alpha = function
    | 'a' .. 'z'
    | 'A' .. 'Z' -> true
    | _ -> false
(** [is_num c]: is [c] a digit from 0 to 9? *)
let is_num = function
    | '0' .. '9' -> true
    | _ -> false
(** [is_char c c']: is it a given character? *)
let is_char c c' = c = c'

(** [is_ws_nl c]: is [c] whitespace (including newlines)? *)
let is_ws_nl = function
    | ' ' | '\t' | '\010' | '\013' -> true
    | _ -> false

let is_taxon_delimiter = function
    | '[' | ']' | '(' | ')' | ',' | ';' | ' ' | '\t' | '\010' | '\013' -> true
    | _ -> false

let is_unacceptable_in_taxon_name = function
    | '@' | '%' -> (* these break the formatter *) true
    | _ -> false

let has_condition check x = 
    try
        for i = 0 to (String.length x) - 1 do
            if check x.[i] then raise Exit;
        done;
        false
    with
    | Exit -> true

(** {2 Readers} *)

(** [reader] is a base class for reading from a stream.  It includes high-level
    functions to make parsing easier.  Specific classes to use are found
    below. *)
class virtual reader =
object (self)
    method virtual getch : char
    (** [getch] is a virtual method to return a character *)
    method virtual putback : char -> unit
    (** [putback ch] must put back a character *)

    val mutable character_number = 0
    method get_position = character_number 
    (** [get_position] returns how many characters have been read *)

    (** [read_while fn] returns the longest stream of characters [c] in the
        input for which [fn c] returns [true].  The first character returning
        [false] is kept in the stream.  If the stream is already at EOF, an
        [End_of_file] exception is thrown; otherwise, if an EOF is encountered
        in reading, the string up to EOF is returned. *)
    method read_while fn =
        let chars = ref [] in
        let char = ref self#getch in
        try
            while fn !char do
                chars := !char :: !chars;
                char := self#getch
            done;
            self#putback !char;
            string_of_charlist_rev !chars
        with End_of_file ->
            string_of_charlist_rev !chars

    (** [read_incl chars] reads the longest string consisting of characters in
        the list passed in *)
    method read_incl chars =
        self#read_while (fun char -> List.mem char chars)

    (** [read_excl chars] reads the longest string consisting of characters
        other than those in the list passed in *)
    method read_excl chars =
        self#read_while (fun char -> not (List.mem char chars))

    (** [read_line] reads up to a newline character (inclusive), then returns
        the portion before the newline *)
    method read_line =
        (* assume input is cooked (multiple newline chars replaced with single
           \n) *)
        let line = self#read_excl ['\n'; '\013'; '\010'] in
        while ( (self#match_prefix "\n") || (self#match_prefix "\013") ||
        (self#match_prefix "\010")) do
            ()
        done;
        line

    (** [skip_ws] skips past leading whitespace, but not newlines.  Raises
        [End_of_file] if already at EOF. *)
    method skip_ws =
        ignore(self#read_incl [' '; '\t'])

    (** [skip_ws_nl] skips past leading whitespace, including newlines.  Raises
        [End_of_file] if already at EOF. *)
    method skip_ws_nl =
        ignore(self#read_incl [' '; '\t'; '\010'; '\013'])

    (** [match_prefix str] examines the input stream.  If the beginning of the
        stream matches [str], it is read and discarded and the function returns
        [true].  If it doesn't match, the text is left in the stream and the
        function returns [false]. *)
    method match_prefix str =
        let len = String.length str in
        let putback = function
            | None -> ()
            | Some ch -> self#putback ch in
        let rec m i =
            if i = len
            then true
            else begin
                let ch, right_ch =
                    try
                        let ch = self#getch in
                        Some ch, ch = str.[i]
                    with End_of_file -> None, false in
                if right_ch
                then (if m (succ i)
                      then true
                      else (putback ch; false))
                else (putback ch; false)
            end
        in m 0

    (** [read_int] returns an integer at the beginning of the input.  Raises
        exceptions on initial EOF or on non-numeric input. *)
    method read_int =
        int_of_string (self#read_incl ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
                                       '8'; '9';])

    (** [read_float] returns a float at the beginning of the input.  Raises
        exceptions on initial EOF. *)
    method read_float =
        let whole = self#read_while is_num in
        if self#match_prefix "."
        then begin
                let part = self#read_while is_num in
                float_of_string (whole ^ "." ^ part)
            end
        else float_of_string whole
            
    (** [getch_safe] returns [Some ch] if a character is available, or [None]
        otherwise. *)
    method getch_safe =
        try Some self#getch
        with End_of_file -> None
end

(** [auto_store_reader] is a reader that implements a simple queue of put-back
    characters, and keeps track of the current character position *)
class virtual auto_store_reader =
object (self)
    inherit reader as super
    method virtual get : char
    val mutable stored = []
    method getch =
        match stored with
        | [] ->
                let c = self#get in
                (match c with
                | '\010' | '\013' -> ()
                | _ -> character_number <- character_number + 1);
                c
        | c :: cs ->
              stored <- cs;
                (match c with
                | '\010' | '\013' -> ()
                | _ -> character_number <- character_number + 1);
              c
    method putback c =
        (match c with
        | '\010' | '\013' -> ()
        | _ -> character_number <- character_number - 1);
        stored <- c :: stored
end

(** [stream_reader stream] constructs a stream reader object from an input
    stream *)
class stream_reader stream =
object (self)
    inherit auto_store_reader as super

    method private get_char = Pervasives.input_char stream
        
    (* By default, we want to skip extra newline chars *)
    val mutable last_cr = false
    method private process_char ch =
        if ((ch = '\010') || (ch = '\013') || (ch = '\n')) && last_cr then 
            (last_cr <- false; self#get)
        else if ch = '\013' || ch = '\010' || ch = '\n' then 
            (last_cr <- true; '\n')
        else (last_cr <- false; ch)
    method get =
        let ch = self#get_char in
        self#process_char ch
end

class compressed_reader stream = object (self)
    inherit stream_reader stream as super

    val mutable table = Lz.initial_table ()

    val mutable buffer = Buffer.create (1024 * 1024) 

    val mutable buffer_length = 0
    val mutable buffer_position = 0

    method private fill_buffer =
        Buffer.reset buffer;
        buffer_length <- 0;
        buffer_position <- 0;
        let fst = Pervasives.input_byte stream in
        let snd = Pervasives.input_byte stream in
        let int = (fst lsl 8) lor snd in
        Lz.decompress table [int] buffer;
        buffer_length <- Buffer.length buffer;

    method private process_char ch =
        if ((ch = '\010') || (ch = '\013') || (ch = '\n')) && last_cr then 
            (last_cr <- false; self#get)
        else if ch = '\013' || ch = '\010' || ch = '\n' then 
            (last_cr <- true; '\n')
        else (last_cr <- false; ch)

    method get =
        if buffer_length = 0 then begin
            self#fill_buffer;
            self#get
        end else begin
            buffer_length <- buffer_length - 1;
            let ch = Buffer.nth buffer buffer_position in
            buffer_position <- buffer_position + 1;
            self#process_char ch
        end
end

type f = [`Local of string | `Remote of string]

let read_contents file = 
    let b = Buffer.create (1024 * 1024) in
    let ch = open_in file in
    try
        while true do
            Buffer.add_char b (input_char ch);
        done;
        b
    with
    | End_of_file -> b

let filename = function
    | `Local f 
    | `Remote f -> f

let open_in fn = 
    StatusCommon.Files.flush ();
    match fn with
    | `Local file -> open_in file
    | `Remote file ->
IFDEF USENOSHAREDHD THEN
            let rank = Mpi.comm_rank Mpi.comm_world in
            let tmp = 
                Filename.temp_file "POY" (".input_" ^ string_of_int rank) 
            in
            let output = open_out tmp in
            let contents =
                if 0 = Mpi.comm_rank Mpi.comm_world then begin
                    read_contents file
                end else Buffer.create 1
            in
            let file = Mpi.broadcast contents 0 Mpi.comm_world in
            Buffer.output_buffer output file; 
            flush output;
            close_out output;
            open_in tmp
ELSE
            open_in file
END

let channel_n_filename fn = 
    open_in fn, filename fn

class file_reader file =
    let stream = open_in file in
object (self)
    inherit stream_reader stream

    method close_in = Pervasives.close_in stream
end

(** [string_reader string] constructs a stream reader object from a string *)
class string_reader string =
    let len = String.length string in
object (self)
    inherit auto_store_reader as super
    val mutable index = 0
    method get_position = index
    method get =
        if index = len
        then raise End_of_file
        else begin
            let ch = string.[index] in
            index <- succ index;
            ch
        end
end


module Pervasives = struct
    let open_in fn =
        new file_reader fn
    let input_line r =
        r#read_line
    let close_in stream = stream#close_in
end
