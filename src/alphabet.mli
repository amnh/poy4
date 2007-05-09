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

(* $Id: alphabet.mli 1644 2007-02-14 19:05:47Z andres $ *)
(* Alphabet.
*
* Description and handling of different kinds of alphabets for the analysis of
* molecular data. *)

(** Alphabet specification for sequences.
*
* Sequences are usually composed of nucleotides or aminoacides. Other kind of
* sequences are valid though. This library provides the interface for such
* specifications, as well as the default ones (nucleotides and aminoacids). *)

(** {2 Exceptions} *)

exception Illegal_Character of string
exception Illegal_Code of int

(** {2 Types} *)

(** Alphabet type *)
type a

(** {2 Alphabets} *)

(** [dna] contains codes for A, G, C, and T *)
val dna : a

(** [nucleotides] contains codes for A, C, G, and T, and all their possible
* combinations, including gaps, represented as "_". The alphabet used follows
* the IUPAC specification. *)
val nucleotides : a

(* [nucleotides_annotated] in addition to the elements of [nucleotides] contains
* the symbol for the pipe '|' which separates annotated genes within a sequence.
* *)
val nucleotides_annotated : a

(** [aminoacids] contains aminoacids alphabet according to the IUPAC.
* Combinations are not considered in this alphabet. *)
val aminoacids : a

(** [of_string l] creates an encoding for the string list [l] to produce an 
* alphabet. *)
val of_string : ?orientation:bool -> string list -> string -> string -> a

(** {2 Finding} *)

(** [match_base b a] finds the code assigned in an alphabet [a] to an element
* [b]. If the element is not found raises an [Illegal_Character] exception. *)
val match_base : string -> a -> int

val gap : int

(** Same as [match_base] *)
val find_base : string -> a -> int

(** [match_base c a] finds the string representation of code [c] in the alphabet
* specification [a]. If not found raises an [Illegal_Code] exception. *)
val match_code : int -> a -> string 

(** Same as match_code *)
val find_code : int -> a -> string

(** [rnd a] creates a function that generates random elements in the alphabet
* [a] *)
val rnd : a -> (unit -> int)

(** {2 Alphabets Properties} *)

(** [size a] gets the size of the alphabet [a]. *)
val size : a -> int


val get_all : a -> int
val get_gap : a -> int


val to_list : a -> (string * int) list

module Lexer : sig
    (* The returned list is in the inverse order of the stream. It is done like
    * this because the sequences are loaded by prepending, so there is no need
    * to hit the performance by keeping the order of the sequences being read.
    * The boolean argument is used to issue error warnings (true) or not
    * (false). *)
    val make_lexer : 
        bool -> a -> (char Stream.t -> int list -> int -> int list * int)
end

val print : a -> unit

val list_to_a :
  (All_sets.StringMap.key * All_sets.IntegerMap.key) list ->
  All_sets.StringMap.key -> All_sets.StringMap.key -> a

