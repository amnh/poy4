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

(** Sequence
 *
 * Molecular sequence representation and handling *)

exception Invalid_Argument of string;;
exception Invalid_Sequence of (string * string * int);;
(** An invalid sequence was found. The contents of the exception is a triple
 * [(x, y, z)] where [x] is the sequence where the error was found, [y] is the
 * illegal character in the sequence and [z] is the position where the parsing
 * process stopped. *)

type s
(** The sequence type *)

val reverse : s -> s
(** [reverse s1] returns a reversed fresh copy of s1. *)

val safe_reverse : s -> s

val reverse_ip : s -> unit
(** [reverse_ip s1] reverse in place s1. *)

val capacity : s -> int
(** [capacity s1] returns the capacity of the sequence [s1]. Although a sequence
 * may have a certain length, the allocated memory will have a limited capacity
 * that can be changed at runtime. The current capacity of a sequence is calculated
 * here. *)

val length : s -> int
(** [length s] returns the length of the sequence [s]. The total length must be
 * not greater than the capacity of the sequence.*)

val compare : s -> s -> int

(** [is_existed_code code seq] returns true
* if the code is in the sequence [seq], otherwise false *)
val is_existed_code : int -> s -> bool

(** [is_existed_char code seq] returns true
* if the code of character [ch] is in the sequence [seq], otherwise false *)
val is_existed_char : string -> s -> bool


(** [printDNA seq] prints the DNA sequence [seq] into stdout *)
val printDNA : s -> unit

(** [create_gap_seq gap len] create a 
* sequence of  [len] gaps *)
val create_gap_seq : ?gap:int -> int -> s

(** [cmp_num_all_DNA] returns number of
* codes in [seq] which do not include gap *)
val cmp_num_all_DNA : s -> int

(** [cmp_num_all_DNA] returns number of
* codes in [seq] which are not gap *)
val cmp_num_not_gap : s -> int

(** [cmp_gap_cost indel seq] returns the indel cost
* of sequence [seq] *)
val cmp_gap_cost : int * int -> s -> int

(** [cmp_ali_cost alied_seq1 alied_seq2 direction cost_mat]
* returns the alignment cost for alignment [alied_seq1] and [alied_seq2] *)
val cmp_ali_cost :
  s -> s -> [> `Positive ] -> Cost_matrix.Two_D.m -> int

(** [get_empty_seq] returns an empty sequence *)
val get_empty_seq : unit -> s


(** [subseq seq start len] returns a subsequence
* of sequence [seq] start from position [start] 
* for a length of [len]. If length [len] < 1, return 
* an empty sequence *)
val subseq : s -> int -> int -> s

(** [align2 seq1 seq2 cost_mat] aligns 
* two sequences [seq1], [seq2] without conditioning 
* that two first characters are gaps *)
val align2 :
  s ->
  s -> Cost_matrix.Two_D.m -> s * s * int * int


(** [align3] returns the alignment of three sequences
* [seq1], [seq2], and [seq3] without requiring 
* that three first letters of three sequences to be gaps *)
val align3 :
  s ->
  s ->
  s ->
  Cost_matrix.Three_D.m -> s * s * s * int * int




val resize : s ref -> int -> unit
(** [resize s1 v] modifies the sequence [s1] to have capacity [v]. If [v] is
 * less than the original value, the sequence is cut at the end. If the new
 * sequence is longer, the contents doesn't change, but the capacity will grow
 * (ie. the sequence printed or for alignment is still the same). *)

val clone : s -> s
(** [clone s1] creates a new fresh clone of [s1]. *)

external set : (s -> int -> int -> unit) = "seq_CAML_set"
(** [set s1 p v] sets the value v in the position p of s1. If p > (get_capacity
 * s1) raise an Invalid_Argument error. *)

val get : (s -> int -> int) 
(** [get s1 p] returns the contents of s1 in position p. If p > (get_capacity
 * s1) raise an Invalid_Argument error. *)

val prepend : s -> int -> unit
(** [prepend x y] Prepends the y to the string x, therefore, increasing its
 * length. If the capacity of the string is not enough, the program will exit with
 * an invalid assert statement. If the library was compiled without assert headers,
 * then this function will be unsafe. *)

val of_string : string -> Alphabet.a -> s
(** [of_string str alph] digests the string str using the alphabet encoding alph
 * and returns the encoded sequence s. If a character is not found, raise an
 * Invalid_Sequence exception. *)


val of_string_ls : string list -> Alphabet.a -> s
(** [of_string_ls str_ls alph] digests the list of string str_ls 
 * using the alphabet encoding alph
 * and returns the encoded sequence s. If a character is not found, raise an
 * Invalid_Sequence exception. *)

val to_string : s -> Alphabet.a -> string
(** [to_string seq alph] converts a sequence seq into a string using the
 * encoding specified in the alphabet alph. If a sequences is not found raise
 * Alphabet.Illegal_Code exception. *)

val to_formater : s -> Alphabet.a -> string
(* Same as [to_string] but with @ and % escaped for the formating functions *)

val print : Pervasives.out_channel -> s -> Alphabet.a -> unit
(** [print x y z] prints the sequence y in the channel x using alphabet z. If
 * one of the values in x is not present in the alphabet definition, an
 * Illegal_Code exception is raised. *)

val concat : s list -> s
(** [concat x] concatenates the contents of the sequences in x in a new fresh
 * sequence *)

val prepend_char : s -> int -> s
(** [prepend_element seq gap] Add a gap at the begin of seq *)

val del_first_char : s -> s
(** [del_first_char seq] Delete the first character of seq *)


val to_array : s -> int array
(** No comments *)

(** [creates l] creates a fresh array with unspecified contents of length
    [l]. *)
val create : int -> s

(** [make_empty a] creates an empty sequence using alphabet [a] *)
val make_empty : Alphabet.a -> s

(** [map f s] outputs a new sequence [t] such that [t](i) = [f]([s](i)). *)
val map : (int -> int) -> s -> s
val mapi : (int -> int -> int) -> s -> s

val fold : ('a -> int -> 'a) -> 'a -> s -> 'a

val foldi : ('a -> int -> int -> 'a) -> 'a -> s -> 'a

val fold_right : ('a -> int -> 'a) -> 'a -> s -> 'a

val fold_righti : ('a -> int -> int -> 'a) -> 'a -> s -> 'a

(** [iter f s] is equivalent to for i = 0 to length [s] do [f] ([get] [i] [s]);
 * done. *)
val iter : (int -> unit) -> s -> unit

val fold : ('a -> int -> 'a) -> 'a -> s -> 'a

(** [init f l] creates a fresh sequence [s] such that [s](i) = [f] [i]. *)
val init : (int -> int) -> int -> s

val lambda : s

val sub : s -> int -> int -> s

(** [rnd a l] creates a random sequence of length [l] over the alphabet [a] *)
(*val rnd: Alphabet.a -> int -> s*)

val gap_saturation : s -> Alphabet.a -> float

val poly_saturation : s -> int -> float

(** Sequence alignment module for two or three sequences. *)
module Align : sig
    (** [cost_2 x y z u v flag] calculates the cost of the alignment of two
     * sequences x and y using the cost matrix z in the alignment matrix u with 
     * ukkonen barrier at distance v if flag is set to true *)
    val cost_2 : ?deltaw:int -> s -> s -> Cost_matrix.Two_D.m -> Matrix.m -> int 

    val align_affine_3 : s -> s -> Cost_matrix.Two_D.m -> s * s * s * int * s

    val max_cost_2 : s -> s -> Cost_matrix.Two_D.m -> int
    val verify_cost_2 : int -> s -> s -> Cost_matrix.Two_D.m -> int
 
    external c_cost_2 :
        s -> s -> Cost_matrix.Two_D.m -> Matrix.m -> int -> int =
        "algn_CAML_simple_2";;

    (* [cost_2_limit ?w ?h x y u v st_x st_y len_x len_y] performs a pairwise
    * alignment of the subsequences of [x] and [y] starting in position [st_x]
    * and [st_y] respectively, with length [len_x] and [len_y] using the
    * transformation cost matrix [u] and the alignment matrix [v], with optional 
    * maximum width [w] and height [h]. *)
    val cost_2_limit : ?w:int -> ?h:int -> s -> s -> Cost_matrix.Two_D.m ->
        Matrix.m -> int -> int -> int -> int -> int

    val cost_2_stitched : int list -> int -> Alphabet.a -> s -> s ->
        Cost_matrix.Two_D.m -> Matrix.m -> int 

    (** Same as cost_2 but for three sequences. *) 
    external cost_3 :
        s -> s -> s -> Cost_matrix.Three_D.m -> Matrix.m -> int -> int =
        "algn_CAML_simple_3_bc" "algn_CAML_simple_3";;
    
    external myers :
        s -> s -> int = "algn_CAML_myers"
        
    (** [extract_edited_2 x y z u v w] takes two previously aligned sequences x
     * and y * (for which the cost_2 function has been called), and extracts their
     * edited version in z and u, using the alignment matrix v and the transformation
     * cost mstrix w. Make sure the v and w are the same as used in the alignment of
     * the sequence for the call of cost_2. No check of an appropriate call of cost_2
     * is made; therefore the behavior of the function in this case is undefined. *)
    external extract_edited_2 : 
        s -> s -> s -> s -> Matrix.m -> Cost_matrix.Two_D.m -> bool -> unit =
        "algn_CAML_backtrack_2d_bc" "algn_CAML_backtrack_2d";;
    
    (** Same as extract_edited_2 but for three sequences at a time *)
    external extract_edited_3 :
        s -> s -> s -> s -> s -> s -> Matrix.m -> Cost_matrix.Three_D.m -> 
            unit = "algn_CAML_backtrack_3d_bc" "algn_CAML_backtrack_3d";;

    val print_backtrack : s -> s -> Matrix.m -> unit

    val make_backtrack : s -> s -> Matrix.m -> int array array

    val count_paths : int array array -> int

    (** Same as extract_edited_2 but instead of storing the edited sequences in
     * a preexistant sequence, a fresh tuple of sequences is created *)
    val create_edited_2 : s -> s -> Matrix.m -> Cost_matrix.Two_D.m -> s * s

    val create_edited_2_limit : s -> s -> Matrix.m -> Cost_matrix.Two_D.m -> 
        int -> int -> int -> int -> s * s
        
    val create_edited_3 : 
        s -> s -> s -> Matrix.m -> Cost_matrix.Three_D.m -> s * s * s
    (** Same as [create_edited_2] but for three sequences at a time *)
        
    val align_2 : ?first_gap:bool -> s -> s -> Cost_matrix.Two_D.m -> Matrix.m -> s * s * int
    (** [align_2 s1 s2 c hash] aligns the sequences s1 and s2 using the cost
        matrix c. If first_gap is true, the first characters of s1 and s2 are gaps.
        Otherwise, we must insert a gap at the begin of s1 and s2.
        The resulting alignment is keep in the internal hash table iff
        hash. the function returns the triple [(s1', s2', cost)] which is the edited
        sequences s1 and s2 and the cost of the alignment.*)

    val align_3 :
        ?first_gap:bool -> s -> s -> s -> Cost_matrix.Three_D.m -> Matrix.m -> s * s * s * int
    (** [align_3 s1 s2 s3 c hash] does the same as align_2 but for three
        sequences. *)

    val median_2_with_gaps : s -> s -> Cost_matrix.Two_D.m -> s
    (** [median_2_with_gaps s1 s2 c] calculates a new sequence s which is the median
        between the sequences s1 and s2. Note that s1 and s2 should have the same
        length, otherwise the function raises and Invalid_Argument error *)

    val median_2 : s -> s -> Cost_matrix.Two_D.m -> s
    (** Same as [median_2_with_gaps] but removes any gap from the generated
    * median. *)

    val ancestor_2 : s -> s -> Cost_matrix.Two_D.m -> s 

    val median_3 : s -> s -> s -> Cost_matrix.Three_D.m -> s
    (** [median_3 s1 s2 s3 c] does the same as median_2 but for three
        sequences. *)

    val full_median_2 : s -> s -> Cost_matrix.Two_D.m -> Matrix.m -> s

    val full_median_3 : s -> s -> s -> Cost_matrix.Three_D.m -> Matrix.m -> s

    val union : s -> s -> s 

    val closest : s -> s -> Cost_matrix.Two_D.m -> Matrix.m -> s * int

    val recost : s -> s -> Cost_matrix.Two_D.m -> int

    (** [align_3_powell a b c m o e] generates a quadruple [(a', b', c', ed)]
     * where [a'], [b'], and [c'], are the edited sequences of [a], [b], and [c]
     * in their optimal three dimensional sequence alignment, for mismatch cost
     * [m], gap opening cost [o] and gap extension cost [e], yielding a total
     * edition cost [ed]. *)
    val align_3_powell : s -> s -> s -> int -> int -> int -> s * s * s * int 

    (** [align_3_powell_inter a b c cm cm3] generates the median and edition
    * cost between the three sequences [a], [b], and [c], according to the cost
    * matrix specified by [cm] and [cm3]. *)
    val align_3_powell_inter : s -> s -> s -> Cost_matrix.Two_D.m ->
        Cost_matrix.Three_D.m -> s * int

    (** [readjust_3d a b mine cm cm3 p] readjust [mine] as the median between
    * the sequences [a], [b] and [p]. The result is a triple [(ed, s, ch)],
    * where [ed] is the total edition cost of the median [s], which is in the
    * center of [a], [b], and [p], and [ch] is true iff [ch] is different from
    * [mine]. *)
    val readjust_3d : ?first_gap:bool -> s -> s -> s -> Cost_matrix.Two_D.m -> Cost_matrix.Three_D.m -> s
    -> int * s * bool

end

(** [select_one s m] 
 * Given a bitset of sequences [s] and a transformation cost matrix [m], select
 * a single sequence (gapless) among them and return it. *)
val select_one : s -> Cost_matrix.Two_D.m -> s

(** [select_one_randomized s cm] returns a single sequence from the bitset
* sequences [s] selecting each base uniformly at random from those initially in
* [s]. *)
val select_one_randomized : s -> Cost_matrix.Two_D.m -> s

(** [readjust ch1 ch2 m cm p] attempts to readjust the median [m] to somewhere
* in between the sequences [ch1], [ch2], and [p], according to the the
* transformation cost matrix [cm]. The function returns a tuple [(a, b)], where
* [b] is the new readjusted median, and [a] is the cost of that median as the
* parent of [ch1] and [ch2]. *)
val readjust : s -> s -> s -> Cost_matrix.Two_D.m -> s -> int * s * bool

(* A module to perform alignments in pure Ocaml for debugging and easy
* experimentation *)
module CamlAlign : sig

    (* The type of transformation events that can be performed over the
    * sequences *)
    type e = Align | Delete | Insert

    val non_linnear : 
        (int -> int -> s -> float) -> Cost_matrix.Two_D.m -> s -> s -> float

    val cost_2_of_tcm : Cost_matrix.Two_D.m -> s -> s -> float
    (* A sequence in the CamlAlign module *)
    type s = int array

    (* [align a b] aligns two sequences [a] and [b] and returns the cost
    * alignment matrix and the direction matrix containing all the possible
    * operations to be perfomed for the alignment. This initial implementation
    * assumes a transformation cost of 1 for each operation. *)
    (*val align : s -> s -> int array array * e list array array*)

    (* [backtrace m a b] returns all the possible sequences that are product of
    * aligning sequences [a] and [b] using the transformation matrix [m]. [m]
    * should be second element in the output of [align] and [a] and [b] should
    * be passed in the same order they where passed to [align]. *)
    (*val backtrace : e list array array -> s -> s -> s list*)


    val create_pair_align :
        int array ->
        int array -> int array array -> int -> int array * int array * int

    val create_triple_align :
        int array ->
        int array ->
        int array ->
        int array array -> int -> int array * int array * int array * int



    val pc : int -> int -> int -> float

end

module SeqFp :
    (** A module to generate fingerprints (hash values) of sequences. Look at
    * the {!FingerPrint} module for more information. *)
  sig
    val fp : s -> int -> int -> int -> int -> int
    val complete_fp : s -> int -> int -> int
    val fp_incremental :
      s -> int -> int -> int -> int -> int -> int
    val fp_match : s -> s -> int -> int -> int
    val do_match : s -> s -> int -> int
  end

module SeqMFp :
    (* A module to generate lists of fingerprints (hash values) of sequences,
    * rather than a single hash value, multiple values are generated to reduce
    * the probability of a collition.*)
    sig
        val fp : s -> int list -> int -> int -> int -> int list
        val complete_fp : s -> int list -> int -> int -> int list
        val fp_incremental : 
            s -> int list -> int -> int -> int list -> int -> int list
    end

module Anchors : sig
    (** Detecting anchors in pairs of sequences.
    *
    * This module detects possible anchors in a pairwise alignment, to break the
    * sequence in smaller subsequences. *)

    (* [subhash a p l n] calculates the hash value of all the subsequences of length
    * [l] in the sequence [a] using the prime [p] as hash module, where [a] is
    * defined over the alphabet [n] , using the Karp-Rabi algorithm. *)
    val subhash : s -> int -> int -> Alphabet.a -> (int * int) list

    val shared_subseqs : int -> (int * int) list -> (int * int) list -> 
        int All_sets.IntegerMap.t -> (int * int) list 

    val subhash_mult : s -> int list -> int -> Alphabet.a -> (int list * int) list

    val shared_subseqs_mult : 
        int list -> (int list * int) list -> (int list * int) list -> 
            int All_sets.IntegerListMap.t -> int -> Alphabet.a -> (int * int) list 

    val consistent_groups : (int * int) list -> ((int * int) list) list

    val consecutive_matches : (int * int) list -> ((int * int) * (int * int)) list
end

module Unions : sig

IFDEF USE_LONG_SEQUENCES THEN
    type off_type =
            (int32, Bigarray.int32_elt, Bigarray.c_layout) 
            Bigarray.Array1.t
    type int_type = int32

ELSE
    type off_type =
            (int, Bigarray.int16_signed_elt, Bigarray.c_layout) 
            Bigarray.Array1.t
    type int_type = int
END

    val to_int : int_type -> int
    type u = { 
        seq : s; 
        offset : off_type;
        union_c1 : off_type;
        union_c2 : off_type;
    }

    val leaf : s -> u

    val union : s -> s -> s -> u -> u -> Cost_matrix.Two_D.m -> u

    val get_seq : u -> s

    val get_positions : u -> (int * int) list -> 
        (int * int) list * (int * int) list 

    val compare : u -> u -> int
end

val split : (int * int) list -> s -> Alphabet.a -> s list

val count_bits : int -> int

val of_code_arr : int array -> int -> s

val is_empty : s -> int -> bool

external encoding : 
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t -> 
        s -> float = "seq_CAML_encoding"

(* [complement alph s] returns a sequence which is the reverse complement of the
* input sequence [s] as specified by the alphabet [alph]. If [alph] does not
* provide the necessary specification, an exception is raised. *)
val complement : Alphabet.a -> s -> s

(* The same as complement for sequence, but no gap is inserted at the begin*)
val complement_chrom : Alphabet.a -> s -> s

(** [is_existed_code code seq] returns true
* if the code is in the sequence [seq], otherwise false *)
val is_existed_code : int -> s -> bool

(** [is_existed_char code seq] returns true
* if the code of character [ch] is in the sequence [seq], otherwise false *)
val is_existed_char : string -> s -> bool


(** [printDNA seq] prints the DNA sequence [seq] into stdout *)
val printDNA : s -> unit

(** [create_gap_seq gap len] create a 
* sequence of  [len] gaps *)
val create_gap_seq : ?gap:int -> int -> s

(** [cmp_num_all_DNA] returns number of
* codes in [seq] which do not include gap *)
val cmp_num_all_DNA : s -> int

(** [cmp_num_all_DNA] returns number of
* codes in [seq] which are not gap *)
val cmp_num_not_gap : s -> int

(** [cmp_gap_cost indel seq] returns the indel cost
* of sequence [seq] *)
val cmp_gap_cost : int * int -> s -> int

(** [cmp_ali_cost alied_seq1 alied_seq2 direction cost_mat]
* returns the alignment cost for alignment [alied_seq1] and [alied_seq2] *)
val cmp_ali_cost :
  s -> s -> [> `Positive ] -> Cost_matrix.Two_D.m -> int

(** [get_empty_seq] returns an empty sequence *)
val get_empty_seq : unit -> s


(** [subseq seq start len] returns a subsequence
* of sequence [seq] start from position [start] 
* for a length of [len]. If length [len] < 1, return 
* an empty sequence *)
val subseq : s -> int -> int -> s

(** [align2 seq1 seq2 cost_mat] aligns 
* two sequences [seq1], [seq2] without conditioning 
* that two first characters are gaps *)
val align2 :
  s ->
  s -> Cost_matrix.Two_D.m -> s * s * int * int


(** [align3] returns the alignment of three sequences
* [seq1], [seq2], and [seq3] without requiring 
* that three first letters of three sequences to be gaps *)
val align3 :
  s ->
  s ->
  s ->
  Cost_matrix.Three_D.m -> s * s * s * int * int


(** [closest_alied_seq alied_parent alied_child c2]
* returns the single sequence of sequence [alied_child]
* which is closest to the aligned parent sequence [alied_parent] *)
val closest_alied_seq :
  s -> s -> Cost_matrix.Two_D.m -> s * int


(** [concat seq_ls] returns a concatination 
* of sequences in the sequence list [seq_ls] *)
val concat : s list -> s

(** [create_subalign2 seq1 seq2 cost_mat 
* start_pos1 end_pos1 start_pos2 end_pos2] returns
* the alignment of subsequence from [start_pos1] to [end_pos1] 
* in the first sequence [seq1] with subsequence from [start_pos2] to [end_pos2] 
* in the second sequence [seq2]  *)
val create_subalign2 :
  s ->
  s ->
  Cost_matrix.Two_D.m ->
  int -> int -> int -> int -> s * s * int

(** [dna_gap] returns the gap code in tha nucleotide alphabet *)
val dna_gap : int


(** [get_num_base seq] returns number bases, not gaps, of sequence [seq] *)
val get_num_base : s -> int

(** [delete_gap] deletes all gaps in the sequence [seq] *)
val delete_gap : ?gap_code:int -> s -> s

(** [create_median_gap seq start_pos end_pos cost_mat] 
* returns the median sequence between sequence [seq] and gaps *)
val create_median_gap :
  s ->
  ?start_pos:int -> ?end_pos:int -> Cost_matrix.Two_D.m -> s

(** [create_median_seq approx alied_seq1 alied_seq2 cost_mat]
* returns the median sequence between aligned sequence [alied_seq1] 
* and aligned sequence [alied_seq2] *)
val create_median_seq :
  ?approx:[< `BothSeq | `First | `Second > `BothSeq ] ->
  s -> s -> Cost_matrix.Two_D.m -> s * int

val create_median_deled_seq : s -> 'a -> s

(** [create_median approx seq1 seq2 s1 e1 s2 e2 cost_mat]
* returns the median sequence between subsequence
* from [s1] to [e1] in the first sequence [seq1] and subsequence 
* from [s2] tp [e2] in the second sequence [seq2] *)
val create_median :
  ?approx:[< `BothSeq | `First | `Second > `BothSeq ] ->
  s ->
  s ->
  ?s1:int ->
  ?e1:int ->
  ?s2:int ->
  ?e2:int ->
  Cost_matrix.Two_D.m -> s * s * s * int

val check_repeated_char : s -> Alphabet.a -> unit

(** [create_general_ali code1_arr code2_arr gap_code cost_mat]
* returns the general alignment between sequence [code1_arr] and 
* sequence [code2_arr] *)
val create_general_ali :
  int array ->
  int array -> int -> Cost_matrix.Two_D.m -> int array * int array * int

val map : (int -> int) -> s -> s

(** [of_array code_arr] converts the code array [code_arr]
* into a sequence *)
val of_array : int array -> s

(** [get_single_seq seq c2] returns a signle sequence
* of sequence [seq] *)        
val get_single_seq : s -> Cost_matrix.Two_D.m -> s

(** [cmp_locus_indel_cost s c2 locus_indel]
* returns the locus indel cost of locus [s] *)
val cmp_locus_indel_cost :
  s -> Cost_matrix.Two_D.m -> int * int -> int

module Clip : sig
    val fraction : float
    type old_s = s
    val count_tip_gaps : int -> old_s -> old_s -> int
    val correct_distance : int -> Cost_matrix.Two_D.m -> old_s -> old_s -> int
    -> int
    val corrected_distance : Cost_matrix.Two_D.m -> int -> old_s -> old_s -> int
    type s = [ `DO of old_s | `First of old_s | `Last of old_s ]
    module Align : sig
        val align_2 : ?first_gap:bool -> s -> s -> 
            Cost_matrix.Two_D.m -> Matrix.m -> s * s * int * int * s * s
    end
    val extract_s : s -> old_s
    val cmp_ali_cost :
      s -> s -> [> `Positive ] -> Cost_matrix.Two_D.m -> int
    val cmp_gap_cost : int * int -> s -> int
    val complement_chrom : Alphabet.a -> s -> s
    val concat : s list -> s
    val create : [`DO | `First | `Last ] -> int -> s
    val delete_gap : ?gap_code:int -> s -> s
    val foldi : ('a -> int -> int -> 'a) -> 'a -> s -> 'a
    val get : s -> int -> int
    val get_empty_seq : [`DO | `First | `Last ] -> s
    val init : [`DO | `First | `Last ] -> (int -> int) -> int -> s
    val is_empty : s -> int -> bool
    val length : s -> int
    val prepend : s -> int -> unit
    val sub : s -> int -> int -> s
    val to_array : s -> int array
    val to_formater : s -> Alphabet.a -> string
    val to_string : s -> Alphabet.a -> string
    val safe_reverse : s -> s
end
