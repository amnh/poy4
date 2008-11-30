type tag = string
type value =
    [ `Bool of bool
    | `Float of float
    | `FloatFloatTuple of float * float
    | `Fun of unit -> string
    | `Int of int
    | `IntFloatTuple of int * float
    | `IntTuple of int * int
    | `String of string ]
type 'a struc =
    [ `Delayed of unit -> 'a Sexpr.t
    | `Empty
    | `Set of 'a Sexpr.t list
    | `Single of 'a ]
val eagerly_compute :
  [< `Delayed of unit -> ([> 'b Sexpr.t ] as 'a)
   | `Empty
   | `Set of 'b Sexpr.t list
   | `Single of 'b ] ->
  'a
type attribute = tag * value
type attributes = attribute list
type output =
    tag * attributes *
    [ `Bool of bool
    | `Delayed of unit -> output Sexpr.t
    | `Empty
    | `Float of float
    | `FloatFloatTuple of float * float
    | `Fun of unit -> string
    | `Int of int
    | `IntFloatTuple of int * float
    | `IntTuple of int * int
    | `Set of output Sexpr.t list
    | `Single of output
    | `String of string ]
val make : 'a -> 'b -> 'c -> 'a * 'b * 'c
val remove_non_alpha_numeric : string -> string
val value_to_string :
  [< `Bool of bool
   | `Float of float
   | `FloatFloatTuple of float * float
   | `Fun of unit -> string
   | `Int of int
   | `IntFloatTuple of int * float
   | `IntTuple of int * int
   | `String of string ] ->
  string
val print_string :
  out_channel ->
  [< `Bool of bool
   | `Float of float
   | `FloatFloatTuple of float * float
   | `Fun of unit -> string
   | `Int of int
   | `IntFloatTuple of int * float
   | `IntTuple of int * int
   | `String of string ] ->
  unit
val to_xml :
  out_channel ->
  string *
  (string *
   [< `Bool of bool
    | `Float of float
    | `FloatFloatTuple of float * float
    | `Fun of unit -> string
    | `Int of int
    | `IntFloatTuple of int * float
    | `IntTuple of int * int
    | `String of string ])
  list *
  [< `Bool of bool
   | `Delayed of
       unit ->
       (string *
        (string *
         ([< `Bool of bool
           | `Float of float
           | `FloatFloatTuple of float * float
           | `Fun of unit -> string
           | `Int of int
           | `IntFloatTuple of int * float
           | `IntTuple of int * int
           | `String of string ]
          as 'b))
        list *
        ([< `Bool of bool
          | `Delayed of unit -> 'a Sexpr.t
          | `Empty
          | `Float of float
          | `FloatFloatTuple of float * float
          | `Fun of unit -> string
          | `Int of int
          | `IntFloatTuple of int * float
          | `IntTuple of int * int
          | `Set of 'a Sexpr.t list
          | `Single of 'a
          | `String of string ]
         as 'c)
        as 'a)
       Sexpr.t
   | `Empty
   | `Float of float
   | `FloatFloatTuple of float * float
   | `Fun of unit -> string
   | `Int of int
   | `IntFloatTuple of int * float
   | `IntTuple of int * int
   | `Set of 'a Sexpr.t list
   | `Single of 'a
   | `String of string ] ->
  unit
module Alphabet :
  sig val element : string val value : string val code : string end
module Characters : sig
    val suffix : string
    val character : string
    val additive : string
    val nonadditive : string
    val molecular : string
    val sankoff : string
    val set : string
    val name : string
    val cost : string
    val recost : string
    val definite : string
    val weight : string
    val cclass : string
    val alphabet : string
    val words : string
    val ints : string
    val chars : string
    val min : string
    val max : string
    val value : string
    val annchrom : string
    val initial_assignment : string
    val tcm : string
    val gap_opening : string
    val state : string
    val states : string
    val ref_code : string
    val chrom_map : string
    val label : string
    val labels : string
    val item : string
    val observed : string
    val source : string
    val missing_symbol : string
    val matchstate_symbol : string
    val gap_symbol : string
    val ignore : string
    val case : string
    val equivalencies : string
    val equivalent : string
    val from : string
    val towards : string
    val clas : string
    val sequence : string
    val chromosome : string
    val genome : string
    val annotated : string
    val breakinv : string
    val seed_len : string
    val re_meth : string
    val circular : string
    val locus_indel_cost : string
    val chrom_indel_cost : string
    val chrom_hom : string
    val chrom_breakpoint : string
    val sig_block_len : string
    val rearranged_len : string
    val keep_median : string
    val swap_med : string
    val approx : string
    val symmetric : string
    val max_3d_len : string
    val max_kept_wag : string
  end
module Nodes :
  sig
    val node : string
    val preliminary : string
    val single : string
    val final : string
    val cost : string
    val recost : string
    val node_cost : string
    val name : string
    val nce : string
    val notu : string
    val child1_name : string
    val child2_name : string
  end
module Trees :
  sig
    val forest : string
    val tree : string
    val cost : string
    val recost : string
  end
module Data :
  sig
    val cost_matrix : string
    val gap_opening : string
    val trees : string
    val data : string
    val synonyms : string
    val synonym : string
    val value : string
    val code : string
    val name : string
    val taxon : string
    val taxa : string
    val file_contents : string
    val filename : string
    val file : string
    val files : string
    val ignored_taxa : string
    val ignored_characters : string
    val characters : string
  end
module GenomeMap :
  sig
    val genome : string
    val chrom : string
    val seg : string
    val ref_code : string
    val a_ref_code : string
    val d_ref_code : string
    val a_chrom_id : string
    val d_chrom_id : string
    val seq_order : string
    val a_seq_order : string
    val d_seq_order : string
    val start_seg : string
    val a_start_seg : string
    val d_start_seg : string
    val end_seg : string
    val a_end_seg : string
    val d_end_seg : string
    val dir_seg : string
    val a_dir_seg : string
    val d_dir_seg : string
  end
