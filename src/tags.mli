(*** Producing and processing an XML like data structure.
*
* Each character has it's own components, and accessing those components could
* be cumbersome for many applications, in particular the generations of
* user-specific kinds of output.  For this reason, the name of the data
* structure is [xml].
*
* This module defines the [xml] type, the tags used by other modules, and
* some convenience functions to manipulate the types here defined.*)

(** {2 Types} *)

type tag = string

(** Unstructured values. *)
type unstructured =
    [ `Bool of bool     
    | `Float of float
    | `FloatFloatTuple of float * float
    | `Fun of unit -> string
    | `Int of int
    | `IntFloatTuple of int * float
    | `IntTuple of int * int
    | `String of string ]

(** A structured type can be a Sexpr.t, or a delayed computation that will only
* be performed if required, producing a Sexpr.t itself. This is a simple
* structured type because we don't allow a CDATA kind of contents here, which is
* really an unstructured content from the XML point of view.*)
type 'b simple_struc =
    [ `Delayed of (unit -> 'b Sexpr.t)
    | 'b Sexpr.t ]

(** The official structured type, is any of the simple structured constructures
* defined in [smiple_struc] and the CDATA, which could hold a unstructured, or some
* structured contents (for example when holding HTML). *)
type 'a struc =
    [ 'a simple_struc 
    | `CDATA of [ unstructured | 'a simple_struc] ]

(** An attribute of a tag is a pair consisting of a name of the attribute, which
* has type [tag], and an attribute unstructured. *)
type attribute = tag * unstructured

(** The attributes of a tag is simply a list of [attribute]. *)
type attributes = attribute list

(** The contents of a tag, that is, the contents of an XML node, is either a
* [unstructured], or a [struc]. *)
type 'a contents = [ unstructured | 'a struc ]

(** Finally we define the [xml] type, consisting of a triplet, of [tag],
* [attributes] of the tag, and its [contents], which must hold inside more valid
* XML, that is, [output]. *)
type xml = tag * attributes * xml contents

val eagerly_compute : [ 'b simple_struc ] -> 'b Sexpr.t

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
val to_xml : out_channel -> xml -> unit
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

module Util : sig
    val attribute : string -> xml -> unstructured
    val tag : xml -> string
    val contents : xml -> xml contents
    val attributes : xml -> attributes
    val children : string -> xml -> xml Sexpr.t
    val value : xml -> unstructured
end
