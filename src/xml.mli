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
type 'b structured =
    [ `Delayed of (unit -> 'b Sexpr.t)
    | 'b Sexpr.t ]

(** The official structured type, is any of the simple structured constructures
* defined in [smiple_struc] and the CDATA, which could hold a unstructured, or some
* structured contents (for example when holding HTML). *)
type 'a structured_xml =
    [ 'a structured 
    | `CDATA of [ unstructured | 'a structured] ]

(** An attribute of a tag is a pair consisting of a name of the attribute, which
* has type [tag], and an attribute unstructured. *)
type attribute = tag * unstructured

(** The attributes of a tag is simply a list of [attribute]. *)
type attributes = attribute list

(** The contents of a tag, that is, the contents of an XML node, is either a
* [unstructured], or a [structured_xml]. *)
type 'a contents = [ unstructured | 'a structured_xml ]

(** Finally we define the [xml] type, consisting of a triplet, of [tag],
* [attributes] of the tag, and its [contents], which must hold inside more valid
* XML, that is, [output]. *)
type xml = tag * attributes * xml contents

(** {2 Utility functions} *)
(** Some utility functions to operate on [xml]'s. *)

(**  [eagerly_compute s] applies any delayed  computation in the top of 
* [s] and produces the structured contents available for pattern 
* matching. *)
val eagerly_compute : [< xml structured ] -> xml Sexpr.t


(** [attribute a xml] retrieves the attribute with name [a] in the [xml]. If
* the attribute doesn't exists, the function raises Not_found. *)
val attribute : string -> xml -> unstructured

(** [tag xml] retrieves the tag of the [xml]. *)
val tag : xml -> string

(** [contents xml] retrieves the contents of [xml]. *)
val contents : xml -> xml contents

(** [attributes xml] retrieves the list of attributes in [xml]. *)
val attributes : xml -> attributes

(** [children tag xml] retrieves the children of [xml] with [tag]. *)
val children : string -> xml -> xml Sexpr.t

(** [value xml] retrieves the contents of [xml] when its contents are
* [unstructured]. If the contents are really structured, it raises a [Failure
* "Not a value"] exception. *)
val value : xml -> unstructured

(** [value xml] retrieves the contents of [xml] when these are structured. If
* the contents are really an [unstructured] or [structured_xml] then the
* function raises a [Failure "Not structured"] exception. *)
val structured : xml -> xml structured

(** [coherce contents] cohereces any subset of the valid [xml contents] in
 * to the [xml contents]. This is useful to avoid silly polymorphic variant
 * errors due to the difficulty of unifying this types. *)
val coherce : [< xml contents ] -> xml contents

(** Create xml contents. The preferred way to do this, however, is using the
* poyExtension. [make a b c] is equivalent to (RXML [a] [b] {c}) *)
val make : tag -> attributes -> xml contents -> xml

(** [value_to_string v] takes an unstructed and outputs a string
* representation. *)
val value_to_string : unstructured -> string

(** [to_file ch xml] dumps the [xml] in the text channel [ch]. *)
val to_file : out_channel -> xml -> unit

(** {2 Xml}
 *
 * Various tags employed in XML components. Convenient for comparison functions
 * or retrieve particular elments.*)

module Alphabet :
  sig 
      val element : string 
      val value : string 
      val code : string 
  end

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
