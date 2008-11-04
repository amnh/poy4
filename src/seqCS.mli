exception Illegal_Arguments
module Codes :
  sig
    type key = int
    type 'a t = 'a All_sets.IntegerMap.t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
type cost_tuple = { min : float; max : float; }
type packed_algn =
    Packed of (int * BitSet.t * packed_algn)
  | Raw of Sequence.s
type heuristic = {
  seqs : int;
  c2 : Cost_matrix.Two_D.m;
  c3 : Cost_matrix.Three_D.m;
}
val make_default_heuristic :
  ?c3:Cost_matrix.Three_D.m -> Cost_matrix.Two_D.m -> heuristic
module ProtAff :
  sig
    type s = Sequence.s
    module Unions :
      sig
        type off_type =
            (int, Bigarray.int16_signed_elt, Bigarray.c_layout)
            Bigarray.Array1.t
        val to_int : int -> int
        type u =
          Sequence.Unions.u = {
          seq : Sequence.s;
          offset : off_type;
          union_c1 : off_type;
          union_c2 : off_type;
        }
        val leaf : Sequence.s -> u
        val union :
          Sequence.s ->
          Sequence.s -> Sequence.s -> u -> u -> Cost_matrix.Two_D.m -> u
        val get_seq : u -> Sequence.s
        val get_positions :
          u -> (int * int) list -> (int * int) list * (int * int) list
        val compare : u -> u -> int
      end
    val encoding :
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
      Sequence.s -> float
    val compare : Sequence.s -> Sequence.s -> int
    val to_formater : Sequence.s -> Alphabet.a -> string
    val create : int -> Sequence.s
    val length : Sequence.s -> int
    val get : Sequence.s -> int -> int
    val prepend : Sequence.s -> int -> unit
    val is_empty : Sequence.s -> int -> bool
    val close_block_diagonal : int array array ref
    val extend_block_diagonal : int array array ref
    val extend_vertical : int array array ref
    val extend_horizontal : int array array ref
    val final_cost_matrix : int array array ref
    val direction_matrix : int array array ref
    val create_arrays : 'a array -> 'b array -> unit
    val ( --> ) : 'a -> ('a -> 'b) -> 'b
    val max_int : int
    val has_gap_extension : int -> 'a -> int -> int
    val align : int
    val align_to_vertical : int
    val align_to_horizontal : int
    val align_to_diagonal : int
    val begin_block : int
    val end_block : int
    val begin_vertical : int
    val end_vertical : int
    val begin_horizontal : int
    val end_horizontal : int
    val do_align : int
    val do_vertical : int
    val do_horizontal : int
    val do_diagonal : int
    val a : int
    val c : int
    val g : int
    val t : int
    val p : int
    val align_sequences : int -> int -> int -> int array -> int array -> unit
    val initialize_matrices :
      int -> int -> 'a -> int array -> int array -> unit
    type mode = Todo | Vertical | Horizontal | Diagonal | Align
    exception Wrong_Position of (int * int * string)
    val to_s : mode -> string
    val print_matrix : int -> int -> string -> int array array -> unit
    val backtrace :
      Sequence.s -> Sequence.s -> Sequence.s * Sequence.s * Sequence.s
    val seq_to_array : Sequence.s -> int array
    val do_alignment :
      int ->
      int ->
      int -> Sequence.s -> Sequence.s -> Sequence.s * Sequence.s * Sequence.s
    val print_list : string -> int list -> unit
    module Align :
      sig
        val of_list : int list -> Sequence.s
        val readjust_3d : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
        val align_2 :
          Sequence.s ->
          Sequence.s ->
          Cost_matrix.Two_D.m -> Sequence.s * Sequence.s * Sequence.s * int
        val union : 'a -> 'b -> 'c
        val cost_2 : Sequence.s -> Sequence.s -> Cost_matrix.Two_D.m -> int
        val closest :
          Sequence.s ->
          Sequence.s -> Cost_matrix.Two_D.m -> 'a -> Sequence.s * int
        val full_median_2 :
          Sequence.s -> Sequence.s -> Cost_matrix.Two_D.m -> 'a -> Sequence.s
      end
  end
module DOS :
  sig
    val bitset_to_seq : int -> packed_algn -> Sequence.s
    val seq_to_bitset : int -> Sequence.s -> packed_algn -> packed_algn
    type do_single_sequence = {
      sequence : Sequence.s;
      aligned_children : packed_algn * packed_algn * packed_algn;
      costs : cost_tuple;
      position : int;
    }
    val create : Sequence.s -> do_single_sequence
    val to_union : do_single_sequence -> Sequence.Unions.u
    val make_cost : int -> cost_tuple
    val readjust :
      [< `ApproxD of 'a | `ThreeD of 'b ] ->
      heuristic ->
      do_single_sequence ->
      do_single_sequence ->
      do_single_sequence ->
      do_single_sequence -> bool * do_single_sequence * int
    val to_single :
      heuristic ->
      do_single_sequence -> do_single_sequence -> do_single_sequence * int
    val median :
      'a ->
      'b ->
      heuristic ->
      do_single_sequence -> do_single_sequence -> do_single_sequence * int
    val median_3_no_union :
      heuristic ->
      do_single_sequence ->
      do_single_sequence ->
      do_single_sequence -> do_single_sequence -> do_single_sequence
    val median_3_union :
      heuristic ->
      do_single_sequence ->
      do_single_sequence -> 'a -> 'b -> do_single_sequence
    val distance :
      Alphabet.a ->
      heuristic -> int -> do_single_sequence -> do_single_sequence -> int
  end
val max_float : float
module RL :
  sig
    type relaxed_lifted = {
      distance_table : float array array;
      sequence_table : Sequence.s array;
    }
    type fs_sequences = {
      states : float array;
      left : int array;
      right : int array;
    }
    val find_smallest : fs_sequences -> int
    val find_single_position : int -> relaxed_lifted * fs_sequences -> int
    val to_single :
      'a * fs_sequences ->
      relaxed_lifted * fs_sequences -> DOS.do_single_sequence * int
    val to_single_parent_done :
      DOS.do_single_sequence ->
      relaxed_lifted * fs_sequences -> DOS.do_single_sequence * int
    val median :
      'a ->
      relaxed_lifted * fs_sequences ->
      'b * fs_sequences -> (relaxed_lifted * fs_sequences) * float
    val median_3 : 'a -> 'b -> 'c -> 'd -> 'e -> 'c
    val distance :
      relaxed_lifted * fs_sequences -> 'a * fs_sequences -> float
    val dist_2 :
      'a * fs_sequences ->
      relaxed_lifted * fs_sequences -> 'b * fs_sequences -> int
  end
type sequence_characters =
    Heuristic_Selection of DOS.do_single_sequence
  | Relaxed_Lifted of (RL.relaxed_lifted * RL.fs_sequences)
type t = {
  characters : sequence_characters array;
  codes : int array;
  total_cost : float;
  alph : Alphabet.a;
  code : int;
  heuristic : heuristic;
  priority : int list;
}
module Union :
  sig
    type ustr = {
      unions : Sequence.Unions.u option array;
      u_c2 : Cost_matrix.Two_D.m;
      u_alph : Alphabet.a;
      u_codes : int array;
    }
    type u = ustr option
    val compare_union : ustr option -> ustr option -> int
    val cardinal_union : ustr option -> int
    val poly_saturation : ustr option -> int -> float
    val union : t -> ustr option -> ustr option -> ustr option
    val distance_union : ustr option -> ustr option -> float
    val get_sequence_union : int -> ustr option -> Sequence.Unions.u option
  end
val cardinal : t -> int
val empty : int -> Cost_matrix.Two_D.m -> Alphabet.a -> t
val to_union : t -> Union.ustr option
val to_string : t -> string
val of_array :
  Data.dynamic_hom_spec -> (Sequence.s * int) array -> int -> int -> t
val of_list :
  Data.dynamic_hom_spec -> (Sequence.s * int) list -> int -> int -> t
val same_codes : t -> t -> bool
val readjust :
  [< `ApproxD of 'a | `ThreeD of 'b ] ->
  All_sets.Integers.t option ->
  All_sets.Integers.t -> t -> t -> t -> t -> All_sets.Integers.t * float * t
val to_single : t -> t -> float * float * t
val median : 'a -> t -> t -> t
val median_3 : t -> t -> t -> t -> t
val distance : float -> t -> t -> float
val dist_2 : float -> t -> t -> t -> float
val f_codes_comp : t -> All_sets.Integers.t -> t
val f_codes : t -> All_sets.Integers.t -> t
val compare_data : t -> t -> int
val ( --> ) : 'a -> ('a -> 'b) -> 'b
val to_formatter :
  Tags.attribute list -> t -> t option -> Data.d -> Tags.output list
val tabu_distance : t -> float
val explode :
  t ->
  (int * Sequence.s * Cost_matrix.Two_D.m * Cost_matrix.Three_D.m *
   Alphabet.a)
  list
val encoding :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  t -> float
