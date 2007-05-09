type seed_t = Seed.seed_t
type pairChromPam_t = ChromPam.chromPairAliPam_t
type block_t = Block.block_t
type subseq_t = Subseq.subseq_t
val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val deref : 'a option -> 'a
val debug : bool
type direction_t = ChromPam.direction_t
type seg_t = {
  sta : int;
  en : int;
  cost : int;
  alied_med : Sequence.s;
  sta1 : int;
  en1 : int;
  alied_seq1 : Sequence.s;
  dir1 : direction_t;
  sta2 : int;
  en2 : int;
  alied_seq2 : Sequence.s;
  dir2 : direction_t;
}
type med_t = {
  seq : Sequence.s;

  ref_code : int;
  ref_code1 : int;
  ref_code2 : int;
  cost1 : int;
  recost1 : int;
  cost2 : int;
  recost2 : int;
  chrom_map : seg_t list;
}
val create_med : Sequence.s -> med_t
val init_med : Sequence.s -> med_t
val get_dir : [> `Negative | `Positive ] -> string
val print_median : med_t list -> string -> unit
val convert_map : med_t -> (int * int * int * int * int * int) list
val create_map : med_t -> int -> int * int * Tags.output
val create_global_map :
  Sequence.s ->
  Sequence.s ->
  Cost_matrix.Two_D.m ->
  Seed.pairChromPam_t ->
  Block.block_t list * Block.subseq_t list * Block.subseq_t list
val create_median :
  Subseq.subseq_t list ->
  Subseq.subseq_t list ->
  Sequence.s * int ->
  Sequence.s * int ->
  Block.block_t list ->
  (Sequence.s * Sequence.s) array array ->
  int array ->
  int array ->
  int array * int * int * int ->
  Cost_matrix.Two_D.m -> ChromPam.chromPairAliPam_t -> med_t
val cmp_cost :
  med_t -> med_t -> Cost_matrix.Two_D.m -> Data.dyna_pam_t -> int * int
val find_med2_ls :
  med_t ->
  med_t -> Cost_matrix.Two_D.m -> Data.dyna_pam_t -> int * int * med_t list
val test : unit -> 'a
