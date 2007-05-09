type seed_t = Seed.seed_t
type pairChromPam_t = ChromPam.chromPairAliPam_t
type block_t = Block.block_t
type subseq_t = Subseq.subseq_t
val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
val deref : 'a option -> 'a
val debug : bool
val gap : int
type direction_t = ChromPam.direction_t
type seg_t = {
  sta : int;
  en : int;
  cost : int;
  med_chrom_id : int;
  ref_code1 : int;
  sta1 : int;
  en1 : int;
  chi1_chrom_id : int;
  alied_seq1 : Sequence.s;
  dir1 : direction_t;
  ref_code2 : int;
  sta2 : int;
  en2 : int;
  chi2_chrom_id : int;
  alied_seq2 : Sequence.s;
  dir2 : direction_t;
}
type chrom_t = {
  chrom_id : int ref;
  chrom_ref_code : int;
  map : seg_t list;
  seq : Sequence.s;
}
type med_t = { chrom_arr : chrom_t array; genome_ref_code : int; }
type genome_block_t = {
  block_id : int;
  chrom1_id : int;
  chrom2_id : int;
  block : Block.block_t;
}
val to_string : med_t -> string
val print_genome : med_t -> unit
val get_chroms : med_t -> Sequence.s array
val chrom_map_to_string : seg_t list -> string
val genome_map_to_string : med_t -> string
val ref_genome : med_t ref
val assign_hom_chrom :
  med_t -> Cost_matrix.Two_D.m -> Data.dyna_pam_t -> unit
val init : Sequence.s Data.dyna_data -> med_t
val find_conserved_areas :
  Sequence.s ->
  Sequence.s -> Cost_matrix.Two_D.m -> Data.dyna_pam_t -> Block.block_t list
val find_chrom : int -> med_t -> chrom_t option
val create_loci : Sequence.s -> Subseq.subseq_t list -> Subseq.subseq_t list
val create_fast_general_ali :
  int ->
  int ->
  Sequence.s ->
  Subseq.subseq_t list ->
  int ->
  Sequence.s ->
  Subseq.subseq_t list ->
  genome_block_t list ->
  Cost_matrix.Two_D.m ->
  ChromPam.chromPairAliPam_t -> Sequence.s * seg_t list * int * int
val create_chrom_med :
  int * chrom_t ->
  int * chrom_t ->
  genome_block_t list ->
  Cost_matrix.Two_D.m -> ChromPam.chromPairAliPam_t -> chrom_t * int * int
val create_genome_blocks :
  med_t ->
  med_t -> Cost_matrix.Two_D.m -> Data.dyna_pam_t -> genome_block_t list
val create_med :
  med_t ->
  med_t -> Cost_matrix.Two_D.m -> Data.dyna_pam_t -> med_t * int * int
val cmp_cost :
  med_t -> med_t -> Cost_matrix.Two_D.m -> Data.dyna_pam_t -> int * int
val find_med2_ls :
  med_t ->
  med_t -> Cost_matrix.Two_D.m -> Data.dyna_pam_t -> int * int * med_t list
val compare : med_t -> med_t -> int
