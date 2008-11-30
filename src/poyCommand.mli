exception Exit
type read_option_t = [ `Init3D of bool | `Orientation of bool ]
type otherfiles =
    [ `Aminoacids of string list
    | `AutoDetect of string
    | `Breakinv of string * string * read_option_t list
    | `Chromosome of string list
    | `ComplexTerminals of string list
    | `GeneralAlphabetSeq of string * string * read_option_t list
    | `Genome of string list
    | `Nucleotides of string list
    | `PartitionedFile of string list
    | `Prealigned of otherfiles * Methods.prealigned_costs ]
type reada = Methods.input
type keepa = Methods.tree_handling
type old_identifiers =
    [ `All
    | `AllDynamic
    | `AllStatic
    | `Missing of bool * int
    | `Names of bool * string list
    | `Random of float
    | `Some of bool * int list ]
type identifiers =
    [ `All
    | `AllDynamic
    | `AllStatic
    | `Files of bool * string list
    | `Missing of bool * int
    | `Names of bool * string list
    | `Random of float
    | `Some of bool * int list ]
type chromosome_args =
    [ `Approx of bool
    | `Chrom_Breakpoint of int
    | `Chrom_Hom of int
    | `Chrom_Indel_Cost of int * int
    | `Circular of bool
    | `Keep_Median of int
    | `Locus_Breakpoint of int
    | `Locus_Indel_Cost of int * int
    | `Locus_Inversion of int
    | `Max_3D_Len of int
    | `Max_kept_wag of int
    | `Rearranged_Len of int
    | `Seed_Len of int
    | `Sig_Block_Len of int
    | `SwapMed of int
    | `Symmetric of bool ]
type transform_method =
    [ `AffGap of int
    | `AlphabeticTerminals
    | `AnnchromToBreakinv of chromosome_args list
    | `Automatic_Sequence_Partition of bool * int option
    | `Automatic_Static_Aprox of bool
    | `BreakinvToSeq of chromosome_args list
    | `ChangeDynPam of chromosome_args list
    | `ChromToSeq of chromosome_args list
    | `CustomToBreakinv of chromosome_args list
    | `Direct_Optimization
    | `Fixed_States
    | `Gap of int * int
    | `MultiStaticApproximation of bool
    | `OriginCost of float
    | `Partitioned
    | `Prealigned_Transform
    | `PrepFile of string
    | `PrepInput of int array
    | `Prioritize
    | `RandomizedTerminals
    | `ReWeight of float
    | `SearchBased
    | `SeqToChrom of chromosome_args list
    | `StaticApproximation of bool
    | `TailFile of string
    | `TailInput of int array
    | `Tcm of string
    | `WeightFactor of float ]
type transforma = identifiers * transform_method
type transform = [ `Transform of transforma list ]
type cost_calculation =
    [ `Exhaustive_Strong
    | `Exhaustive_Weak
    | `Iterative of [ `ApproxD of int option | `ThreeD of int option ]
    | `Normal
    | `Normal_plus_Vitamines ]
type keep_method = [ `First | `Keep_Random | `Last ]
type thresh_trees = [ `Threshold of float | `Trees of int ]
type builda =
    [ `AllBased of int option
    | `Branch_and_Bound of float option
    | `Constraint of string option
    | `DistancesRnd
    | `First
    | `Keep_Random
    | `Last
    | `Lookahead of int
    | `Mst
    | `Ordered
    | `Partition of
        [ `ConstraintFile of Methods.filename
        | `MaxDepth of int
        | `Sets of All_sets.IntSet.t Lazy.t ] list
    | `Prebuilt of Methods.filename
    | `Random
    | `RandomTree
    | `Threshold of float
    | `Transform of transforma list
    | `Trees of int
    | `UnionBased of int option ]
type swap_neighborhood = [ `Spr | `Tbr ]
type swap_strategy =
    [ `Alternate of swap_neighborhood * swap_neighborhood
    | `ChainNeighborhoods of swap_neighborhood
    | `None
    | `SingleNeighborhood of swap_neighborhood ]
type swap_trajectory =
    [ `AllAround of string option
    | `AllThenChoose
    | `Annealing of float * float
    | `BestFirst
    | `PoyDrifting of float * float ]
type swapa =
    [ `AllAround of string option
    | `AllBased of int option
    | `AllThenChoose
    | `AllVisited of string option
    | `Alternate of swap_neighborhood * swap_neighborhood
    | `Annealing of float * float
    | `AttemptsDistr of string option
    | `BestFirst
    | `Bfs of int option
    | `BreakVsJoin of string option
    | `ChainNeighborhoods of swap_neighborhood
    | `DistanceSorted of bool
    | `First
    | `Forest of float
    | `KeepBestTrees
    | `Keep_Random
    | `Last
    | `None
    | `OnlyOnce
    | `Partition of
        [ `ConstraintFile of Methods.filename
        | `MaxDepth of int
        | `Sets of All_sets.IntSet.t Lazy.t ] list
    | `PoyDrifting of float * float
    | `PrintTrajectory of string option
    | `Randomized
    | `RootUnionDistr of string option
    | `SingleNeighborhood of swap_neighborhood
    | `Threshold of float
    | `TimeOut of Methods.timer
    | `TimedPrint of float * string option
    | `Transform of transforma list
    | `Trees of int
    | `UnionBased of int option
    | `UnionStats of string option * int ]
type swap = [ `Swap of swapa list ]
type build = [ `Build of builda list ]
type supporta =
    [ `Bootstrap of int option
    | `Bremer
    | `Build of builda list
    | `Jackknife of [ `Resample of int | `Select of float ] list
    | `Swap of swapa list ]
type poy_file_commands =
    [ `InspectFile of string
    | `Load of string
    | `Save of string * string option ]
type internal_memory =
    [ `Discard of Methods.store_class list * string
    | `Store of Methods.store_class list * string
    | `Use of Methods.store_class list * string ]
type settings =
    [ `Exhaustive_Strong
    | `Exhaustive_Weak
    | `HistorySize of int
    | `Iterative of [ `ApproxD of int option | `ThreeD of int option ]
    | `Logfile of string option
    | `Normal
    | `Normal_plus_Vitamines
    | `Root of int option
    | `RootName of string
    | `SetSeed of int
    | `TimerInterval of int ]
type output_class = [ `Error | `Information | `Output of string option ]
type application =
    [ `ChangeWDir of string
    | `ClearMemory of Methods.clear_item list
    | `ClearRecovered
    | `Discard of Methods.store_class list * string
    | `Echo of string * output_class list
    | `Exit
    | `Help of string option
    | `InspectFile of string
    | `Load of string
    | `PrintWDir
    | `ReDiagnose
    | `ReadScript of string list
    | `Recover
    | `Redraw
    | `Save of string * string option
    | `Set of settings list
    | `Store of Methods.store_class list * string
    | `Use of Methods.store_class list * string
    | `Version
    | `Wipe ]
type charortax = [ `Characters | `Taxa ]
type charoper =
    [ `Characters of identifiers | `Distance | `Median | `Taxa of identifiers ]
type reporta =
    [ `AllRootsCost
    | `Ascii of bool
    | `Ci of old_identifiers option
    | `Clades
    | `CompareSequences of bool * old_identifiers * old_identifiers
    | `Consensus of float option
    | `CrossReferences of old_identifiers option
    | `Data
    | `Diagnosis
    | `ExplainScript of string
    | `FasWinClad
    | `File of string
    | `Graph of bool
    | `GraphicConsensus of float option
    | `GraphicDiagnosis
    | `GraphicSupports of Methods.support_output option
    | `Implied_Alignments of identifiers * bool
    | `Memory
    | `MstR
    | `Nodes
    | `Ri of old_identifiers option
    | `SearchStats
    | `SequenceStats of old_identifiers
    | `Supports of Methods.support_output option
    | `TerminalsFiles
    | `TimeDelta of string
    | `TreeCosts
    | `Trees of Methods.information_contained list
    | `TreesStats
    | `Xslt of string * string ]
type perturba =
    [ `Iterations of int
    | `Ratchet of float * int
    | `Resample of int * charortax
    | `Swap of swapa list
    | `TimeOut of Methods.timer
    | `Transform of transforma list ]
type selecta =
    [ `All
    | `AllDynamic
    | `AllStatic
    | `BestN of int option
    | `BestWithin of float
    | `Characters
    | `Files of bool * string list
    | `Missing of bool * int
    | `Names of bool * string list
    | `Random of float
    | `RandomTrees of int
    | `Some of bool * int list
    | `Taxa
    | `Unique ]
type renamea =
    [ `Characters | `File of string | `Syn of string * string | `Taxa ]
type fusea =
    [ `Clades of int * int option
    | `Iterations of int
    | `Keep of int
    | `Replace of [ `Best | `Better ]
    | `Swap of swapa list
    | `Weighting of [ `Uniform ] ]
type searcha = [ `Build of bool | `Transform of bool ]
type std_searcha =
    [ `ConstraintFile of string
    | `MaxRam of int
    | `MaxTime of float
    | `MinHits of int
    | `MinTime of float
    | `Target of float
    | `Visited of string option ]
type command =
    [ `Build of builda list
    | `Calculate of charoper list
    | `ChangeWDir of string
    | `ClearMemory of Methods.clear_item list
    | `ClearRecovered
    | `Discard of Methods.store_class list * string
    | `Echo of string * output_class list
    | `Exit
    | `Fuse of fusea list
    | `Help of string option
    | `InspectFile of string
    | `Load of string
    | `Perturb of perturba list
    | `PrintWDir
    | `ReDiagnose
    | `Read of reada list
    | `ReadScript of string list
    | `Recover
    | `Redraw
    | `Rename of renamea list
    | `Repeat of int * command list
    | `Report of reporta list
    | `Save of string * string option
    | `Search of searcha list
    | `Select of selecta list
    | `Set of settings list
    | `StandardSearch of std_searcha list
    | `Store of Methods.store_class list * string
    | `Support of supporta list
    | `Swap of swapa list
    | `Transform of transforma list
    | `Use of Methods.store_class list * string
    | `Version
    | `Wipe ]
val all_store_types :
  [> `Bootstrap | `Bremer | `Data | `Jackknife | `Trees ] list
val transform_transform :
  ([> `AlphabeticTerminals
    | `Annchrom_to_Breakinv of [> Methods.characters ] * 'b
    | `Assign_Affine_Gap_Cost of 'c * [> Methods.characters ]
    | `Assign_Prep_Cost of
        [> `Array of 'd | `File of [> `Local of 'e ] ] *
        [> Methods.characters ]
    | `Assign_Tail_Cost of
        [> `Array of 'f | `File of [> `Local of 'g ] ] *
        [> Methods.characters ]
    | `Assign_Transformation_Cost_Matrix of
        [> `Local of 'h ] option * [> Methods.characters ]
    | `Automatic_Sequence_Partition of [> Methods.characters ] * 'i * 'j
    | `Automatic_Static_Aprox of 'k
    | `Breakinv_to_Custom of [> Methods.characters ] * 'l
    | `Change_Dyn_Pam of [> Methods.characters ] * 'm
    | `Chrom_to_Seq of [> Methods.characters ] * 'n
    | `Create_Transformation_Cost_Matrix of 'o * 'p * [> Methods.characters ]
    | `Custom_to_Breakinv of [> Methods.characters ] * 'q
    | `Direct_Optimization of [> Methods.characters ]
    | `Fixed_States of [> Methods.characters ]
    | `MultiStatic_Aprox of [> Methods.characters ] * 'r
    | `OriginCost of 's
    | `Partitioned of [> Methods.characters ]
    | `Prealigned_Transform of [> Methods.characters ]
    | `Prioritize
    | `RandomizedTerminals
    | `ReWeight of [> Methods.characters ] * 't
    | `Search_Based of [> Methods.characters ]
    | `Seq_to_Chrom of [> Methods.characters ] * 'u
    | `Static_Aprox of [> Methods.characters ] * 'v
    | `WeightFactor of [> Methods.characters ] * 'w ]
   as 'a)
  list ->
  [< `All
   | `AllDynamic
   | `AllStatic
   | `Files of 'x
   | `Missing of bool * int
   | `Names of bool * string list
   | `Random of float
   | `Some of bool * int list ] *
  [< `AffGap of 'c
   | `AlphabeticTerminals
   | `AnnchromToBreakinv of 'b
   | `Automatic_Sequence_Partition of 'i * 'j
   | `Automatic_Static_Aprox of 'k
   | `BreakinvToSeq of 'l
   | `ChangeDynPam of 'm
   | `ChromToSeq of 'n
   | `CustomToBreakinv of 'q
   | `Direct_Optimization
   | `Fixed_States
   | `Gap of 'o * 'p
   | `MultiStaticApproximation of 'r
   | `OriginCost of 's
   | `Partitioned
   | `Prealigned_Transform
   | `PrepFile of 'e
   | `PrepInput of 'd
   | `Prioritize
   | `RandomizedTerminals
   | `ReWeight of 't
   | `SearchBased
   | `SeqToChrom of 'u
   | `StaticApproximation of 'v
   | `TailFile of 'g
   | `TailInput of 'f
   | `Tcm of 'h
   | `WeightFactor of 'w ] ->
  'a list
val transform_transform_arguments :
  ([< `All
    | `AllDynamic
    | `AllStatic
    | `Files of 'a
    | `Missing of bool * int
    | `Names of bool * string list
    | `Random of float
    | `Some of bool * int list ] *
   [< `AffGap of 'b
    | `AlphabeticTerminals
    | `AnnchromToBreakinv of 'c
    | `Automatic_Sequence_Partition of 'd * 'e
    | `Automatic_Static_Aprox of 'f
    | `BreakinvToSeq of 'g
    | `ChangeDynPam of 'h
    | `ChromToSeq of 'i
    | `CustomToBreakinv of 'j
    | `Direct_Optimization
    | `Fixed_States
    | `Gap of 'k * 'l
    | `MultiStaticApproximation of 'm
    | `OriginCost of 'n
    | `Partitioned
    | `Prealigned_Transform
    | `PrepFile of 'o
    | `PrepInput of 'p
    | `Prioritize
    | `RandomizedTerminals
    | `ReWeight of 'q
    | `SearchBased
    | `SeqToChrom of 'r
    | `StaticApproximation of 's
    | `TailFile of 't
    | `TailInput of 'u
    | `Tcm of 'v
    | `WeightFactor of 'w ])
  list ->
  [> `AlphabeticTerminals
   | `Annchrom_to_Breakinv of [> Methods.characters ] * 'c
   | `Assign_Affine_Gap_Cost of 'b * [> Methods.characters ]
   | `Assign_Prep_Cost of
       [> `Array of 'p | `File of [> `Local of 'o ] ] *
       [> Methods.characters ]
   | `Assign_Tail_Cost of
       [> `Array of 'u | `File of [> `Local of 't ] ] *
       [> Methods.characters ]
   | `Assign_Transformation_Cost_Matrix of
       [> `Local of 'v ] option * [> Methods.characters ]
   | `Automatic_Sequence_Partition of [> Methods.characters ] * 'd * 'e
   | `Automatic_Static_Aprox of 'f
   | `Breakinv_to_Custom of [> Methods.characters ] * 'g
   | `Change_Dyn_Pam of [> Methods.characters ] * 'h
   | `Chrom_to_Seq of [> Methods.characters ] * 'i
   | `Create_Transformation_Cost_Matrix of 'k * 'l * [> Methods.characters ]
   | `Custom_to_Breakinv of [> Methods.characters ] * 'j
   | `Direct_Optimization of [> Methods.characters ]
   | `Fixed_States of [> Methods.characters ]
   | `MultiStatic_Aprox of [> Methods.characters ] * 'm
   | `OriginCost of 'n
   | `Partitioned of [> Methods.characters ]
   | `Prealigned_Transform of [> Methods.characters ]
   | `Prioritize
   | `RandomizedTerminals
   | `ReWeight of [> Methods.characters ] * 'q
   | `Search_Based of [> Methods.characters ]
   | `Seq_to_Chrom of [> Methods.characters ] * 'r
   | `Static_Aprox of [> Methods.characters ] * 's
   | `WeightFactor of [> Methods.characters ] * 'w ]
  list
val modify_acc :
  ([> `Other of 'b list * 'c ] as 'a) list -> 'c -> 'b list -> 'a list
val build_default_method_args :
  int * float * [> `Last ] * 'a list * [> `UnionBased of 'b option ]
val build_default_method :
  [> `Wagner_Rnd of
       int * float * [> `Last ] * 'a list * [> `UnionBased of 'b option ] ]
val build_default :
  int *
  [> `Wagner_Rnd of
       int * float * [> `Last ] * 'a list * [> `UnionBased of 'b option ] ] *
  'c list
val transform_build :
  'a * Methods.build_method * Methods.transform list ->
  [< `AllBased of int option
   | `Branch_and_Bound of float option
   | `Constraint of string option
   | `DistancesRnd
   | `First
   | `Keep_Random
   | `Last
   | `Lookahead of int
   | `Mst
   | `Ordered
   | `Partition of
       [ `ConstraintFile of Methods.filename
       | `MaxDepth of int
       | `Sets of All_sets.IntSet.t Lazy.t ] list
   | `Prebuilt of Methods.filename
   | `Random
   | `RandomTree
   | `Threshold of float
   | `Transform of
       ([< `All
         | `AllDynamic
         | `AllStatic
         | `Files of 'b
         | `Missing of bool * int
         | `Names of bool * string list
         | `Random of float
         | `Some of bool * int list ] *
        [< `AffGap of int
         | `AlphabeticTerminals
         | `AnnchromToBreakinv of Methods.chromosome_pam_t list
         | `Automatic_Sequence_Partition of bool * int option
         | `Automatic_Static_Aprox of bool
         | `BreakinvToSeq of Methods.chromosome_pam_t list
         | `ChangeDynPam of Methods.chromosome_pam_t list
         | `ChromToSeq of Methods.chromosome_pam_t list
         | `CustomToBreakinv of Methods.chromosome_pam_t list
         | `Direct_Optimization
         | `Fixed_States
         | `Gap of int * int
         | `MultiStaticApproximation of bool
         | `OriginCost of float
         | `Prealigned_Transform
         | `PrepFile of string
         | `PrepInput of int array
         | `Prioritize
         | `RandomizedTerminals
         | `ReWeight of float
         | `SearchBased
         | `SeqToChrom of Methods.chromosome_pam_t list
         | `StaticApproximation of bool
         | `TailFile of string
         | `TailInput of int array
         | `Tcm of string
         | `WeightFactor of float ])
       list
   | `Trees of 'a
   | `UnionBased of int option ] ->
  'a * Methods.build_method * Methods.transform list
val transform_build_arguments :
  [< `AllBased of int option
   | `Branch_and_Bound of float option
   | `Constraint of string option
   | `DistancesRnd
   | `First
   | `Keep_Random
   | `Last
   | `Lookahead of int
   | `Mst
   | `Ordered
   | `Partition of
       [ `ConstraintFile of Methods.filename
       | `MaxDepth of int
       | `Sets of All_sets.IntSet.t Lazy.t ] list
   | `Prebuilt of Methods.filename
   | `Random
   | `RandomTree
   | `Threshold of float
   | `Transform of
       ([< `All
         | `AllDynamic
         | `AllStatic
         | `Files of 'a
         | `Missing of bool * int
         | `Names of bool * string list
         | `Random of float
         | `Some of bool * int list ] *
        [< `AffGap of int
         | `AlphabeticTerminals
         | `AnnchromToBreakinv of Methods.chromosome_pam_t list
         | `Automatic_Sequence_Partition of bool * int option
         | `Automatic_Static_Aprox of bool
         | `BreakinvToSeq of Methods.chromosome_pam_t list
         | `ChangeDynPam of Methods.chromosome_pam_t list
         | `ChromToSeq of Methods.chromosome_pam_t list
         | `CustomToBreakinv of Methods.chromosome_pam_t list
         | `Direct_Optimization
         | `Fixed_States
         | `Gap of int * int
         | `MultiStaticApproximation of bool
         | `OriginCost of float
         | `Prealigned_Transform
         | `PrepFile of string
         | `PrepInput of int array
         | `Prioritize
         | `RandomizedTerminals
         | `ReWeight of float
         | `SearchBased
         | `SeqToChrom of Methods.chromosome_pam_t list
         | `StaticApproximation of bool
         | `TailFile of string
         | `TailInput of int array
         | `Tcm of string
         | `WeightFactor of float ])
       list
   | `Trees of int
   | `UnionBased of int option ]
  list -> int * Methods.build_method * Methods.transform list
val swap_default :
  [> `ChainNeighborhoods of [> `Tbr ] ] * float * int * [> `Last ] *
  'a list * 'b option * [> `BestFirst ] * [> `DistanceSorted of bool ] *
  [> `UnionBased of 'c option ] * [> `Bfs of 'd option ] * 'e list
val swap_default_none :
  [> `None ] * float * int * [> `Last ] * 'a list * 'b option *
  [> `BestFirst ] * [> `DistanceSorted of bool ] *
  [> `UnionBased of 'c option ] * [> `Bfs of 'd option ] * 'e list
val transform_swap :
  ([> swap_strategy ] as 'a) * float * int *
  ([> Methods.keep_method ] as 'b) *
  ([> `AlphabeticTerminals
    | `Annchrom_to_Breakinv of [> Methods.characters ] * chromosome_args list
    | `Assign_Affine_Gap_Cost of int * [> Methods.characters ]
    | `Assign_Prep_Cost of
        [> `Array of int array | `File of [> `Local of string ] ] *
        [> Methods.characters ]
    | `Assign_Tail_Cost of
        [> `Array of int array | `File of [> `Local of string ] ] *
        [> Methods.characters ]
    | `Assign_Transformation_Cost_Matrix of
        [> `Local of string ] option * [> Methods.characters ]
    | `Automatic_Sequence_Partition of
        [> Methods.characters ] * bool * int option
    | `Automatic_Static_Aprox of bool
    | `Breakinv_to_Custom of [> Methods.characters ] * chromosome_args list
    | `Change_Dyn_Pam of [> Methods.characters ] * chromosome_args list
    | `Chrom_to_Seq of [> Methods.characters ] * chromosome_args list
    | `Create_Transformation_Cost_Matrix of
        int * int * [> Methods.characters ]
    | `Custom_to_Breakinv of [> Methods.characters ] * chromosome_args list
    | `Direct_Optimization of [> Methods.characters ]
    | `Fixed_States of [> Methods.characters ]
    | `MultiStatic_Aprox of [> Methods.characters ] * bool
    | `OriginCost of float
    | `Partitioned of [> Methods.characters ]
    | `Prealigned_Transform of [> Methods.characters ]
    | `Prioritize
    | `RandomizedTerminals
    | `ReWeight of [> Methods.characters ] * float
    | `Search_Based of [> Methods.characters ]
    | `Seq_to_Chrom of [> Methods.characters ] * chromosome_args list
    | `Static_Aprox of [> Methods.characters ] * bool
    | `WeightFactor of [> Methods.characters ] * float ]
   as 'c)
  list * float option * ([> swap_trajectory ] as 'd) *
  ([> Methods.tabu_break_strategy ] as 'e) *
  ([> Methods.tabu_join_strategy ] as 'f) * ([> `Bfs of int option ] as 'g) *
  ([> Methods.samples ] as 'h) list ->
  swapa ->
  'a * float * int * 'b * 'c list * float option * 'd * 'e * 'f * 'g *
  'h list
val transform_swap_arguments :
  swapa list ->
  [> `LocalOptimum of
       [> swap_strategy ] * float * int * [> Methods.keep_method ] *
       [> `AlphabeticTerminals
        | `Annchrom_to_Breakinv of
            [> Methods.characters ] * chromosome_args list
        | `Assign_Affine_Gap_Cost of int * [> Methods.characters ]
        | `Assign_Prep_Cost of
            [> `Array of int array | `File of [> `Local of string ] ] *
            [> Methods.characters ]
        | `Assign_Tail_Cost of
            [> `Array of int array | `File of [> `Local of string ] ] *
            [> Methods.characters ]
        | `Assign_Transformation_Cost_Matrix of
            [> `Local of string ] option * [> Methods.characters ]
        | `Automatic_Sequence_Partition of
            [> Methods.characters ] * bool * int option
        | `Automatic_Static_Aprox of bool
        | `Breakinv_to_Custom of
            [> Methods.characters ] * chromosome_args list
        | `Change_Dyn_Pam of [> Methods.characters ] * chromosome_args list
        | `Chrom_to_Seq of [> Methods.characters ] * chromosome_args list
        | `Create_Transformation_Cost_Matrix of
            int * int * [> Methods.characters ]
        | `Custom_to_Breakinv of
            [> Methods.characters ] * chromosome_args list
        | `Direct_Optimization of [> Methods.characters ]
        | `Fixed_States of [> Methods.characters ]
        | `MultiStatic_Aprox of [> Methods.characters ] * bool
        | `OriginCost of float
        | `Partitioned of [> Methods.characters ]
        | `Prealigned_Transform of [> Methods.characters ]
        | `Prioritize
        | `RandomizedTerminals
        | `ReWeight of [> Methods.characters ] * float
        | `Search_Based of [> Methods.characters ]
        | `Seq_to_Chrom of [> Methods.characters ] * chromosome_args list
        | `Static_Aprox of [> Methods.characters ] * bool
        | `WeightFactor of [> Methods.characters ] * float ]
       list * float option * [> swap_trajectory ] *
       [> Methods.tabu_break_strategy ] * [> Methods.tabu_join_strategy ] *
       [> `Bfs of int option ] * [> Methods.samples ] list ]
val transform_fuse :
  ?iterations:'a option ->
  ?keep:'b option ->
  ?replace:([> `Better ] as 'c) ->
  ?search:([> `LocalOptimum of
                [> swap_strategy ] * float * int * [> Methods.keep_method ] *
                [> `AlphabeticTerminals
                 | `Annchrom_to_Breakinv of
                     [> Methods.characters ] * chromosome_args list
                 | `Assign_Affine_Gap_Cost of int * [> Methods.characters ]
                 | `Assign_Prep_Cost of
                     [> `Array of int array | `File of [> `Local of string ] ] *
                     [> Methods.characters ]
                 | `Assign_Tail_Cost of
                     [> `Array of int array | `File of [> `Local of string ] ] *
                     [> Methods.characters ]
                 | `Assign_Transformation_Cost_Matrix of
                     [> `Local of string ] option * [> Methods.characters ]
                 | `Automatic_Sequence_Partition of
                     [> Methods.characters ] * bool * int option
                 | `Automatic_Static_Aprox of bool
                 | `Breakinv_to_Custom of
                     [> Methods.characters ] * chromosome_args list
                 | `Change_Dyn_Pam of
                     [> Methods.characters ] * chromosome_args list
                 | `Chrom_to_Seq of
                     [> Methods.characters ] * chromosome_args list
                 | `Create_Transformation_Cost_Matrix of
                     int * int * [> Methods.characters ]
                 | `Custom_to_Breakinv of
                     [> Methods.characters ] * chromosome_args list
                 | `Direct_Optimization of [> Methods.characters ]
                 | `Fixed_States of [> Methods.characters ]
                 | `MultiStatic_Aprox of [> Methods.characters ] * bool
                 | `OriginCost of float
                 | `Partitioned of [> Methods.characters ]
                 | `Prealigned_Transform of [> Methods.characters ]
                 | `Prioritize
                 | `RandomizedTerminals
                 | `ReWeight of [> Methods.characters ] * float
                 | `Search_Based of [> Methods.characters ]
                 | `Seq_to_Chrom of
                     [> Methods.characters ] * chromosome_args list
                 | `Static_Aprox of [> Methods.characters ] * bool
                 | `WeightFactor of [> Methods.characters ] * float ]
                list * float option * [> swap_trajectory ] *
                [> Methods.tabu_break_strategy ] *
                [> Methods.tabu_join_strategy ] * [> `Bfs of int option ] *
                [> Methods.samples ] list ]
           as 'd) ->
  ?weighting:([> `Uniform ] as 'e) ->
  ?clades:int * int ->
  [< `Clades of int * int option
   | `Iterations of 'a
   | `Keep of 'b
   | `Replace of 'c
   | `Swap of swapa list
   | `Weighting of 'e ]
  list -> [> `Fusing of 'a option * 'b option * 'e * 'c * 'd * (int * int) ]
val perturb_default_swap : Methods.local_optimum
val perturb_default_iterations : int
val perturb_default_perturb : [> `Ratchet of float * int ]
val perturb_transform : 'a list
val perturb_default :
  'a list * [> `Ratchet of float * int ] * Methods.local_optimum * int *
  'b option
val transform_perturb :
  ([> `AlphabeticTerminals
    | `Annchrom_to_Breakinv of [> Methods.characters ] * 'b
    | `Assign_Affine_Gap_Cost of 'c * [> Methods.characters ]
    | `Assign_Prep_Cost of
        [> `Array of 'd | `File of [> `Local of 'e ] ] *
        [> Methods.characters ]
    | `Assign_Tail_Cost of
        [> `Array of 'f | `File of [> `Local of 'g ] ] *
        [> Methods.characters ]
    | `Assign_Transformation_Cost_Matrix of
        [> `Local of 'h ] option * [> Methods.characters ]
    | `Automatic_Sequence_Partition of [> Methods.characters ] * 'i * 'j
    | `Automatic_Static_Aprox of 'k
    | `Breakinv_to_Custom of [> Methods.characters ] * 'l
    | `Change_Dyn_Pam of [> Methods.characters ] * 'm
    | `Chrom_to_Seq of [> Methods.characters ] * 'n
    | `Create_Transformation_Cost_Matrix of 'o * 'p * [> Methods.characters ]
    | `Custom_to_Breakinv of [> Methods.characters ] * 'q
    | `Direct_Optimization of [> Methods.characters ]
    | `Fixed_States of [> Methods.characters ]
    | `MultiStatic_Aprox of [> Methods.characters ] * 'r
    | `OriginCost of 's
    | `Partitioned of [> Methods.characters ]
    | `Prealigned_Transform of [> Methods.characters ]
    | `Prioritize
    | `RandomizedTerminals
    | `ReWeight of [> Methods.characters ] * 't
    | `Search_Based of [> Methods.characters ]
    | `Seq_to_Chrom of [> Methods.characters ] * 'u
    | `Static_Aprox of [> Methods.characters ] * 'v
    | `WeightFactor of [> Methods.characters ] * 'w ]
   as 'a)
  list *
  ([> `Ratchet of 'y | `Resample of [> `Characters of 'z | `Taxa of 'z ] ]
   as 'x) *
  ([> `LocalOptimum of
        [> swap_strategy ] * float * int * [> Methods.keep_method ] *
        [> `AlphabeticTerminals
         | `Annchrom_to_Breakinv of
             [> Methods.characters ] * chromosome_args list
         | `Assign_Affine_Gap_Cost of int * [> Methods.characters ]
         | `Assign_Prep_Cost of
             [> `Array of int array | `File of [> `Local of string ] ] *
             [> Methods.characters ]
         | `Assign_Tail_Cost of
             [> `Array of int array | `File of [> `Local of string ] ] *
             [> Methods.characters ]
         | `Assign_Transformation_Cost_Matrix of
             [> `Local of string ] option * [> Methods.characters ]
         | `Automatic_Sequence_Partition of
             [> Methods.characters ] * bool * int option
         | `Automatic_Static_Aprox of bool
         | `Breakinv_to_Custom of
             [> Methods.characters ] * chromosome_args list
         | `Change_Dyn_Pam of [> Methods.characters ] * chromosome_args list
         | `Chrom_to_Seq of [> Methods.characters ] * chromosome_args list
         | `Create_Transformation_Cost_Matrix of
             int * int * [> Methods.characters ]
         | `Custom_to_Breakinv of
             [> Methods.characters ] * chromosome_args list
         | `Direct_Optimization of [> Methods.characters ]
         | `Fixed_States of [> Methods.characters ]
         | `MultiStatic_Aprox of [> Methods.characters ] * bool
         | `OriginCost of float
         | `Partitioned of [> Methods.characters ]
         | `Prealigned_Transform of [> Methods.characters ]
         | `Prioritize
         | `RandomizedTerminals
         | `ReWeight of [> Methods.characters ] * float
         | `Search_Based of [> Methods.characters ]
         | `Seq_to_Chrom of [> Methods.characters ] * chromosome_args list
         | `Static_Aprox of [> Methods.characters ] * bool
         | `WeightFactor of [> Methods.characters ] * float ]
        list * float option * [> swap_trajectory ] *
        [> Methods.tabu_break_strategy ] * [> Methods.tabu_join_strategy ] *
        [> `Bfs of int option ] * [> Methods.samples ] list ]
   as 'a1) *
  'b1 * 'c1 option ->
  [< `Iterations of 'b1
   | `Ratchet of 'y
   | `Resample of 'z * [< `Characters | `Taxa ]
   | `Swap of swapa list
   | `TimeOut of 'c1
   | `Transform of
       ([< `All
         | `AllDynamic
         | `AllStatic
         | `Files of 'd1
         | `Missing of bool * int
         | `Names of bool * string list
         | `Random of float
         | `Some of bool * int list ] *
        [< `AffGap of 'c
         | `AlphabeticTerminals
         | `AnnchromToBreakinv of 'b
         | `Automatic_Sequence_Partition of 'i * 'j
         | `Automatic_Static_Aprox of 'k
         | `BreakinvToSeq of 'l
         | `ChangeDynPam of 'm
         | `ChromToSeq of 'n
         | `CustomToBreakinv of 'q
         | `Direct_Optimization
         | `Fixed_States
         | `Gap of 'o * 'p
         | `MultiStaticApproximation of 'r
         | `OriginCost of 's
         | `Prealigned_Transform
         | `PrepFile of 'e
         | `PrepInput of 'd
         | `Prioritize
         | `RandomizedTerminals
         | `ReWeight of 't
         | `SearchBased
         | `SeqToChrom of 'u
         | `StaticApproximation of 'v
         | `TailFile of 'g
         | `TailInput of 'f
         | `Tcm of 'h
         | `WeightFactor of 'w ])
       list ] ->
  'a list * 'x * 'a1 * 'b1 * 'c1 option
val transform_perturb_arguments :
  [< `Iterations of int
   | `Ratchet of float * int
   | `Resample of int * [< `Characters | `Taxa ]
   | `Swap of swapa list
   | `TimeOut of Methods.timer
   | `Transform of
       ([< `All
         | `AllDynamic
         | `AllStatic
         | `Files of 'a
         | `Missing of bool * int
         | `Names of bool * string list
         | `Random of float
         | `Some of bool * int list ] *
        [< `AffGap of int
         | `AlphabeticTerminals
         | `AnnchromToBreakinv of Methods.chromosome_pam_t list
         | `Automatic_Sequence_Partition of bool * int option
         | `Automatic_Static_Aprox of bool
         | `BreakinvToSeq of Methods.chromosome_pam_t list
         | `ChangeDynPam of Methods.chromosome_pam_t list
         | `ChromToSeq of Methods.chromosome_pam_t list
         | `CustomToBreakinv of Methods.chromosome_pam_t list
         | `Direct_Optimization
         | `Fixed_States
         | `Gap of int * int
         | `MultiStaticApproximation of bool
         | `OriginCost of float
         | `Prealigned_Transform
         | `PrepFile of string
         | `PrepInput of int array
         | `Prioritize
         | `RandomizedTerminals
         | `ReWeight of float
         | `SearchBased
         | `SeqToChrom of Methods.chromosome_pam_t list
         | `StaticApproximation of bool
         | `TailFile of string
         | `TailInput of int array
         | `Tcm of string
         | `WeightFactor of float ])
       list ]
  list -> Methods.script list
val support_default_swap :
  [> `LocalOptimum of
       [> `ChainNeighborhoods of [> `Tbr ] ] * float * int * [> `Last ] *
       'a list * 'b option * [> `BestFirst ] * [> `DistanceSorted of bool ] *
       [> `UnionBased of 'c option ] * [> `Bfs of 'd option ] * 'e list ]
val support_select : float
val support_resamplings : int
val support_default_build :
  int *
  [> `Wagner_Rnd of
       int * float * [> `Last ] * 'a list * [> `UnionBased of 'b option ] ] *
  'c list
val support_default :
  [> `Bremer ] *
  (float * int *
   [> `LocalOptimum of
        [> `ChainNeighborhoods of [> `Tbr ] ] * float * int * [> `Last ] *
        'a list * 'b option * [> `BestFirst ] *
        [> `DistanceSorted of bool ] * [> `UnionBased of 'c option ] *
        [> `Bfs of 'd option ] * 'e list ] *
   (int *
    [> `Wagner_Rnd of
         int * float * [> `Last ] * 'f list * [> `UnionBased of 'g option ] ] *
    'h list))
val transform_support :
  ([> `Bootstrap | `Bremer | `Jackknife ] as 'a) *
  ('b * 'c *
   ([> `LocalOptimum of
         [> swap_strategy ] * float * int * [> Methods.keep_method ] *
         [> `AlphabeticTerminals
          | `Annchrom_to_Breakinv of
              [> Methods.characters ] * chromosome_args list
          | `Assign_Affine_Gap_Cost of int * [> Methods.characters ]
          | `Assign_Prep_Cost of
              [> `Array of int array | `File of [> `Local of string ] ] *
              [> Methods.characters ]
          | `Assign_Tail_Cost of
              [> `Array of int array | `File of [> `Local of string ] ] *
              [> Methods.characters ]
          | `Assign_Transformation_Cost_Matrix of
              [> `Local of string ] option * [> Methods.characters ]
          | `Automatic_Sequence_Partition of
              [> Methods.characters ] * bool * int option
          | `Automatic_Static_Aprox of bool
          | `Breakinv_to_Custom of
              [> Methods.characters ] * chromosome_args list
          | `Change_Dyn_Pam of [> Methods.characters ] * chromosome_args list
          | `Chrom_to_Seq of [> Methods.characters ] * chromosome_args list
          | `Create_Transformation_Cost_Matrix of
              int * int * [> Methods.characters ]
          | `Custom_to_Breakinv of
              [> Methods.characters ] * chromosome_args list
          | `Direct_Optimization of [> Methods.characters ]
          | `Fixed_States of [> Methods.characters ]
          | `MultiStatic_Aprox of [> Methods.characters ] * bool
          | `OriginCost of float
          | `Partitioned of [> Methods.characters ]
          | `Prealigned_Transform of [> Methods.characters ]
          | `Prioritize
          | `RandomizedTerminals
          | `ReWeight of [> Methods.characters ] * float
          | `Search_Based of [> Methods.characters ]
          | `Seq_to_Chrom of [> Methods.characters ] * chromosome_args list
          | `Static_Aprox of [> Methods.characters ] * bool
          | `WeightFactor of [> Methods.characters ] * float ]
         list * float option * [> swap_trajectory ] *
         [> Methods.tabu_break_strategy ] * [> Methods.tabu_join_strategy ] *
         [> `Bfs of int option ] * [> Methods.samples ] list ]
    as 'd) *
   (int * Methods.build_method * Methods.transform list)) ->
  [< `Bootstrap of 'c option
   | `Bremer
   | `Build of
       [< `AllBased of int option
        | `Branch_and_Bound of float option
        | `Constraint of string option
        | `DistancesRnd
        | `First
        | `Keep_Random
        | `Last
        | `Lookahead of int
        | `Mst
        | `Ordered
        | `Partition of
            [ `ConstraintFile of Methods.filename
            | `MaxDepth of int
            | `Sets of All_sets.IntSet.t Lazy.t ] list
        | `Prebuilt of Methods.filename
        | `Random
        | `RandomTree
        | `Threshold of float
        | `Transform of
            ([< `All
              | `AllDynamic
              | `AllStatic
              | `Files of 'e
              | `Missing of bool * int
              | `Names of bool * string list
              | `Random of float
              | `Some of bool * int list ] *
             [< `AffGap of int
              | `AlphabeticTerminals
              | `AnnchromToBreakinv of Methods.chromosome_pam_t list
              | `Automatic_Sequence_Partition of bool * int option
              | `Automatic_Static_Aprox of bool
              | `BreakinvToSeq of Methods.chromosome_pam_t list
              | `ChangeDynPam of Methods.chromosome_pam_t list
              | `ChromToSeq of Methods.chromosome_pam_t list
              | `CustomToBreakinv of Methods.chromosome_pam_t list
              | `Direct_Optimization
              | `Fixed_States
              | `Gap of int * int
              | `MultiStaticApproximation of bool
              | `OriginCost of float
              | `Partitioned
              | `Prealigned_Transform
              | `PrepFile of string
              | `PrepInput of int array
              | `Prioritize
              | `RandomizedTerminals
              | `ReWeight of float
              | `SearchBased
              | `SeqToChrom of Methods.chromosome_pam_t list
              | `StaticApproximation of bool
              | `TailFile of string
              | `TailInput of int array
              | `Tcm of string
              | `WeightFactor of float ])
            list
        | `Trees of int
        | `UnionBased of int option ]
       list
   | `Jackknife of [< `Resample of 'c | `Select of 'b ] list
   | `Swap of swapa list ] ->
  'a * ('b * 'c * 'd * (int * Methods.build_method * Methods.transform list))
val transform_support_arguments :
  [< `Bootstrap of int option
   | `Bremer
   | `Build of
       [< `AllBased of int option
        | `Branch_and_Bound of float option
        | `Constraint of string option
        | `DistancesRnd
        | `First
        | `Keep_Random
        | `Last
        | `Lookahead of int
        | `Mst
        | `Ordered
        | `Partition of
            [ `ConstraintFile of Methods.filename
            | `MaxDepth of int
            | `Sets of All_sets.IntSet.t Lazy.t ] list
        | `Prebuilt of Methods.filename
        | `Random
        | `RandomTree
        | `Threshold of float
        | `Transform of
            ([< `All
              | `AllDynamic
              | `AllStatic
              | `Files of 'a
              | `Missing of bool * int
              | `Names of bool * string list
              | `Random of float
              | `Some of bool * int list ] *
             [< `AffGap of int
              | `AlphabeticTerminals
              | `AnnchromToBreakinv of Methods.chromosome_pam_t list
              | `Automatic_Sequence_Partition of bool * int option
              | `Automatic_Static_Aprox of bool
              | `BreakinvToSeq of Methods.chromosome_pam_t list
              | `ChangeDynPam of Methods.chromosome_pam_t list
              | `ChromToSeq of Methods.chromosome_pam_t list
              | `CustomToBreakinv of Methods.chromosome_pam_t list
              | `Direct_Optimization
              | `Fixed_States
              | `Gap of int * int
              | `MultiStaticApproximation of bool
              | `OriginCost of float
              | `Partitioned
              | `Prealigned_Transform
              | `PrepFile of string
              | `PrepInput of int array
              | `Prioritize
              | `RandomizedTerminals
              | `ReWeight of float
              | `SearchBased
              | `SeqToChrom of Methods.chromosome_pam_t list
              | `StaticApproximation of bool
              | `TailFile of string
              | `TailInput of int array
              | `Tcm of string
              | `WeightFactor of float ])
            list
        | `Trees of int
        | `UnionBased of int option ]
       list
   | `Jackknife of [< `Resample of int | `Select of float ] list
   | `Swap of swapa list ]
  list ->
  [> `Bootstrap of
       int *
       ([> `LocalOptimum of
             [> swap_strategy ] * float * int * [> Methods.keep_method ] *
             [> `AlphabeticTerminals
              | `Annchrom_to_Breakinv of
                  [> Methods.characters ] * chromosome_args list
              | `Assign_Affine_Gap_Cost of int * [> Methods.characters ]
              | `Assign_Prep_Cost of
                  [> `Array of int array | `File of [> `Local of string ] ] *
                  [> Methods.characters ]
              | `Assign_Tail_Cost of
                  [> `Array of int array | `File of [> `Local of string ] ] *
                  [> Methods.characters ]
              | `Assign_Transformation_Cost_Matrix of
                  [> `Local of string ] option * [> Methods.characters ]
              | `Automatic_Sequence_Partition of
                  [> Methods.characters ] * bool * int option
              | `Automatic_Static_Aprox of bool
              | `Breakinv_to_Custom of
                  [> Methods.characters ] * chromosome_args list
              | `Change_Dyn_Pam of
                  [> Methods.characters ] * chromosome_args list
              | `Chrom_to_Seq of
                  [> Methods.characters ] * chromosome_args list
              | `Create_Transformation_Cost_Matrix of
                  int * int * [> Methods.characters ]
              | `Custom_to_Breakinv of
                  [> Methods.characters ] * chromosome_args list
              | `Direct_Optimization of [> Methods.characters ]
              | `Fixed_States of [> Methods.characters ]
              | `MultiStatic_Aprox of [> Methods.characters ] * bool
              | `OriginCost of float
              | `Partitioned of [> Methods.characters ]
              | `Prealigned_Transform of [> Methods.characters ]
              | `Prioritize
              | `RandomizedTerminals
              | `ReWeight of [> Methods.characters ] * float
              | `Search_Based of [> Methods.characters ]
              | `Seq_to_Chrom of
                  [> Methods.characters ] * chromosome_args list
              | `Static_Aprox of [> Methods.characters ] * bool
              | `WeightFactor of [> Methods.characters ] * float ]
             list * float option * [> swap_trajectory ] *
             [> Methods.tabu_break_strategy ] *
             [> Methods.tabu_join_strategy ] * [> `Bfs of int option ] *
             [> Methods.samples ] list ]
        as 'b) *
       [> `Build of int * Methods.build_method * Methods.transform list ] *
       'c option
   | `Bremer of
       'b *
       [> `Build of int * Methods.build_method * Methods.transform list ] *
       int * int
   | `Jackknife of
       float * int * 'b *
       [> `Build of int * Methods.build_method * Methods.transform list ] *
       'd option ]
val transform_report :
  Methods.script list * string option ->
  reporta -> Methods.script list * string option
val transform_report_arguments : reporta list -> Methods.script list
val transform_select :
  [ `Characters | `Taxa ] * Methods.script list ->
  [> `AllDynamic
   | `AllStatic
   | `BestN of int option
   | `BestWithin of float
   | `Characters
   | `Files of bool * string list
   | `Missing of bool * int
   | `Names of bool * string list
   | `Random of float
   | `RandomTrees of int
   | `Taxa
   | `Unique ] ->
  [ `Characters | `Taxa ] * Methods.script list
val transform_select_arguments :
  [> `AllDynamic
   | `AllStatic
   | `BestN of int option
   | `BestWithin of float
   | `Characters
   | `Files of bool * string list
   | `Missing of bool * int
   | `Names of bool * string list
   | `Random of float
   | `RandomTrees of int
   | `Taxa
   | `Unique ]
  list -> Methods.script list
val transform_rename :
  [ `Characters | `Taxa ] * Methods.filename list * 'a list *
  ([> `RenameCharacters of 'a list
    | `Synonyms of 'a list
    | `SynonymsFile of Methods.filename list ]
   as 'b)
  list ->
  [< `Characters | `File of string | `Syn of 'a | `Taxa ] ->
  [ `Characters | `Taxa ] * Methods.filename list * 'a list * 'b list
val transform_rename_arguments :
  [< `Characters | `File of string | `Syn of 'a | `Taxa ] list ->
  [> `RenameCharacters of 'a list
   | `Synonyms of 'a list
   | `SynonymsFile of Methods.filename list ]
  list
val default_search : Methods.script list
val transform_search :
  [> `Build of bool | `Transform of bool ] list -> Methods.script list
val transform_stdsearch :
  [< `ConstraintFile of 'a
   | `MaxRam of 'b
   | `MaxTime of 'c
   | `MinHits of 'd
   | `MinTime of 'e
   | `Target of 'f
   | `Visited of 'g ]
  list ->
  [> `StandardSearch of
       'c option * 'e option * 'd option * 'b option * 'f option *
       'g option * 'a option ]
val transform_command : Methods.script list -> command -> Methods.script list
val transform_all_commands : command list -> Methods.script list
val to_local : 'a list -> [> `Local of 'a ] list
module Gram :
  sig
    module Loc :
      sig
        type t = CommandLexer.Lexer.Loc.t
        val mk : string -> t
        val ghost : t
        val of_lexing_position : Lexing.position -> t
        val to_ocaml_location : t -> Camlp4_import.Location.t
        val of_ocaml_location : Camlp4_import.Location.t -> t
        val of_lexbuf : Lexing.lexbuf -> t
        val of_tuple : string * int * int * int * int * int * int * bool -> t
        val to_tuple : t -> string * int * int * int * int * int * int * bool
        val merge : t -> t -> t
        val join : t -> t
        val move : [ `both | `start | `stop ] -> int -> t -> t
        val shift : int -> t -> t
        val move_line : int -> t -> t
        val file_name : t -> string
        val start_line : t -> int
        val stop_line : t -> int
        val start_bol : t -> int
        val stop_bol : t -> int
        val start_off : t -> int
        val stop_off : t -> int
        val start_pos : t -> Lexing.position
        val stop_pos : t -> Lexing.position
        val is_ghost : t -> bool
        val ghostify : t -> t
        val set_file_name : string -> t -> t
        val strictly_before : t -> t -> bool
        val make_absolute : t -> t
        val print : Format.formatter -> t -> unit
        val dump : Format.formatter -> t -> unit
        val to_string : t -> string
        exception Exc_located of t * exn
        val raise : t -> exn -> 'a
        val name : string ref
      end
    module Action :
      sig
        type t =
            Camlp4.Struct.Grammar.Static.Make(CommandLexer.Lexer).Action.t
        val mk : 'a -> t
        val get : t -> 'a
        val getf : t -> 'a -> 'b
        val getf2 : t -> 'a -> 'b -> 'c
      end
    module Token :
      sig
        module Loc :
          sig
            type t = CommandLexer.Lexer.Loc.t
            val mk : string -> t
            val ghost : t
            val of_lexing_position : Lexing.position -> t
            val to_ocaml_location : t -> Camlp4_import.Location.t
            val of_ocaml_location : Camlp4_import.Location.t -> t
            val of_lexbuf : Lexing.lexbuf -> t
            val of_tuple :
              string * int * int * int * int * int * int * bool -> t
            val to_tuple :
              t -> string * int * int * int * int * int * int * bool
            val merge : t -> t -> t
            val join : t -> t
            val move : [ `both | `start | `stop ] -> int -> t -> t
            val shift : int -> t -> t
            val move_line : int -> t -> t
            val file_name : t -> string
            val start_line : t -> int
            val stop_line : t -> int
            val start_bol : t -> int
            val stop_bol : t -> int
            val start_off : t -> int
            val stop_off : t -> int
            val start_pos : t -> Lexing.position
            val stop_pos : t -> Lexing.position
            val is_ghost : t -> bool
            val ghostify : t -> t
            val set_file_name : string -> t -> t
            val strictly_before : t -> t -> bool
            val make_absolute : t -> t
            val print : Format.formatter -> t -> unit
            val dump : Format.formatter -> t -> unit
            val to_string : t -> string
            exception Exc_located of t * exn
            val raise : t -> exn -> 'a
            val name : string ref
          end
        type t = CommandLexer.Lexer.Token.t
        val to_string : t -> string
        val print : Format.formatter -> t -> unit
        val match_keyword : string -> t -> bool
        val extract_string : t -> string
        module Filter :
          sig
            type token_filter = (t, Loc.t) Camlp4.Sig.stream_filter
            type t = CommandLexer.Lexer.Token.Filter.t
            val mk : (string -> bool) -> t
            val define_filter : t -> (token_filter -> token_filter) -> unit
            val filter : t -> token_filter
            val keyword_added : t -> string -> bool -> unit
            val keyword_removed : t -> string -> unit
          end
        module Error :
          sig
            type t = CommandLexer.Lexer.Token.Error.t
            exception E of t
            val to_string : t -> string
            val print : Format.formatter -> t -> unit
          end
      end
    type gram = Camlp4.Struct.Grammar.Static.Make(CommandLexer.Lexer).gram
    type internal_entry =
        Camlp4.Struct.Grammar.Static.Make(CommandLexer.Lexer).internal_entry
    type tree = Camlp4.Struct.Grammar.Static.Make(CommandLexer.Lexer).tree
    type token_pattern = (Token.t -> bool) * string
    type symbol =
      Camlp4.Struct.Grammar.Static.Make(CommandLexer.Lexer).symbol =
        Smeta of string * symbol list * Action.t
      | Snterm of internal_entry
      | Snterml of internal_entry * string
      | Slist0 of symbol
      | Slist0sep of symbol * symbol
      | Slist1 of symbol
      | Slist1sep of symbol * symbol
      | Sopt of symbol
      | Sself
      | Snext
      | Stoken of token_pattern
      | Skeyword of string
      | Stree of tree
    type production_rule = symbol list * Action.t
    type single_extend_statment =
        string option * Camlp4.Sig.Grammar.assoc option *
        production_rule list
    type extend_statment =
        Camlp4.Sig.Grammar.position option * single_extend_statment list
    type delete_statment = symbol list
    type ('a, 'b, 'c) fold =
        internal_entry ->
        symbol list -> ('a Stream.t -> 'b) -> 'a Stream.t -> 'c
    type ('a, 'b, 'c) foldsep =
        internal_entry ->
        symbol list ->
        ('a Stream.t -> 'b) -> ('a Stream.t -> unit) -> 'a Stream.t -> 'c
    module Entry :
      sig
        type 'a t =
            'a Camlp4.Struct.Grammar.Static.Make(CommandLexer.Lexer).Entry.t
        val mk : string -> 'a t
        val of_parser : string -> ((Token.t * Loc.t) Stream.t -> 'a) -> 'a t
        val setup_parser : 'a t -> ((Token.t * Loc.t) Stream.t -> 'a) -> unit
        val name : 'a t -> string
        val print : Format.formatter -> 'a t -> unit
        val dump : Format.formatter -> 'a t -> unit
        val obj : 'a t -> internal_entry
        val clear : 'a t -> unit
      end
    val get_filter : unit -> Token.Filter.t
    type 'a not_filtered =
        'a Camlp4.Struct.Grammar.Static.Make(CommandLexer.Lexer).not_filtered
    val extend : 'a Entry.t -> extend_statment -> unit
    val delete_rule : 'a Entry.t -> delete_statment -> unit
    val srules : 'a Entry.t -> (symbol list * Action.t) list -> symbol
    val sfold0 : ('a -> 'b -> 'b) -> 'b -> ('c, 'a, 'b) fold
    val sfold1 : ('a -> 'b -> 'b) -> 'b -> ('c, 'a, 'b) fold
    val sfold0sep : ('a -> 'b -> 'b) -> 'b -> ('c, 'a, 'b) foldsep
    val lex :
      Loc.t -> char Stream.t -> (Token.t * Loc.t) Stream.t not_filtered
    val lex_string :
      Loc.t -> string -> (Token.t * Loc.t) Stream.t not_filtered
    val filter :
      (Token.t * Loc.t) Stream.t not_filtered -> (Token.t * Loc.t) Stream.t
    val parse : 'a Entry.t -> Loc.t -> char Stream.t -> 'a
    val parse_string : 'a Entry.t -> Loc.t -> string -> 'a
    val parse_tokens_before_filter :
      'a Entry.t -> (Token.t * Loc.t) Stream.t not_filtered -> 'a
    val parse_tokens_after_filter :
      'a Entry.t -> (Token.t * Loc.t) Stream.t -> 'a
  end
val create_expr : unit -> command list Gram.Entry.t
val ( --> ) : 'a -> ('a -> 'b) -> 'b
val process_commands : bool -> Methods.script -> Methods.script list
val read_script_files : bool -> string list -> Methods.script list
val do_analysis : bool -> Methods.script list -> Methods.script list
val simplify_directory : string -> string
val of_parsed : bool -> command list -> Methods.script list
val of_stream : bool -> char Stream.t -> Methods.script list
val of_channel : bool -> in_channel -> Methods.script list
val of_file : bool -> string -> Methods.script list
val of_string : bool -> string -> Methods.script list
