exception TimedOut
val do_job : int
val process_management : int
val io : int
val debugging : int
val barrier : int
type cost_modes =
    [ `Exhaustive_Strong
    | `Exhaustive_Weak
    | `Iterative of [ `ApproxD of int option | `ThreeD of int option ]
    | `Normal
    | `Normal_plus_Vitamines ]
val cost : cost_modes ref
type filename = [ `Local of string | `Remote of string ]
type support_tree = Leaf of int | Node of float * support_tree * support_tree
type orientation_t = [ `NoOrientation | `Orintation ]
type init3D_t = [ `Init3D | `NoInit3D ]
type read_option_t = [ `Init3D of bool | `Orientation of bool ]
type prealigned_costs =
    [ `Assign_Transformation_Cost_Matrix of filename
    | `Create_Transformation_Cost_Matrix of int * int ]
type simple_input =
    [ `Aminoacids of filename list
    | `AutoDetect of filename list
    | `Breakinv of filename * filename * read_option_t list
    | `Chromosome of filename list
    | `ComplexTerminals of filename list
    | `GeneralAlphabetSeq of filename * filename * read_option_t list
    | `Genome of filename list
    | `Nucleotides of filename list
    | `PartitionedFile of filename list
    | `Poyfile of filename list ]
type input =
    [ `Aminoacids of filename list
    | `AnnotatedFiles of simple_input list
    | `AutoDetect of filename list
    | `Breakinv of filename * filename * read_option_t list
    | `Chromosome of filename list
    | `ComplexTerminals of filename list
    | `GeneralAlphabetSeq of filename * filename * read_option_t list
    | `Genome of filename list
    | `Nucleotides of filename list
    | `PartitionedFile of filename list
    | `Poyfile of filename list
    | `Prealigned of simple_input * prealigned_costs ]
type information_contained =
    [ `Collapse of bool
    | `Cost
    | `HennigStyle
    | `Margin of int
    | `Newick
    | `Nothing
    | `Total ]
type taxon_and_characters =
    [ `Missing of bool * int
    | `Names of bool * string list
    | `Random of float ]
type characters =
    [ `All
    | `AllDynamic
    | `AllStatic
    | `Missing of bool * int
    | `Names of bool * string list
    | `Random of float
    | `Some of bool * int list ]
type prep_tail_spec = [ `Array of int array | `File of filename ]
type transform_cost_matrix =
    [ `Assign_Affine_Gap_Cost of int * characters
    | `Assign_Prep_Cost of prep_tail_spec * characters
    | `Assign_Tail_Cost of prep_tail_spec * characters
    | `Assign_Transformation_Cost_Matrix of filename option * characters
    | `Create_Transformation_Cost_Matrix of int * int * characters ]
type chromosome_pam_t =
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
type dynamic_char_transform =
    [ `Annchrom_to_Breakinv of characters * chromosome_pam_t list
    | `Breakinv_to_Custom of characters * chromosome_pam_t list
    | `Change_Dyn_Pam of characters * chromosome_pam_t list
    | `Chrom_to_Seq of characters * chromosome_pam_t list
    | `Custom_to_Breakinv of characters * chromosome_pam_t list
    | `Seq_to_Chrom of characters * chromosome_pam_t list ]
type terminal_transform = [ `AlphabeticTerminals | `RandomizedTerminals ]
type char_transform =
    [ `Annchrom_to_Breakinv of characters * chromosome_pam_t list
    | `Assign_Affine_Gap_Cost of int * characters
    | `Assign_Prep_Cost of prep_tail_spec * characters
    | `Assign_Tail_Cost of prep_tail_spec * characters
    | `Assign_Transformation_Cost_Matrix of filename option * characters
    | `Automatic_Sequence_Partition of characters * bool * int option
    | `Automatic_Static_Aprox of bool
    | `Breakinv_to_Custom of characters * chromosome_pam_t list
    | `Change_Dyn_Pam of characters * chromosome_pam_t list
    | `Chrom_to_Seq of characters * chromosome_pam_t list
    | `Create_Transformation_Cost_Matrix of int * int * characters
    | `Custom_to_Breakinv of characters * chromosome_pam_t list
    | `Direct_Optimization of characters
    | `Fixed_States of characters
    | `Partitioned of ([`Clip | `NoClip] * characters)
    | `MultiStatic_Aprox of characters * bool
    | `Prealigned_Transform of characters
    | `Prioritize
    | `ReWeight of characters * float
    | `Search_Based of characters
    | `Seq_to_Chrom of characters * chromosome_pam_t list
    | `Static_Aprox of characters * bool
    | `WeightFactor of characters * float ]
type transform =
    [ `AlphabeticTerminals
    | `Annchrom_to_Breakinv of characters * chromosome_pam_t list
    | `Assign_Affine_Gap_Cost of int * characters
    | `Assign_Prep_Cost of prep_tail_spec * characters
    | `Assign_Tail_Cost of prep_tail_spec * characters
    | `Assign_Transformation_Cost_Matrix of filename option * characters
    | `Automatic_Sequence_Partition of characters * bool * int option
    | `Automatic_Static_Aprox of bool
    | `Breakinv_to_Custom of characters * chromosome_pam_t list
    | `Change_Dyn_Pam of characters * chromosome_pam_t list
    | `Chrom_to_Seq of characters * chromosome_pam_t list
    | `Create_Transformation_Cost_Matrix of int * int * characters
    | `Custom_to_Breakinv of characters * chromosome_pam_t list
    | `Direct_Optimization of characters
    | `Fixed_States of characters
    | `MultiStatic_Aprox of characters * bool
    | `OriginCost of float
    | `Partitioned of ([`Clip | `NoClip] * characters)
    | `Prealigned_Transform of characters
    | `Prioritize
    | `RandomizedTerminals
    | `ReWeight of characters * float
    | `Search_Based of characters
    | `Seq_to_Chrom of characters * chromosome_pam_t list
    | `Static_Aprox of characters * bool
    | `WeightFactor of characters * float ]
type cost_calculation = transform
type diagnosis =
    [ `AllRootsCost of string option
    | `Implied_Alignment of string option * characters * bool ]
type summary_class = [ `Consensus | `Individual | `InputFile of string ]
type support_output =
    [ `Bootstrap of summary_class
    | `Bremer of filename list option
    | `Jackknife of summary_class ]
type report =
    [ `AllRootsCost of string option
    | `Ci of string option * characters option
    | `Clades of string
    | `CompareSequences of string option * bool * characters * characters
    | `Consensus of string option * float option
    | `CrossReferences of characters option * string option
    | `Dataset of string option
    | `Diagnosis of string option
    | `ExplainScript of string * string option
    | `FasWinClad of string option
    | `GraphicConsensus of string option * float option
    | `GraphicDiagnosis of string 
    | `GraphicSupports of support_output option * string option
    | `Implied_Alignment of string option * characters * bool
    | `Load of string
    | `MstR of string option
    | `Nodes of string option
    | `Ri of string option * characters option
    | `Root of int option
    | `RootName of string
    | `Save of string * string option
    | `SearchStats of string option
    | `SequenceStats of string option * characters
    | `Supports of support_output option * string option
    | `TerminalsFiles of string option
    | `TimeDelta of string * string option
    | `TreeCosts of string option
    | `Trees of information_contained list * string option
    | `TreesStats of string option
    | `Xslt of string * string ]
type keep_method = [ `First | `Keep_Random | `Last ]
type store_class = [ `Bootstrap | `Bremer | `Data | `Jackknife | `Trees ]
type runtime_store =
    [ `Discard of store_class list * string
    | `Keep_only of int * keep_method
    | `Set of store_class list * string
    | `Store of store_class list * string ]
type ('a, 'b) character_input_output = [ `Characters of 'a | `Floats of 'b ]
type ia_seq = [ `DO of int array | `First of int array | `Last of int array ]
type implied_alignment =
    (((int * ia_seq array All_sets.IntegerMap.t list) list *
      (int * string * int * [ `Deletion | `Insertion ] * int Sexpr.t) Sexpr.t
      list list) *
     (int * int Sexpr.t) Sexpr.t list list)
    list
type ('a, 'b, 'c, 'd) parallel_input =
    [ `Characters of 'c
    | `Data of 'b * int
    | `DataNTrees of 'b * int * 'a
    | `Floats of 'd
    | `Random_Seed of int
    | `Support of support_tree Sexpr.t
    | `Trees of 'a ]
type taxa = characters
type parallel_special_condition =
    [ `Caught of int * string * exn
    | `CleanExit of int
    | `Cleanup
    | `Uncaught of int * string * exn
    | `Unknown of int ]
type verbosity = Low | Medium | High
type io =
    [ `Error of string * int
    | `Information of string * int
    | `Output of string * int
    | `Status of string * int ]
type output_class = [ `Error | `Information | `Output of string option ]
type tabu_join_strategy =
    [ `AllBased of int option
    | `Partition of
        [ `ConstraintFile of filename
        | `MaxDepth of int
        | `Sets of All_sets.IntSet.t Lazy.t ] list
    | `UnionBased of int option ]
type build_strategy =
    int * float * keep_method * cost_calculation list * tabu_join_strategy
type build_method =
    [ `Branch_and_Bound of
        float option * float option * keep_method * int *
        cost_calculation list
    | `Build_Random of build_strategy
    | `Constraint of int * float * filename option * cost_calculation list
    | `Prebuilt of filename
    | `Wagner_Distances of build_strategy
    | `Wagner_Mst of build_strategy
    | `Wagner_Ordered of build_strategy
    | `Wagner_Rnd of build_strategy ]
type parallelizable_build = build_method
type build =
    [ `Branch_and_Bound of
        float option * float option * keep_method * int *
        cost_calculation list
    | `Build of int * build_method * cost_calculation list
    | `Build_Random of build_strategy
    | `Prebuilt of filename ]
type optimality_criterion = [ `Likelihood | `Parsimony ]
type neighborhood = [ `Spr | `Tbr ]
type search_space =
    [ `Alternate of neighborhood * neighborhood
    | `ChainNeighborhoods of neighborhood
    | `None
    | `SingleNeighborhood of neighborhood ]
type tabu_break_strategy =
    [ `DistanceSorted of bool | `OnlyOnce | `Randomized ]
type tabu_reroot_strategy = [ `Bfs of int option ]
type origin_cost = float option
type trajectory_method =
    [ `AllAround of string option
    | `AllThenChoose
    | `Annealing of float * float
    | `BestFirst
    | `PoyDrifting of float * float ]
type timer = [ `Dynamic of unit -> float | `Fixed of float ]
type samples =
    [ `AllVisited of string option
    | `AttemptsDistr of string option
    | `BreakVsJoin of string option
    | `KeepBestTrees
    | `PrintTrajectory of string option
    | `RootUnionDistr of string option
    | `TimeOut of timer
    | `TimedPrint of float * string option
    | `UnionStats of string option * int ]
type local_optimum =
    [ `LocalOptimum of
        search_space * float * int * keep_method * cost_calculation list *
        origin_cost * trajectory_method * tabu_break_strategy *
        tabu_join_strategy * tabu_reroot_strategy * samples list ]
type tree_weights = [ `Uniform ]
type fusing_keep_method = [ `Best | `Better ]
type driven_search =
    [ `Fusing of
        int option * int option * tree_weights * fusing_keep_method *
        local_optimum * (int * int) ]
type support_method =
    [ `Bootstrap of int * local_optimum * build * int option
    | `Jackknife of float * int * local_optimum * build * int option ]
type bremer_support = [ `Bremer of local_optimum * build * int * int ]
type perturb_method =
    [ `FixImpliedAlignments of characters * bool
    | `Ratchet of float * int
    | `Resample of [ `Characters of int | `Taxa of int ]
    | `UnFixImpliedAlignments
    | `UnRatchet
    | `UnResample of [ `Characters of int | `Taxa of int ] ]
type escape_local =
    [ `PerturbateNSearch of
        transform list * perturb_method * local_optimum * int * timer option ]
type character_weight_method =
    [ `Original_Goloboff of int | `Product_Goloboff of int | `Retention_Index ]
type state_calculation_method =
    [ `Fixed_States of int list
    | `Goloboff_Approximation
    | `Goloboff_Exact
    | `Iterative_Pass of int
    | `Simple_Dynamic ]
type origin_loss_cost = [ `Flat of int * int ]
type complex_term_method =
    [ `Origin_Loss of origin_loss_cost | `Strictly_Same ]
type consensus_method =
    [ `Majority_Rule of int | `Semistrict | `Strict_Majority ]
type consistency_method = Something
val make_consis_arr : unit -> int array
val consistency_enc : consistency_method -> int
val support_enc :
  [< `Bootstrap | `Bremmer of 'a | `Jackknife | `No_support ] -> int
val make_supp_arr : unit -> int array
val consensus_encoding : [< `Majority_Rule of 'a | `Strict_Majority ] -> int
val state_calculation_enc :
  [< `Fixed_States of 'a
   | `Goloboff_Approximation
   | `Goloboff_Exact
   | `Iterative_Pass of 'b
   | `Simple_Dynamic ] ->
  int
val cost_calculation_enc :
  [< `Approximate
   | `Exact
   | `MultiStatic_Aprox
   | `Search_Based
   | `Static_Aprox ] ->
  int
val character_weight_enc :
  [< `Original_Goloboff of 'a | `Product_Goloboff of 'b | `Retention_Index ] ->
  int
val perturb_enc :
  [< `Char_subset of 'a | `No_perturb | `Ratchet of 'b | `Taxon_subset of 'c ] ->
  int
val neighborhood_enc : [< `Itself | `Spr of 'a | `Tbr of 'b ] -> int
val keep_enc : [< `First | `Keep_Random | `Last ] -> int
val optimality_criterion_enc : [< `Likelihood | `Parsimony ] -> int
val build_enc :
  [< `Build_Random of 'a
   | `Input_file of 'b
   | `Wagner_Mst of 'c
   | `Wagner_Ordered of 'd
   | `Wagner_Rnd of 'e ] ->
  int
val default :
  unit ->
  [> `Itself ] * [> `Parsimony ] * [> `Original_Goloboff of int ] *
  [> `Exact ]
type extract_characters = [ `OfNodes of int ]
type 'a character_operations =
    [ `Distance of 'a Sexpr.t * 'a Sexpr.t
    | `Median of 'a Sexpr.t * 'a Sexpr.t ]
type char_operations =
    [ `Distance of (taxa * taxa) * characters
    | `Median of (taxa * taxa) * characters ]
type clear_item = [ `Matrices | `SequencePool ]
type 'a plugin_arguments = 
    [ `Empty
    |`Float of float 
    | `Int of int
    | `String of string
    | `Lident of string
    | `Labled of (string * 'a plugin_arguments)
    | `List of 'a plugin_arguments list 
    | `Command of 'a
]

type application =
    [ `Ascii of string option * bool
    | `KML of (string option * filename * string)
    | `ChangeWDir of string
    | `ClearMemory of clear_item list
    | `ClearRecovered
    | `Echo of string * output_class
    | `Exhaustive_Strong
    | `Exhaustive_Weak
    | `Exit
    | `Graph of string option * bool
    | `Help of string option
    | `HistorySize of int
    | `InspectFile of string
    | `Interactive
    | `Iterative of [ `ApproxD of int option | `ThreeD of int option ]
    | `Logfile of string option
    | `Memory of string option
    | `Normal
    | `Normal_plus_Vitamines
    | `PrintWDir
    | `ReDiagnose
    | `Recover
    | `Redraw
    | `SetSeed of int
    | `TimerInterval of int
    | `Version
    | `Wipe ]
type characters_handling =
    [ `AnalyzeOnlyCharacterFiles of bool * filename list
    | `AnalyzeOnlyCharacters of characters
    | `RenameCharacters of (string * string) list ]
type taxa_handling =
    [ `AnalyzeOnly of taxon_and_characters
    | `AnalyzeOnlyFiles of bool * filename list
    | `Synonyms of (string * string) list
    | `SynonymsFile of filename list ]
type tree_handling =
    [ `BestN of int option
    | `BestWithin of float
    | `RandomTrees of int
    | `Unique ]
type script =
    [ `AllRootsCost of string option
    | `AlphabeticTerminals
    | `Aminoacids of filename list
    | `AnalyzeOnly of taxon_and_characters
    | `AnalyzeOnlyCharacterFiles of bool * filename list
    | `AnalyzeOnlyCharacters of characters
    | `AnalyzeOnlyFiles of bool * filename list
    | `Annchrom_to_Breakinv of characters * chromosome_pam_t list
    | `AnnotatedFiles of simple_input list
    | `Ascii of string option * bool
    | `KML of (string option * filename * string)
    | `Assign_Affine_Gap_Cost of int * characters
    | `Assign_Prep_Cost of prep_tail_spec * characters
    | `Assign_Tail_Cost of prep_tail_spec * characters
    | `Assign_Transformation_Cost_Matrix of filename option * characters
    | `AutoDetect of filename list
    | `Automatic_Sequence_Partition of characters * bool * int option
    | `Automatic_Static_Aprox of bool
    | `Barrier
    | `BestN of int option
    | `BestWithin of float
    | `Bootstrap of int * local_optimum * build * int option
    | `Branch_and_Bound of
        float option * float option * keep_method * int *
        cost_calculation list
    | `Breakinv of filename * filename * read_option_t list
    | `Breakinv_to_Custom of characters * chromosome_pam_t list
    | `Bremer of local_optimum * build * int * int
    | `Build of int * build_method * cost_calculation list
    | `Build_Random of build_strategy
    | `ChangeWDir of string
    | `Change_Dyn_Pam of characters * chromosome_pam_t list
    | `Chrom_to_Seq of characters * chromosome_pam_t list
    | `Chromosome of filename list
    | `Ci of string option * characters option
    | `Clades of string
    | `ClearMemory of clear_item list
    | `ClearRecovered
    | `CompareSequences of string option * bool * characters * characters
    | `ComplexTerminals of filename list
    | `Consensus of string option * float option
    | `Create_Transformation_Cost_Matrix of int * int * characters
    | `CrossReferences of characters option * string option
    | `Custom_to_Breakinv of characters * chromosome_pam_t list
    | `Dataset of string option
    | `Diagnosis of string option
    | `Direct_Optimization of characters
    | `Discard of store_class list * string
    | `Distance of (taxa * taxa) * characters
    | `Echo of string * output_class
    | `Entry
    | `Exhaustive_Strong
    | `Exhaustive_Weak
    | `Exit
    | `ExplainScript of string * string option
    | `FasWinClad of string option
    | `FixImpliedAlignments of characters * bool
    | `Fixed_States of characters
    | `Fusing of
        int option * int option * tree_weights * fusing_keep_method *
        local_optimum * (int * int)
    | `GatherBootstrap
    | `GatherBremer
    | `GatherJackknife
    | `GatherTrees of script list * script list
    | `GeneralAlphabetSeq of filename * filename * read_option_t list
    | `Genome of filename list
    | `GetStored
    | `Graph of string option * bool
    | `GraphicConsensus of string option * float option
    | `GraphicDiagnosis of string 
    | `GraphicSupports of support_output option * string option
    | `Help of string option
    | `HistorySize of int
    | `Implied_Alignment of string option * characters * bool
    | `InspectFile of string
    | `Interactive
    | `Iterative of [ `ApproxD of int option | `ThreeD of int option ]
    | `Jackknife of float * int * local_optimum * build * int option
    | `Keep_only of int * keep_method
    | `Load of string
    | `LocalOptimum of
        search_space * float * int * keep_method * cost_calculation list *
        origin_cost * trajectory_method * tabu_break_strategy *
        tabu_join_strategy * tabu_reroot_strategy * samples list
    | `Logfile of string option
    | `Median of (taxa * taxa) * characters
    | `Memory of string option
    | `MstR of string option
    | `MultiStatic_Aprox of characters * bool
    | `Nodes of string option
    | `Normal
    | `Normal_plus_Vitamines
    | `Nucleotides of filename list
    | `PartitionedFile of filename list
    | `OnEachTree of script list * script list
    | `OriginCost of float
    | `ParallelPipeline of int * script list * script list * script list
    | `Partitioned of ([`Clip | `NoClip] * characters)
    | `PerturbateNSearch of
        transform list * perturb_method * local_optimum * int * timer option
    | `Plugin of (string * script plugin_arguments)
    | `Poyfile of filename list
    | `Prealigned of simple_input * prealigned_costs
    | `Prealigned_Transform of characters
    | `Prebuilt of filename
    | `PrintWDir
    | `Prioritize
    | `RandomTrees of int
    | `RandomizedTerminals
    | `Ratchet of float * int
    | `ReDiagnose
    | `ReWeight of characters * float
    | `ReadScript of string list
    | `Recover
    | `Redraw
    | `RenameCharacters of (string * string) list
    | `Repeat of int * script list
    | `Resample of [ `Characters of int | `Taxa of int ]
    | `Ri of string option * characters option
    | `Root of int option
    | `RootName of string
    | `Save of string * string option
    | `SearchStats of string option
    | `Search_Based of characters
    | `SelectYourTrees
    | `Seq_to_Chrom of characters * chromosome_pam_t list
    | `SequenceStats of string option * characters
    | `Set of store_class list * string
    | `SetSeed of int
    | `Skip
    | `StandardSearch of
        float option * float option * int option * int option *
        float option * string option option * string option
    | `Static_Aprox of characters * bool
    | `Store of store_class list * string
    | `StoreTrees
    | `Supports of support_output option * string option
    | `Synonyms of (string * string) list
    | `SynonymsFile of filename list
    | `TerminalsFiles of string option
    | `TimeDelta of string * string option
    | `TimerInterval of int
    | `TreeCosts of string option
    | `Trees of information_contained list * string option
    | `TreesStats of string option
    | `UnFixImpliedAlignments
    | `UnRatchet
    | `UnResample of [ `Characters of int | `Taxa of int ]
    | `UnionStored
    | `Unique
    | `Version
    | `WeightFactor of characters * float
    | `Wipe
    | `Xslt of string * string ]
type ('a, 'b, 'c, 'd) checkpoints =
    [ `Aminoacids of filename list
    | `AnnotatedFiles of simple_input list
    | `AutoDetect of filename list
    | `Bootstrap of int * local_optimum * build * int option
    | `Branch_and_Bound of
        float option * float option * keep_method * int *
        cost_calculation list
    | `Breakinv of filename * filename * read_option_t list
    | `Build of int * build_method * cost_calculation list
    | `Build_Random of build_strategy
    | `Characters of 'd Sexpr.t
    | `Chromosome of filename list
    | `ComplexTerminals of filename list
    | `Constraint of int * float * filename option * cost_calculation list
    | `Data of 'b * int
    | `DataNTrees of 'b * int * 'a Sexpr.t
    | `Distance of 'd Sexpr.t * 'd Sexpr.t
    | `FixImpliedAlignments of characters * bool
    | `Floats of 'c Sexpr.t
    | `GeneralAlphabetSeq of filename * filename * read_option_t list
    | `Genome of filename list
    | `Item of int * ('a, 'b, 'c, 'd) checkpoints
    | `Jackknife of float * int * local_optimum * build * int option
    | `LocalOptimum of
        search_space * float * int * keep_method * cost_calculation list *
        origin_cost * trajectory_method * tabu_break_strategy *
        tabu_join_strategy * tabu_reroot_strategy * samples list
    | `Median of 'd Sexpr.t * 'd Sexpr.t
    | `Nucleotides of filename list
    | `PartitionedFile of filename list
    | `OfNodes of int
    | `ParallelPipeline of int * script list * script list * script list
    | `PerturbateNSearch of
        transform list * perturb_method * local_optimum * int * timer option
    | `Poyfile of filename list
    | `Prealigned of simple_input * prealigned_costs
    | `Prebuilt of filename
    | `Random_Seed of int
    | `Ratchet of float * int
    | `Resample of [ `Characters of int | `Taxa of int ]
    | `RunForTrees of script list
    | `Support of support_tree Sexpr.t
    | `Trees of 'a Sexpr.t
    | `UnFixImpliedAlignments
    | `UnRatchet
    | `UnResample of [ `Characters of int | `Taxa of int ]
    | `Wagner_Distances of build_strategy
    | `Wagner_Mst of build_strategy
    | `Wagner_Ordered of build_strategy
    | `Wagner_Rnd of build_strategy ]
