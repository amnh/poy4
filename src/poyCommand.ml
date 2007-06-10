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

exception Exit 

let () = SadmanOutput.register "PoyCommand" "$Revision: 810 $"

type read_option_t = [
| `Init3D of bool
| `Orientation of bool
]

type otherfiles = [
    | `AutoDetect of string
    | `Nucleotides of string list
    | `Aminoacids of string list
    | `GeneralAlphabetSeq of (string * string * read_option_t list) 
    | `Breakinv of (string * string * read_option_t list)
    | `Chromosome of string list
    | `Genome of string list
    | `ComplexTerminals of string list
]

type reada = Methods.input

type keepa = Methods.tree_handling

type old_identifiers = [
    | `All
    | `Names of (bool * string list)
    | `Some of (bool * int list)
    | `AllDynamic
    | `AllStatic
    | `Missing of bool * int
    | `Random of int
]
type identifiers = [
    | old_identifiers
    | `Files of (bool * string list)
]

type chromosome_args = [
    | `Inversion of int
    | `Breakpoint of int (*Breakpoint cost between two loci inside one chromosome *)
    | `Circular of bool

    | `Locus_Indel_Cost of (int * int)
    | `Chrom_Indel_Cost of (int * int)
    | `Chrom_Hom of int (* if cost > threshold * min_cost,  then not homologous *)
    | `Chrom_Breakpoint of int (* Breakpoint cost between loci of two different chromosomes *)
    | `Sig_Block_Len of int (* A conserved block length must be greater than Sig_Block_Len *)
    | `Rearranged_Len of int (* t's believed that no rearrangments or
                                    reversions happened within a segment whose length < rearranged_len *)


    | `Seed_Len of int
    | `Keep_Median of int 
    | `SwapMed of int 
    | `Approx of bool
]


type transform_method = [
    | `RandomizedTerminals
    | `Tcm of string
    | `Gap of (int * int)
    | `AffGap of int
    | `TailInput of int array
    | `TailFile of string
    | `PrepInput of int array
    | `PrepFile of string
    | `StaticApproximation of bool
    | `MultiStaticApproximation of bool
    | `Automatic_Static_Aprox of bool
    | `ReWeight of float
    | `WeightFactor of float
    | `Automatic_Sequence_Partition of bool
    | `Prioritize
    | `SearchBased
    | `Fixed_States
    | `SeqToChrom of chromosome_args list
    | `ChangeDynPam of chromosome_args list
    | `SeqToBreakinv of chromosome_args list
    | `AnnchromToBreakinv of chromosome_args list
    | `ChromToSeq of chromosome_args list
    | `BreakinvToSeq of chromosome_args list
    | `OriginCost of float
]

type transforma = (identifiers * transform_method)

type transform = [
    | `Transform of transforma list
]

type cost_calculation = [
    | `Exact 
    | transform
]

type keep_method = [
    | `Last
    | `First
    | `Keep_Random
]

type thresh_trees = [
    | `Threshold of float
    | `Trees of int
]

type builda = [
    | thresh_trees
    | `Prebuilt of Methods.filename
    | `Mst
    | `DistancesRnd
    | `Random
    | `Ordered
    | keep_method
    | cost_calculation
    | Methods.tabu_join_strategy
]

type swap_neighborhood = [
    | `Spr
    | `Tbr
]

type swap_strategy = [
| `SingleNeighborhood of swap_neighborhood
| `ChainNeighborhoods of swap_neighborhood
| `Alternate of (swap_neighborhood * swap_neighborhood)
| `None ]

type swap_trajectory = [
| `AllAround of string option
| `AllThenChoose
| `BestFirst
| `PoyDrifting of (float * float)
| `Annealing of (float * float) ]

type swapa = [
    | `Forest of float
    | thresh_trees
    | keep_method
    | swap_strategy
    | cost_calculation
    | swap_trajectory
    | Methods.tabu_break_strategy
    | Methods.tabu_join_strategy
    | Methods.tabu_reroot_strategy
    | Methods.samples
]

type swap = [
    | `Swap of swapa list
]

type build = [ `Build of builda list ]

type supporta = [
    | build
    | swap
    | `Bremer
    | `Jackknife of [ `Select of string | `Resample of string ] list
    | `Bootstrap of int option
]

type poy_file_commands = [
    | `Load of string
    | `Save of (string * string option)
    | `InspectFile of string
]

type internal_memory = [
    | `Store of (Methods.store_class list * string)
    | `Use of (Methods.store_class list * string)
    | `Discard of (Methods.store_class list * string)
]

type settings = [
    | `HistorySize of int
    | `Logfile of string option
    | `SetSeed of int
    | `Root of int option
    | `RootName of string
]

type output_class = [
    | `Information
    | `Error
    | `Output of string 
]

type application = [
    | `Version
    | `ChangeWDir of string
    | `PrintWDir
    | `Exit
    | `Recover
    | `ClearRecovered
    | `Echo of (string * output_class list)
    | `Help of string option
    | `Set of settings list
    | `Redraw
    | `Wipe 
    | `ReDiagnose
    | `ClearMemory of Methods.clear_item list
    | `ReadScript of string list
    | poy_file_commands
    | internal_memory
]

type charortax = [
    | `Characters
    | `Taxa
]

type charoper = [
    | `Distance
    | `Median
    | `Taxa of identifiers
    | `Characters of identifiers
]

type reporta = [
    | `File of string
    | `Data
    | `Ascii of bool
    | `Memory
    | `Graph of bool
    | `Trees of Methods.information_contained list
    | `MstR
    | `TreesStats
    | `TimeDelta of string
    | `FasWinClad
    | `ExplainScript of string
    | `Consensus of float option
    | `GraphicConsensus of float option
    | `Clades
    | `CrossReferences of old_identifiers option
    | `TerminalsFiles
    | `Supports of Methods.support_output option
    | `GraphicSupports of Methods.support_output option
    | `AllRootsCost
    | `Implied_Alignments of identifiers
    | `Diagnosis
    | `Nodes
]

type perturba = [
    | `Ratchet of (float * int)
    | `Resample of (int * charortax)
    | `Repeat of int
    | swap
    | transform
]

type selecta = [
    | charortax
    | identifiers
    | Methods.tree_handling
]

type renamea = [
    | charortax
    | `File of string
    | `Syn of (string * string)
]

type fusea = [
    | `Keep of int
    | `Iterations of int
    | `Replace of [`Better | `Best]
    | `Swap of swapa list
    | `Weighting of [`Uniform]
    | `Clades of int * int option
]

type searcha = [
    | `Build of bool
    | `Transform of bool
]

type command = [
    | `Read of reada list 
    | build
    | swap
    | `Fuse of fusea list
    | `Support of supporta list
    | `Calculate of charoper list
    | `Report of reporta list
    | `Select of selecta list
    | `Rename of renamea list
    | `Search of searcha list
    | transform
    | `Perturb of perturba list
    | `Repeat of (int * command list)
    | application
]

let all_store_types = [ `Bremer; `Jackknife; `Bootstrap; `Data; `Trees ]
(* Transform *)
let transform_transform acc (id, x) =
    match id with
    | `Files _ ->
            let msg = "In@ transformations@ of@ characters,@ specifying@ a@ " ^
            "character@ set@ using@ a@ file@ is@ not@ allowed.@ I@ will@ " ^
            "ignore@ this@ command." in
            Status.user_message Status.Error msg;
            acc
    | #Methods.characters as id ->
            match x with
            | `RandomizedTerminals -> `RandomizedTerminals :: acc
            | `Tcm f -> (`Assign_Transformation_Cost_Matrix ((Some (`Local f)), id)) :: acc
            | `Gap (a, b) -> 
                    (`Create_Transformation_Cost_Matrix (a, b, id)) :: acc
            | `AffGap (c) ->
                    (`Assign_Affine_Gap_Cost (c, id)) ::
                        acc
            | `PrepInput x ->
                    (`Assign_Prep_Cost ((`Array x), id)) :: acc
            | `PrepFile x ->
                    (`Assign_Prep_Cost ((`File (`Local x)), id)) :: acc
            | `TailInput x ->
                    (`Assign_Tail_Cost ((`Array x), id)) :: acc
            | `TailFile x ->
                    (`Assign_Tail_Cost ((`File (`Local x)), id)) :: acc
            | `MultiStaticApproximation noninf -> (`MultiStatic_Aprox (id, noninf)) :: acc
            | `StaticApproximation noninf -> (`Static_Aprox (id, noninf)) :: acc
            | `Automatic_Static_Aprox sens -> (`Automatic_Static_Aprox sens) :: acc
            | `ReWeight w -> (`ReWeight (id, w)) :: acc
            | `WeightFactor w -> (`WeightFactor (id, w)) :: acc
            | `Automatic_Sequence_Partition sens -> 
                    (`Automatic_Sequence_Partition (id, sens)) :: acc
            | `Prioritize -> `Prioritize :: acc
            | `SearchBased -> (`Search_Based id) :: acc
            | `Fixed_States -> (`Fixed_States id) :: acc
            | `SeqToChrom x -> (`Seq_to_Chrom (id, x)) :: acc
            | `SeqToBreakinv x -> (`Seq_to_Breakinv (id, x)) :: acc
            | `AnnchromToBreakinv x -> (`Annchrom_to_Breakinv (id, x)) :: acc
            | `ChangeDynPam x -> (`Change_Dyn_Pam (id, x)) :: acc
            | `ChromToSeq x -> (`Chrom_to_Seq (id, x)) :: acc
            | `BreakinvToSeq x -> (`Breakinv_to_Seq (id, x)) :: acc
            | (`OriginCost _) as id -> id :: acc


let transform_transform_arguments x =
    List.fold_left transform_transform [] x

(* Reading files *)
let modify_acc acc c = function
    | [] -> acc
    | files -> (`Other (files, c)) :: acc

(* Building *)
let build_default_method_args = (1, `Last, [], `UnionBased None)
let build_default_method = `Wagner_Rnd build_default_method_args
let build_default = (10, build_default_method, [])

let transform_build ((n, (meth : Methods.build_method), (trans :
    Methods.cost_calculation list)) as acc) = function
    | `Prebuilt fn -> (n, (`Prebuilt fn), trans)
    | `DistancesRnd ->
            begin match meth with
            | `Prebuilt _
            | `Wagner_Distances _ -> acc
            | `Wagner_Mst x
            | `Wagner_Rnd x 
            | `Wagner_Ordered x
            | `Build_Random x -> (n, (`Wagner_Distances x), trans)
            end
    | `Mst ->
            begin match meth with
            | `Prebuilt _
            | `Wagner_Mst _ -> acc
            | `Wagner_Distances x
            | `Wagner_Rnd x 
            | `Wagner_Ordered x
            | `Build_Random x -> (n, (`Wagner_Mst x), trans)
            end
    | `Random ->
            begin match meth with
            | `Prebuilt _
            | `Wagner_Rnd _ -> acc
            | `Wagner_Distances x
            | `Wagner_Mst x
            | `Wagner_Ordered x
            | `Build_Random x -> (n, (`Wagner_Rnd x), trans)
            end
    | `Ordered ->
            begin match meth with
            | `Prebuilt _
            | `Wagner_Ordered _ -> acc
            | `Wagner_Distances x
            | `Wagner_Mst x
            | `Build_Random x 
            | `Wagner_Rnd x -> (n, (`Wagner_Ordered x), trans)
            end
    | `Threshold _ ->
            acc
    | `Trees x ->
            (x, meth, trans)
    | `Last
    | `First
    | `Keep_Random as x -> 
            let nmeth = 
                match meth with
                | `Prebuilt _ -> meth
                | `Wagner_Distances (a, _, c, d) -> 
                        `Wagner_Distances (a, x, c, d)
                | `Wagner_Mst (a, _, c, d) -> 
                        `Wagner_Mst (a, x, c, d)
                | `Wagner_Rnd (a, _, c, d) -> `Wagner_Rnd (a, x, c, d)
                | `Wagner_Ordered (a, _, c, d) -> `Wagner_Ordered (a, x, c, d)
                | `Build_Random (a, _, c, d) -> `Build_Random (a, x, c, d)
            in
            n, nmeth, trans
    | `Transform x ->
            let t = transform_transform_arguments x in
            (n, meth, (t @ trans))
    | `Exact ->
            (n, meth, (`Exact :: trans))
    | #Methods.tabu_join_strategy as tabu ->
            let nmeth = 
                match meth with
                | `Prebuilt _ -> meth
                | `Wagner_Distances (a, b, c, _) -> 
                        `Wagner_Distances (a, b, c, tabu)
                | `Wagner_Mst (a, b, c, _) -> 
                        `Wagner_Mst (a, b, c, tabu)
                | `Wagner_Rnd (a, b, c, _) -> 
                        `Wagner_Rnd (a, b, c, tabu)
                | `Wagner_Ordered (a, b, c, d) -> 
                        `Wagner_Ordered (a, b, c, tabu)
                | `Build_Random (a, b, c, _) -> 
                        `Build_Random (a, b, c, tabu)
            in
            n, nmeth, trans

let transform_build_arguments x =
    let (x, y, z) = List.fold_left transform_build build_default  x in
    (x, y, List.rev z)

(* Swapping *)
let swap_default = (`Alternate (`Spr, `Tbr),
                    0.0,                (* threshold *)
                    1,                  (* trees to keep *)
                    `Last,              (* keep method *)
                    [],                 (* cost calc list *)
                    None,               (* forest search *)
                    `BestFirst,         (* traject. strategy *)
                    `DistanceSorted,    (* Tabu break *)
                    `UnionBased None,     (* Tabu join *)
                    `Bfs None,          (* Tabu reroot *)
                    [])                 (* What should be sampled along the
                                        search *)
let swap_default_none = (`None,
                         0.0,
                         1,
                         `Last,
                         [],
                         None,
                         `BestFirst,
                         `DistanceSorted,
                         `UnionBased None, 
                         `Bfs None,
                         [])               
let transform_swap (space, thres, keep, keepm, cclist, origin, traj, break_tabu,
join_tabu, reroot_tabu, samples)
        (param : swapa) = match param with
    | `Threshold thres ->
          (space, thres, keep, keepm, cclist, origin, traj, break_tabu,
          join_tabu, reroot_tabu, samples)
    | `Trees keep ->
          (space, thres, keep, keepm, cclist, origin, traj, break_tabu,
          join_tabu, reroot_tabu, samples)
    | #Methods.keep_method as keepm ->
          (space, thres, keep, keepm, cclist, origin, traj, break_tabu,
          join_tabu, reroot_tabu, samples)
    | #swap_strategy as space ->
          (space, thres, keep, keepm, cclist, origin, traj, break_tabu,
          join_tabu, reroot_tabu, samples)
    | `Transform x ->
          let t = transform_transform_arguments x in
          let cclist = t @ cclist in
          (space, thres, keep, keepm, cclist, origin, traj, break_tabu,
          join_tabu, reroot_tabu, samples)
    | `Exact as x ->
            (space, thres, keep, keepm, (x :: cclist), origin, traj, break_tabu,
            join_tabu, reroot_tabu, samples)
    | `Forest cost ->
          let origin = Some cost in
          print_endline ("Forest: "^string_of_float cost);
          (space, thres, keep, keepm, cclist, origin, traj, break_tabu,
          join_tabu, reroot_tabu, samples)
    | #swap_trajectory as traj ->
          (space, thres, keep, keepm, cclist, origin, traj, break_tabu,
          join_tabu, reroot_tabu, samples)
    | #Methods.tabu_break_strategy as tabu ->
          (space, thres, keep, keepm, cclist, origin, traj, tabu, join_tabu,
          reroot_tabu, samples)
    | #Methods.tabu_join_strategy as tabu ->
          (space, thres, keep, keepm, cclist, origin, traj, break_tabu, tabu,
          reroot_tabu, samples)
    | #Methods.tabu_reroot_strategy as tabu ->
          (space, thres, keep, keepm, cclist, origin, traj, break_tabu, 
          join_tabu, tabu, samples)
    | #Methods.samples as s ->
          (space, thres, keep, keepm, cclist, origin, traj, break_tabu, 
          join_tabu, reroot_tabu, s :: samples)


(* let transform_swap (meth, (a, b, c, d), e) = function *)
(*     | `Threshold x -> (meth, (x, b, c, d), e) *)
(*     | `Trees x -> (meth, (a, x, c, d), e) *)
(*     | #Methods.keep_method as m -> (meth, (a, b, m, d), e) *)
(*     | #swap_strategy as x -> (x, (a,  b, c, d), e) *)
(*     | `Transform x -> *)
(*             let t = transform_transform_arguments x in *)
(*             (meth, (a, b, c, t @ d), e) *)
(*     | (`Forest _) as x ->(meth, (a, b, c, d), x) *)


let transform_swap_arguments (lst : swapa list) =
    let param =
        List.fold_left transform_swap swap_default lst in
    `LocalOptimum param

(* Fusing *)
let rec transform_fuse ?(iterations=None) ?(keep=None) ?(replace=`Better)
        ?(search=`LocalOptimum swap_default_none) ?(weighting=`Uniform) ?(clades=(4,7)) = function
            | [] -> `Fusing (iterations, keep, weighting, replace, search,
                             clades)
            | x :: xs ->
                  (match x with
                   | `Keep keep ->
                         let keep = Some keep in
                         transform_fuse ~iterations ~keep ~replace ~search
                             ~weighting ~clades xs
                   | `Iterations iterations ->
                         let iterations = Some iterations in
                         transform_fuse ~iterations ~keep ~replace ~search
                             ~weighting ~clades xs
                   | `Replace replace -> transform_fuse ~iterations ~keep
                         ~replace ~search ~weighting ~clades xs
                   | `Weighting weighting -> transform_fuse ~iterations ~keep
                         ~replace ~search ~weighting ~clades xs
                   | `Clades (i, Some j) ->
                         let clades = (i, j) in
                         transform_fuse ~iterations ~keep ~replace ~search
                             ~weighting ~clades xs
                   | `Clades (i, None) ->
                         let clades = (i, i) in
                         transform_fuse ~iterations ~keep ~replace ~search
                             ~weighting ~clades xs
                   | `Swap swapas ->
                         let search = transform_swap_arguments swapas in
                         transform_fuse ~iterations ~keep ~replace ~search
                             ~weighting ~clades xs
                  )



(* Perturbing *)
let perturb_default_swap : Methods.local_optimum = `LocalOptimum swap_default 
let perturb_default_iterations = 1
let perturb_default_perturb = `Ratchet (0.25, 2)
let perturb_transform = []
let perturb_default = 
    (perturb_transform, perturb_default_perturb, perturb_default_swap, 
    perturb_default_iterations)
let transform_perturb (tr, m, sw, it) = function
    | `Transform x  ->
            let x = transform_transform_arguments x in
            ((x @ tr), m, sw, it)
    | `Swap x -> 
            let x = transform_swap_arguments x in
            tr, m, x, it
    | `Ratchet _ as x -> (tr, x, sw, it)
    | `Resample (x, `Taxa) -> tr, `Resample (`Taxa x), sw, it
    | `Resample (x, `Characters) -> tr, `Resample (`Characters x), sw, it
    | `Repeat it -> tr, m, sw, it

let transform_perturb_arguments x : Methods.script list = 
    let tr, a, b, c = List.fold_left transform_perturb perturb_default x in
    `PerturbateNSearch (tr, a, b, c) :: []

(* Support *)
let support_default_swap = `LocalOptimum swap_default
let support_select = 36.0
let support_resamplings = 5
let support_default_build = (1, build_default_method, [])
let support_default = 
    `Bremer, (support_select, support_resamplings, support_default_swap,
    (support_default_build))

let transform_support (meth, (ss, sr, ssw, sb)) = function
    | `Swap s ->
            let v = transform_swap_arguments s in
            (meth, (ss, sr, v, sb))
    | `Build s ->
            let z = transform_build_arguments s in
            (meth, (ss, sr, ssw, z))
    | `Bremer as x -> (x, (ss, sr, ssw, sb))
    | `Bootstrap x ->
            let nx = 
                match x with
                | None -> sr
                | Some v -> v
            in
            (`Bootstrap, (ss, nx, ssw, sb))
    | `Jackknife items ->
            let process_item (a, b, c, sb) = function
                | `Select x -> 
                        ((float_of_string x), b, c, sb)
                | `Resample x ->
                        (a, int_of_string x, c, sb)
            in
            let r = List.fold_left process_item (ss, sr, ssw, sb) items in
            (`Jackknife, r)

let rec transform_support_arguments args =
    match List.fold_left transform_support support_default args with
    | `Bremer, (_, _, c, d) ->`Bremer (c, (`Build d), 0, 1)
    | `Jackknife, (a, b, c, d) -> `Jackknife (a, b, c, (`Build d), None)
    | `Bootstrap, (_, b, c, d) -> `Bootstrap (b, c, (`Build d), None)
    
(* Reporting things *)
let transform_report ((acc : Methods.script list), file) (item : reporta) = 
    match item with
    | `File x -> (acc, Some x)
    | `Data -> (`Dataset file) :: acc, file
    | `Ascii x -> (`Ascii (file, x)) :: acc, file
    | `Memory -> (`Memory file) :: acc, file
    | `Graph x -> 
            begin match acc, file with
            | (_ :: _), None -> ()
            | (_ :: _), Some _ ->
                    let msg = "Warning:@ You@ have@ requested@ to@ output@ " ^
                    "a@ postscript@ file@ in@ a@ flat@ text@ file,@ this@ " ^
                    "is@ probably@ not@ what@ you@ expect.@ If@ you@ want@ " ^
                    "to@ output@ a@ graphical@ " ^
                    "version@ of@ your@ tree@ in@ a@ file,@ assign@ a@ " ^
                    "different@ " ^
                    "filename@ to@ the@ graph@ command@ in@ the@ report@ line."
                    in
                    Status.user_message Status.Error msg
            | _ -> ()
            end;
            (`Graph (file, x)) :: acc, file
    | `Trees lst ->
            (`Trees (lst, file)) :: acc, file
    | `MstR ->
            (`MstR file) :: acc, file
    | `TreesStats ->
            (`TreesStats (file)) :: acc, file
    | `TimeDelta str ->
            (`TimeDelta (str, file)) :: acc, file
    | `Consensus v ->
            (`Consensus (file, v)) :: acc, file
    | `GraphicConsensus v ->
            (`GraphicConsensus (file, v)) :: acc, file
    | `FasWinClad ->
            (`FasWinClad (file)) :: acc, file
    | `ExplainScript script ->
            (`ExplainScript (script, file)) :: acc, file
    | `Clades -> 
            begin match file with
            | None ->
                    let msg = "Sorry,@ I@ need@ a@ filename@ to@ output@ " ^
                    "the@ clades@ file.@ I@ will@ ignore@ your@ clades@ " ^
                    "request." in
                    Status.user_message Status.Error msg;
                    acc, file
            | Some f ->
                    (`Clades f) :: acc, file
            end
    | `CrossReferences x -> (`CrossReferences (x, file)) :: acc, file
    | `TerminalsFiles -> (`TerminalsFiles file) :: acc, file
    | `Supports c -> (`Supports (c, file)) :: acc, file
    | `GraphicSupports c -> (`GraphicSupports (c, file)) :: acc, file
    | `AllRootsCost -> (`AllRootsCost file) :: acc, file
    | `Implied_Alignments id -> 
            (match id with
            | #Methods.characters as id ->
                    (`Implied_Alignment (file, id)) :: acc, file
            | _ -> acc, file)
    | `Diagnosis -> 
            (`Diagnosis file) :: acc, file
    | `Nodes ->
            (`Nodes file) :: acc, file

let transform_report_arguments x =
    match x with
    | [`File file] ->
            let file = Some file in
            [`Ascii (file, true); `Diagnosis file; `Trees ([], file)]
    | [] -> [`Ascii (None, true); `Diagnosis None; `Trees ([], None)]
    | _ -> 
            let def = [], None in
            let x, _ = List.fold_left transform_report def x in
            x

(* Selecting *)
let transform_select (choose, (acc : Methods.script list)) = function
    | `Characters 
    | `Taxa as x -> (x, acc)
    | (`Random _) | (`Missing _) | (`Names _) as meth ->
            begin match choose with
            | `Taxa -> (choose, ((`AnalyzeOnly meth) :: acc))
            | `Characters ->  
                    (choose, (`AnalyzeOnlyCharacters meth) :: acc)
            end
    | `Files (do_complement, x) ->
            let x = List.map (fun x -> `Local x) x in
            begin match choose with
            | `Taxa -> 
                    (choose, ((`AnalyzeOnlyFiles (do_complement, x)) :: acc))
            | `Characters -> 
                    (choose, ((`AnalyzeOnlyCharacterFiles (do_complement, x)) :: acc))
            end
    | `BestN _
    | `BestWithin _
    | `Unique
    | `RandomTrees _ as x ->
            (choose, (x :: acc))
    | _ -> 
            (* TODO: I don't think any user will really use this feature, ever,
            * and I would have to spend a fair ammount of time getting it going,
            * so I will leave it off, if someone requests the feature, I add it
            * *)
            let msg = "I@ only@ support@ taxa@ selection@ with@ the names@ of" ^
            "@ the@ taxa,@ not@ their@ codes,@ and,@ of@ course,@ it@ makes@ " ^
            "no@ sense@ to@ select@ all!.@ So@ please,@ if@ it@ is@ worth@ " ^
            "the@ time@ and@ effort,@ place@ the@ feature@ request." in
            Status.user_message Status.Error msg;
            (choose, acc)

let transform_select_arguments x =
    match x with
    | [] -> [`BestN None; `Unique]
    | x ->
            let _, res = List.fold_left transform_select (`Taxa, []) x in
            res

(* Renaming *)
let transform_rename (on, (files : Methods.filename list), ren, acc) x = 
    let is_empty =
        match files, ren with
        | [], [] -> true
        | _ -> false
    in
    match x with
    | `Characters ->
            (match on with
            | `Characters -> (on, files, ren, acc)
            | `Taxa ->
                    if is_empty then (`Characters, files, ren, acc)
                    else 
                        let acc = (`RenameCharacters ren) :: acc in
                        (`Characters, [], [], acc))
    | `Taxa ->
            (match on with
            | `Taxa -> (on, files, ren, acc)
            | `Characters ->
                    if is_empty then (`Taxa, files, ren, acc)
                    else 
                        let acc = 
                            (`SynonymsFile (List.rev files)) :: 
                                (`Synonyms ren) :: acc
                        in
                        (`Taxa, [], [], acc))
    | `File f -> 
            (match on with
            | `Taxa -> (on, (`Local f) :: files, ren, acc)
            | `Characters -> 
                    let msg = "We@ only@ suport@ command@ line@ character@ " ^
                    "renaming@ for@ now.@ If@ you@ think@ this@ is@ a@ useful"
                    ^ "@ feature,@ file@ the@ request.@ For@ now,@ the@ file" 
                    ^ " " ^ f ^ "@ will@ be@ ignored." in
                    Status.user_message Status.Error msg;
                    (on, files, ren, acc))
    | `Syn s -> (on, files, s :: ren, acc)
    
let transform_rename_arguments x =
    match List.fold_left transform_rename (`Taxa, [], [], []) x with
    | `Characters, [], [], x 
    | `Taxa, [], [], x -> x
    | `Taxa, files, ren, acc ->
            (`SynonymsFile files) :: (`Synonyms ren) ::  acc
    | `Characters, _, ren, acc ->
            (`RenameCharacters ren) :: acc

let default_search : Methods.script list = 
(*    List.rev [`Build build_default; `LocalOptimum swap_default]*)
    let change_transforms x = 
        let (a, b, c, d, _, f, g, h, i, j, k) = swap_default in
        (a, b, c, d, x, f, g, h, i, j, k)
    in
    let s1 = change_transforms [(`Static_Aprox (`All, false))]
    and s2 = 
        change_transforms 
        [(`Automatic_Sequence_Partition (`All, false));
        (`Automatic_Static_Aprox false)]
    and s3 = 
        change_transforms
        [`Automatic_Sequence_Partition (`All, false)]
    in
    List.rev (`Build build_default :: 
        `LocalOptimum s1 :: `LocalOptimum s2 :: [`LocalOptimum s3])

let transform_search items = 
    let do_transform = 
        List.exists (function `Transform true -> true | _ -> false) items
    and do_build =
        List.exists (function `Build true -> true | _ -> false) items
    in
    match default_search with
    | [a; b; c; d] ->
            if not do_build && not do_transform then 
                [`LocalOptimum swap_default]
            else if not do_transform then
                [`LocalOptimum swap_default; d]
            else if not do_build then
                [a; b; c]
            else default_search
    | _ -> failwith "Forgot to update the list of options of search?"


let rec transform_command (acc : Methods.script list) (meth : command) : Methods.script list =
    match meth with
    | `Version
    | `Exit 
    | `Recover
    | `ClearRecovered
    | `Redraw
    | `Wipe 
    | `ReDiagnose
    | `ClearMemory _
    | `ReadScript _
    | `Store _
    | `Discard _
    | `Load _
    | `Save _
    | `InspectFile _
    | `ChangeWDir _
    | `PrintWDir
    | `Help _ as meth -> meth :: acc
    | `Use x -> `Set x :: acc
    | `Echo (a, y) ->
            List.fold_left (fun acc x -> `Echo (a, x) :: acc) acc y
    | `Set settings ->
            let s = ((List.rev settings) :> Methods.script list)  in
            s @ acc
    | `Read meth ->
            ((List.rev meth) :> Methods.script list) @ acc
    | `Build x ->
            let setting = transform_build_arguments x in
            (`Build setting) :: acc
    | `Swap x ->
            (transform_swap_arguments x) :: acc
    | `Search args ->
            (transform_search args) @ acc
    | `Fuse x ->
          (transform_fuse x) :: acc
    | `Support x ->
            (transform_support_arguments x) :: acc
    | `Calculate _ -> acc
    | `Report x ->
            (transform_report_arguments x) @ acc
    | `Select x ->
            (transform_select_arguments x) @ acc
    | `Transform x ->
            let x = transform_transform_arguments x in
            x @ acc
    | `Perturb x ->
            (transform_perturb_arguments x) @ acc
    | `Rename x ->
            (transform_rename_arguments x) @ acc
    | `Repeat (it, com) ->
            let res = List.rev (List.fold_left transform_command [] com) in
            `Repeat (it, res) :: acc

let transform_all_commands (x : command list) = 
    List.rev (List.fold_left transform_command [] x)

let to_local x = List.map (fun x -> `Local x) x

(* The necessary types to produce the tree of the parsed input. *)
let create_expr lexer = 
    let gram = Grammar.gcreate lexer in
    let expr = Grammar.Entry.create gram "expr" in
    EXTEND
        GLOBAL: expr;
        expr: [ [a = LIST1 command; EOI -> (a : command list)] ];
        (* Application commands *)
        (* Transforming taxa or characters *)
        transform:
            [
                [ LIDENT "transform"; left_parenthesis; 
                    x = LIST0 transform_argument SEP ","; right_parenthesis ->
                        (`Transform x : transform) ]
            ];
        transform_argument:
            [
                [ left_parenthesis; x = identifiers; ","; t = transform_method; 
                    right_parenthesis -> (x, t) ] |
                [ t = transform_method -> (`All, t) ]
            |   [ LIDENT "origin_cost"; ":"; x = integer_or_float
                        -> (`All, `OriginCost (float_of_string x)) ] |
                [ LIDENT "prioritize" -> (`All, `Prioritize) ] 
            ];
        transform_method:
            [
                [ LIDENT "randomize_terminals" -> `RandomizedTerminals ] |
                [ LIDENT "tcm"; ":";  x = STRING -> `Tcm x ] |
                [ LIDENT "fixedstates" -> `Fixed_States ] |
                [ LIDENT "tcm"; ":"; left_parenthesis; x = INT; ","; y = INT; 
                    right_parenthesis -> `Gap (int_of_string x, int_of_string y) ] |
                [ LIDENT "gap_opening"; ":"; x = INT -> `AffGap (int_of_string x) ] |
                [ LIDENT "trailing_deletion"; ":"; x = STRING -> `TailFile x ] |
                [ LIDENT "td"; ":"; x = STRING -> `TailFile x ] |
                [ LIDENT "trailing_deletion"; ":"; left_parenthesis; 
                    x = LIST1 INT SEP ","; right_parenthesis -> 
                        `TailInput (Array.of_list (List.map (int_of_string) x)) ] |
                [ LIDENT "td"; ":"; left_parenthesis; x = LIST1 INT SEP ",";
                    right_parenthesis -> 
                        `TailInput (Array.of_list (List.map (int_of_string) x)) ] |
                [ LIDENT "trailing_insertion"; ":"; x = STRING -> `PrepFile x ] |
                [ LIDENT "ti"; ":"; x = STRING -> `PrepFile x ] |
                [ LIDENT "trailing_insertion"; ":"; left_parenthesis; 
                    x = LIST1 INT SEP ","; right_parenthesis -> 
                        `PrepInput (Array.of_list (List.map (int_of_string) x)) ] |
                [ LIDENT "ti"; ":"; left_parenthesis; x = LIST1 INT SEP ",";
                    right_parenthesis -> 
                        `PrepInput (Array.of_list (List.map (int_of_string) x)) ] |
                [ LIDENT "static_approx"; x = OPT informative_characters -> 
                    match x with 
                    | None -> `StaticApproximation true 
                    | Some v -> `StaticApproximation v ] |
                [ LIDENT "multi_static_approx"; x = OPT informative_characters -> 
                    match x with 
                    | None -> `MultiStaticApproximation true 
                    | Some v -> `MultiStaticApproximation v ] |
                [ LIDENT "auto_static_approx"; x = OPT optional_boolean -> 
                    match x with
                    | None -> `Automatic_Static_Aprox false 
                    | Some x -> `Automatic_Static_Aprox x ] |
                [ LIDENT "auto_sequence_partition"; x = OPT optional_boolean -> 
                    match x with
                    | None -> `Automatic_Sequence_Partition false 
                    | Some x -> `Automatic_Sequence_Partition x ] |
                [ LIDENT "weight"; ":"; x = neg_integer_or_float -> `ReWeight (float_of_string x) ] |
                [ LIDENT "weightfactor"; ":"; x = neg_integer_or_float -> `WeightFactor (float_of_string x) ] |
                [ LIDENT "search_based" -> `SearchBased ] |
                [ LIDENT "seq_to_chrom"; ":"; left_parenthesis; x = LIST0
                        chromosome_argument SEP ","; right_parenthesis -> `SeqToChrom x ] | 
                [ LIDENT "seq_to_breakinv"; ":"; left_parenthesis; x = LIST0
                        chromosome_argument SEP ","; right_parenthesis -> `SeqToBreakinv x ] | 

                [ LIDENT "annchrom_to_breakinv"; ":"; left_parenthesis; x = LIST0
                        chromosome_argument SEP ","; right_parenthesis -> `AnnchromToBreakinv x ] | 

                [ LIDENT "dynamic_pam"; ":"; left_parenthesis; x = LIST0 
                        chromosome_argument SEP ","; right_parenthesis -> `ChangeDynPam x ] | 
                [ LIDENT "chrom_to_seq" -> `ChromToSeq [] ] |
                [ LIDENT "breakinv_to_seq" -> `BreakinvToSeq [] ] 
            ];
        informative_characters:
            [
                [ ":"; LIDENT "keep" -> false ] |
                [ ":"; LIDENT "remove" -> true ]
            ];
        chromosome_argument:
            [
                [ LIDENT "inversion"; ":"; c = INT -> 
                      `Inversion (int_of_string c) ]  |
                [ LIDENT "breakpoint"; ":"; c = INT -> 
                      `Breakpoint (int_of_string c) ]  |
                [ LIDENT "chrom_breakpoint"; ":"; c = INT -> 
                      `Chrom_Breakpoint (int_of_string c) ]  |
                [ LIDENT "circular"; ":"; e = boolean -> `Circular e] |

                [ LIDENT "locus_indel"; ":"; left_parenthesis; o = INT; 
                    ","; e = FLOAT; right_parenthesis ->
                      `Locus_Indel_Cost ( (int_of_string o), 
                      int_of_float ((float_of_string e) *. 100.0) ) ] | 
                [ LIDENT "chrom_indel"; ":"; left_parenthesis; o = INT; 
                ","; e = FLOAT; right_parenthesis ->
                      `Chrom_Indel_Cost ( (int_of_string o), 
                      int_of_float ((float_of_string e) *. 100.0) ) ] | 
                [ LIDENT "chrom_hom"; ":"; c = FLOAT -> 
                      `Chrom_Hom (int_of_float ((float_of_string c) *. 100.)) ] | 
                [ LIDENT "sig_block_len"; ":"; c = INT -> 
                      `Sig_Block_Len (int_of_string c) ] | 
                [ LIDENT "rearranged_len"; ":"; c = INT -> 
                      `Rearranged_Len (int_of_string c) ] | 
                [ LIDENT "seed_length"; ":"; c = INT -> 
                      `Seed_Len (int_of_string c) ] | 
                [ LIDENT "median"; ":"; c = INT ->
                      `Keep_Median (int_of_string c) ] |
                [ LIDENT "swap_med"; ":"; iters = INT -> `SwapMed (int_of_string iters) ] | 
                [ LIDENT "approx"; ":"; ans = boolean -> `Approx ans] 
            ];

        (* Applications *)
        application_command:
            [
                [ LIDENT "version"; left_parenthesis; right_parenthesis -> `Version ] |
                [ LIDENT "exit"; left_parenthesis; right_parenthesis -> `Exit ] |
                [ LIDENT "recover"; left_parenthesis; right_parenthesis -> `Recover ] |
                [ LIDENT "clear_recovered"; left_parenthesis; right_parenthesis -> 
                    `ClearRecovered ] |
                [ LIDENT "quit" ; left_parenthesis; right_parenthesis -> `Exit ] |
                [ LIDENT "echo"; left_parenthesis; a = STRING; OPT ",";
                    x = LIST0 output_class SEP ","; right_parenthesis -> `Echo (a, x) ] |
                [ LIDENT "help"; left_parenthesis; a = OPT string_or_ident; 
                    right_parenthesis -> `Help a ] |
                [ LIDENT "set"; left_parenthesis; b = LIST0 setting SEP ","; 
                    right_parenthesis -> `Set b ] |
                [ LIDENT "redraw"; left_parenthesis; right_parenthesis -> `Redraw ] |
                [ LIDENT "wipe"; left_parenthesis; right_parenthesis -> `Wipe ] |
                [ LIDENT "clear_memory"; left_parenthesis; x = LIST0 clear_options
                    SEP ","; right_parenthesis -> `ClearMemory x ] |
                [ LIDENT "load"; left_parenthesis; a = STRING; 
                    right_parenthesis -> `Load a ] |
                [ LIDENT "save"; left_parenthesis; a = STRING; 
                    b = OPT file_comment; right_parenthesis -> `Save (a, b) ] |
                [ LIDENT "inspect"; left_parenthesis; a = STRING; 
                    right_parenthesis -> `InspectFile a ] |
                [ LIDENT "store"; left_parenthesis; a = STRING; 
                    right_parenthesis -> `Store (all_store_types, a) ] |
                [ LIDENT "use"; left_parenthesis; a = STRING; 
                    right_parenthesis -> 
                        `Use (all_store_types, a) ] |
                [ LIDENT "rediagnose"; left_parenthesis; 
                    right_parenthesis -> `ReDiagnose ] |
                [ LIDENT "run"; left_parenthesis; a = LIST0 STRING SEP ","; 
                    right_parenthesis -> `ReadScript a ] |
                [ LIDENT "cd"; left_parenthesis; a = STRING; right_parenthesis ->
                    `ChangeWDir a ] |
                [ LIDENT "pwd"; left_parenthesis; right_parenthesis -> 
                    `PrintWDir ]
            ];
        clear_options:
            [
                [ LIDENT "m" -> `Matrices ] |
                [ LIDENT "s" -> `SequencePool ]
            ];
        file_comment:
            [
                [ ","; x = STRING -> x ]
            ];
        output_class:
            [
                [ LIDENT "info" -> `Information ] |
                [ LIDENT "error" -> `Error ] |
                [ LIDENT "output"; ":"; x = STRING -> `Output x ]
            ];
        setting:
            [
                [ LIDENT "history"; ":"; x = INT -> `HistorySize (int_of_string x) ] |
                [ LIDENT "log"; ":"; x = STRING -> `Logfile (Some x) ] |
                [ LIDENT "log"; ":"; LIDENT "new"; ":"; x = STRING ->
                    StatusCommon.Files.closef x ();
                    let _ = StatusCommon.Files.openf ~mode:`New x in
                    `Logfile (Some x)
                    ] |
                [ LIDENT "nolog" -> `Logfile None ] |
                [ LIDENT "seed"; ":"; x = INT -> `SetSeed (int_of_string x) ] |
                [ LIDENT "root"; ":"; x = STRING -> `RootName x ] |
                [ LIDENT "root"; ":"; x = INT -> `Root (Some (int_of_string x)) ]
            ];
        (* Reporting *)
        report:
            [
                [ LIDENT "report"; left_parenthesis; a = LIST0 report_argument SEP ","; 
                    right_parenthesis -> `Report a ]
            ];
        report_argument:
            [
                [ x = STRING -> `File x ] |
                [ LIDENT "new"; ":"; x = STRING ->
                    StatusCommon.Files.closef x ();
                    let _ = StatusCommon.Files.openf ~mode:`New x in
                    `File x
                    ] |
                [ LIDENT "asciitrees" ; y = OPT optional_collapse -> 
                    match y with
                    | Some (`Collapse y) -> `Ascii y
                    | None -> `Ascii true ] |
                [ LIDENT "memory" -> `Memory ] | 
                [ LIDENT "graphtrees"; y = OPT optional_collapse -> 
                    match y with
                    | Some (`Collapse y) -> `Graph y
                    | None -> `Graph true ] |
                [ LIDENT "trees"; x = OPT tree_information_list -> 
                    match x with
                    | Some x -> `Trees x | None -> `Trees [] ] |
                [ LIDENT "treestats" -> `TreesStats ] |
                [ LIDENT "timer"; ":"; x = STRING -> `TimeDelta x ] |
                [ LIDENT "_mst" -> `MstR ] | 
                [ LIDENT "consensus"; x = OPT optional_integer_or_float -> 
                    `Consensus 
                    (match x with
                    | None -> None 
                    | Some x -> Some (float_of_string x)) ] | 
                [ LIDENT "graphconsensus"; x = OPT optional_integer_or_float -> 
                    `GraphicConsensus 
                    (match x with
                    | None -> None 
                    | Some x -> Some (float_of_string x)) ] | 
                [ LIDENT "clades" -> `Clades ] |
                [ LIDENT "phastwinclad" -> `FasWinClad ] | 
                [ LIDENT "script_analysis"; ":"; x = STRING -> `ExplainScript x ] |
                [ LIDENT "supports"; y = OPT opt_support_names -> `Supports y ] |
                [ LIDENT "graphsupports"; y = OPT opt_support_names -> 
                    `GraphicSupports y ] |
                [ LIDENT "diagnosis" -> `Diagnosis ] |
                [ LIDENT "data" -> `Data ] |
                [ LIDENT "implied_alignments"; ":"; x = identifiers ->
                    `Implied_Alignments x ] |
                [ LIDENT "all_roots" -> `AllRootsCost ] |
                [ LIDENT "implied_alignments" -> `Implied_Alignments `All] |
                [ LIDENT "ia"; ":"; x = identifiers -> `Implied_Alignments x ] | 
                [ LIDENT "ia" -> `Implied_Alignments `All ] |
                [ LIDENT "nodes" -> `Nodes ] |
                [ LIDENT "cross_references"; ":"; x = old_identifiers -> 
                    `CrossReferences (Some x) ] |
                [ LIDENT "terminals" -> `TerminalsFiles ] | 
                [ LIDENT "cross_references" -> `CrossReferences None ]
            ];
        (* Perturbation method *)
        perturb:
            [
                [ LIDENT "perturb"; left_parenthesis; x = LIST0 perturb_argument SEP ","; 
                    right_parenthesis -> `Perturb x ]
            ];
        perturb_argument:
            [
                [ x = ratchet -> x ]  |
                [ x = resample -> x ] |
                [ x = swap -> (x :> perturba) ] |
                [ x = transform -> (x :> perturba) ] |
                [ LIDENT "iterations"; ":"; x = INT -> `Repeat (int_of_string x) ]
            ];
        ratchet:
            [
                [ LIDENT "ratchet"; ":"; left_parenthesis; x = FLOAT; ","; y = INT; 
                    right_parenthesis -> `Ratchet (float_of_string x, int_of_string y) ] 
            ];
        resample:
            [
                [ LIDENT "resample"; ":"; left_parenthesis; x = INT; ","; y = charortax; 
                    right_parenthesis -> `Resample (int_of_string x, y) ]
            ];
        charortax:
            [
                [ LIDENT "characters" -> `Characters ] |
                [ LIDENT "terminals" -> `Taxa ]
            ];
        (* Selecting characters or taxa *)
        select:
            [
                [ LIDENT "select"; left_parenthesis; x = LIST0 select_argument SEP ","; 
                    right_parenthesis -> `Select x ]
            ];
        select_argument:
            [
                [ x = identifiers -> (x :> selecta) ] |
                [ x = charortax -> (x :> selecta) ] |
                [ x = seltrees -> (x :> selecta) ] |
                [ x = STRING -> `Files (true, [x]) ] 
            ];
        seltrees:
            [
                [ LIDENT "optimal" -> `BestN None ] |
                [ LIDENT "unique" -> `Unique ] |
                [ LIDENT "best"; ":"; x = INT -> `BestN (Some (int_of_string x)) ] |
                [ LIDENT "within"; ":"; x = integer_or_float -> 
                    `BestWithin (float_of_string x) ] |
                [ LIDENT "random"; ":"; x = INT -> `RandomTrees (int_of_string x) ]
            ];
        (* Renaming characters or taxa *)
        rename:
            [
                [ rename_cmd; left_parenthesis; x = LIST0 rename_argument SEP ","; 
                    right_parenthesis -> `Rename x ]
            ];
        rename_cmd:
            [
                [ LIDENT "synonymize" ] |
                [ LIDENT "rename" ]
            ];
        rename_argument:
            [
                [ x = charortax -> (x :> renamea) ] |
                [ x = STRING -> `File x ] | 
                [ left_parenthesis; a = STRING; ","; b = STRING; right_parenthesis -> 
                    `Syn (a, b) ]
            ];
        (* POY commands *)
        command:
            [
                [ t = read -> (t :> command) ] |
                [ t = build -> (t :> command) ] |
                [ t = swap -> (t :> command) ] |
                [ t = search -> (t :> command) ] |
                [ t = calculate_support -> (t :> command) ] |
                [ t = perturb -> (t :> command) ] | 
                [ t = transform -> (t :> command) ] |
                [ t = report -> (t :> command) ] |
                [ t = select -> (t :> command) ] |
                [ t = rename -> (t :> command) ] |
                [ t = application_command -> (t :> command) ] |
                [ t = fuse -> (t :> command) ] |
                [ t = loop -> (t :> command) ]
            ];
        loop:
            [
                [ LIDENT "repeat"; x = INT; LIDENT "times"; com = LIST0 command; 
                    LIDENT "end" -> `Repeat (int_of_string x, com) ] 
            ];
        read:
            [
                [ LIDENT "read"; left_parenthesis; a = LIST0 read_argument SEP ","; 
                    right_parenthesis -> `Read a ]
            ];
        build:
            [
                [ LIDENT "build"; left_parenthesis; a = LIST0 build_argument SEP ","; 
                    right_parenthesis -> `Build a ]
            ];
        swap:
            [
                [ LIDENT "swap"; left_parenthesis; a = LIST0 swap_argument SEP ","; 
                    right_parenthesis -> (`Swap a :> swap) ]
            ];
        search:
            [
                [ LIDENT "search"; left_parenthesis; a = LIST0 search_argument SEP
                ","; right_parenthesis -> `Search a ]
            ];
        fuse:
            [
                [ LIDENT "fuse"; left_parenthesis; a = LIST0 fuse_argument SEP ","; 
                    right_parenthesis -> (`Fuse a) ]
            ];
        fuse_argument:
            [
                [ LIDENT "keep"; ":"; i = INT -> `Keep (int_of_string i) ]
            |   [ LIDENT "iterations"; ":"; i = INT -> `Iterations (int_of_string i) ]
            |   [ LIDENT "replace"; ":"; r = fuseareplace -> `Replace r]
            |   [ x = swap -> (x :> fusea) ]
            |   [ LIDENT "weighting"; ":"; w = fuseaweighting -> `Weighting w]
            |   [ LIDENT "clades"; ":"; cfrom = INT; cto = OPT fusea_cto ->
                      `Clades (int_of_string cfrom, cto)]
            ];
        fuseareplace:
            [ [ LIDENT "better" -> `Better ]
            | [ LIDENT "best" -> `Best ] ];
        fuseaweighting:
            [ [ LIDENT "uniform" -> `Uniform ] ];
        fusea_cto:
            [ [ "-"; cto = INT -> int_of_string cto ] ];
        calculate_support:
            [
                [ LIDENT "calculate_support"; left_parenthesis; a = LIST0
                support_argument SEP ","; 
                right_parenthesis -> `Support a ]
            ];
        (* Reading a file *)
        read_argument:
            [ 
                [ LIDENT "annotated"; ":"; left_parenthesis; a = LIST1 otherfiles SEP ","; 
                    right_parenthesis -> ((`AnnotatedFiles a) :> Methods.input) ] |
                [ x = otherfiles -> (x :> Methods.input) ]
            ];
        otherfiles:
            [
                [ f = STRING -> `AutoDetect [`Local f] ] |
                [ LIDENT "nucleotides"; ":"; left_parenthesis; a = LIST1 STRING SEP ","; 
                    right_parenthesis -> `Nucleotides (to_local a) ] |

                [ LIDENT "chromosome"; ":"; left_parenthesis; a = LIST1 STRING SEP ","; 
                    right_parenthesis -> `Chromosome (to_local a) ] |

                [ LIDENT "genome"; ":"; left_parenthesis; a = LIST1 STRING SEP ","; 
                    right_parenthesis -> `Genome (to_local a) ] |

                [ LIDENT "aminoacids"; ":"; left_parenthesis; a = LIST1 STRING SEP ","; 
                    right_parenthesis -> `Aminoacids (to_local a) ] |
             
                [ LIDENT "custom_alphabet"; ":"; left_parenthesis; seq = STRING;","; cost_mat = STRING; OPT ",";
                  read_options = LIST0 read_optiona SEP ","; right_parenthesis 
                      -> `GeneralAlphabetSeq (`Local seq, `Local cost_mat, read_options)  ] |

                [ LIDENT "breakinv"; ":"; left_parenthesis; seq = STRING; ","; cost_mat = STRING; OPT ",";
                  read_options = LIST0 read_optiona SEP ","; right_parenthesis 
                      -> `Breakinv (`Local seq, `Local cost_mat, read_options)  ] |

                [ LIDENT "complex"; left_parenthesis; a = LIST1 STRING SEP ","; 
                    right_parenthesis -> `ComplexTerminals (to_local a) ] 
            ];

        read_optiona:
            [
                [LIDENT "init3D"; ":"; init3D = boolean -> `Init3D init3D] |
                [LIDENT "orientation"; ":"; ori = boolean -> `Orientation ori] 
            ];

        tree_information_list:
            [   
                [ ":"; "("; x = LIST0 tree_information SEP ","; ")" -> x ]
            ];
        tree_information:
            [
                [ LIDENT "_cost" -> `Cost ] |
                [ LIDENT "hennig" -> `HennigStyle ] |
                [ LIDENT "total" -> `Total ] |
                [ LIDENT "newick" -> `Newick ] |
                [ LIDENT "margin"; ":"; m = INT -> `Margin (int_of_string m) ] |
                [ LIDENT "nomargin" -> `Margin max_int] |
                [ x = collapse -> (x :> Methods.information_contained)  ]

            ];
        collapse:
            [
                [ LIDENT "collapse"; y = OPT optional_boolean -> 
                    match y with
                    | Some y -> `Collapse y 
                    | None -> `Collapse true ]
            ];
        optional_collapse:
            [ 
                [ ":"; x = collapse -> x ]
            ];
        optional_boolean:
            [
                [ ":"; x = boolean -> x ]
            ];
        boolean: 
            [
                [ LIDENT "true" -> true ] |   
                [ LIDENT "false" -> false ]    
            ];
        (* Building a tree *)
        join_method:
            [
                [ LIDENT "sectorial"; x = OPT integer -> 
                    ((`UnionBased x) : Methods.tabu_join_strategy)  ] |
                [ LIDENT "all"; x = OPT integer -> `AllBased x ] |
                [ LIDENT "constraint"; ":"; left_parenthesis; 
                    x = LIST1 constraint_options SEP ","; right_parenthesis
                    -> `Partition x ] |
                [ LIDENT "constraint" -> `Partition [] ]
            ];
        build_argument:
            [
                [ x = threshold_and_trees -> (x :> builda) ] |
                [ x = build_method -> (x :> builda) ] |
                [ x = join_method -> (x :> builda) ] |
                [ x = keep_method -> (x :> builda) ] |
                [ x = cost_calculation -> (x :> builda) ]
            ];
        threshold_and_trees:
            [
                [ LIDENT "threshold"; ":"; x = integer_or_float -> `Threshold
                (float_of_string x) ] |
                [ LIDENT "trees"; ":"; x = INT -> `Trees (int_of_string x) ] |
                [ x = INT -> `Trees (int_of_string x) ]
            ];
        build_method:
            [
                [ x = STRING -> `Prebuilt (`Local x) ] |
                [ LIDENT "of_file"; ":"; x = STRING -> `Prebuilt (`Local x) ] |
                [ LIDENT "randomized" -> `Random ] |
                [ LIDENT "as_is" -> `Ordered ] |
                [ LIDENT "_mst" -> `Mst ] |
                [ LIDENT "_distances" -> `DistancesRnd ]
            ];
        keep_method:
            [ 
                [ LIDENT "last" -> `Last ] |
                [ LIDENT "first" -> `First ] | 
                [ LIDENT "at_random" -> `Keep_Random ] 
            ];
        cost_calculation:
            [
                [ x = transform -> (x :> cost_calculation) ] |
                [ LIDENT "exact" -> `Exact ]
            ];
        (* Swaping *)
        search_argument:
            [ 
                [ LIDENT "build"; x = OPT optional_boolean -> 
                    (match x with
                    | None -> `Build true
                    | Some x -> `Build x) ] |
                [ LIDENT "transform"; x = OPT optional_boolean -> 
                    match x with
                    | None -> `Transform false
                    | Some x -> `Transform x ]
            ];
        swap_argument:
            [
                [ x = sample_method -> (x :> swapa) ] |
                [ x = swap_method -> (x :> swapa) ] |
                [ x = keep_method -> (x :> swapa) ] |
                [ x = threshold_and_trees -> (x :> swapa) ] |
                [ x = cost_calculation -> (x :> swapa) ] |
                [ LIDENT "forest"; a = OPT optional_integer_or_float -> 
                    match a with
                    | None -> `Forest 0.
                    | Some a -> `Forest (float_of_string a) ] |   
                [ a = trajectory_method -> a ] |
                [ a = break_method -> a ] |
                [ a = reroot_method -> a ] |
                [ a = join_method -> (a :> swapa) ] 
            ];
        trajectory_method:
            [
                [ LIDENT "annealing"; ":"; left_parenthesis; 
                  coeff = integer_or_float;
                  ","; exp = integer_or_float; right_parenthesis ->
                      `Annealing (float_of_string coeff,
                                  float_of_string exp)] |
                [ LIDENT "drifting"; ":"; left_parenthesis;
                    equalprob = integer_or_float; ","; 
                    worstfactor = integer_or_float; right_parenthesis ->
                        `PoyDrifting (float_of_string equalprob, float_of_string
                        worstfactor) ] |
                [ LIDENT "current_neighborhood"; f = OPT string_arg -> 
                    `AllAround f ] |
                [ LIDENT "around" -> `AllThenChoose ]
            ];
        sample_method:
            [
                [ LIDENT "timeout"; ":"; x = integer_or_float -> 
                    `TimeOut (float_of_string x) ] |
                [ LIDENT "timedprint"; ":"; left_parenthesis; x = integer_or_float; ","; 
                    y = STRING; right_parenthesis -> 
                        `TimedPrint (float_of_string x, Some y) ] |
                [ LIDENT "visited"; x = OPT string_arg ->
                    `AllVisited x ] |
                [ LIDENT "trajectory"; x = OPT string_arg -> 
                        `PrintTrajectory x ] |
                [ LIDENT "recover" -> `KeepBestTrees ] |
                [ LIDENT "_unionstats"; ":"; left_parenthesis; 
                    x = STRING; ","; y = INT; right_parenthesis ->
                    `UnionStats (Some x, int_of_string y) ] |
                [ LIDENT "_rootuniondistr"; ":"; x = STRING -> 
                    `RootUnionDistr (Some x) ] |
                [ LIDENT "_attemptsdistr"; ":"; x = STRING ->
                    `AttemptsDistr (Some x) ] |
                [ LIDENT "_breakvsjoin"; x = OPT string_arg ->
                    `BreakVsJoin x ]
            ];
        swap_method:
            [
                [ LIDENT "spr"; y = OPT single_option -> 
                    match y with
                    | None -> `ChainNeighborhoods `Spr 
                    | Some _ -> `SingleNeighborhood `Spr
                ] |
                [ LIDENT "tbr"; y = OPT single_option -> 
                    match y with
                    | None -> `ChainNeighborhoods `Tbr 
                    | Some _ -> `SingleNeighborhood `Tbr] |
                [ LIDENT "alternate" -> `Alternate (`Spr, `Tbr) ]
            ];
        single_option:
            [
                [ ":"; LIDENT "once" -> 1 ]
            ];
        break_method:
            [
                [ LIDENT "randomized" -> `Randomized ] |
                [ LIDENT "distance" -> `DistanceSorted ] |
                [ LIDENT "once" -> `OnlyOnce ]
            ];
        constraint_options:
            [
                [ x = INT -> `MaxDepth (int_of_string x) ] |
                [ LIDENT "depth"; ":"; x = INT -> `MaxDepth (int_of_string x) ] |
                [ x = STRING -> `ConstraintFile (`Local x) ] |
                [ LIDENT "file"; ":"; x = STRING -> `ConstraintFile (`Local x) ]
            ];
        reroot_method:
            [
                [ LIDENT "bfs"; x = OPT integer -> `Bfs x ]
            ];
        integer:
            [
                [ ":"; x = INT -> int_of_string x ]
            ];
        string_arg:
            [
                [ ":"; x = STRING -> x ]
            ];
        (* Support values *)
        support_argument:
            [
                [ x = build -> (x :> supporta) ] |
                [ x = swap -> (x :> supporta) ] |
                [ x = support_method -> (x :> supporta) ]
            ];
        support_method:
            [
                [ LIDENT "bremer" -> `Bremer ] |
                [ LIDENT "jackknife"; x = OPT list_of_jackknifea ->
                    match x with
                    | None -> `Jackknife []
                    | Some v -> `Jackknife v ] |
                [ LIDENT "bootstrap"; x = OPT integer -> `Bootstrap x ]
            ];
        opt_support_names:
            [
                [ ":"; y = support_names -> y ]
            ];
        support_names:
            [
                [ LIDENT "bremer"; ":"; x = STRING -> `Bremer (Some (`Local x)) ] |
                [ LIDENT "bremer" -> `Bremer None ] |
                [ LIDENT "jackknife" -> `Jackknife ] |
                [ LIDENT "bootstrap" -> `Bootstrap ] 
            ];
        list_of_jackknifea:
            [
                [ ":"; left_parenthesis; x = LIST0 jackknifea SEP ","; 
                    right_parenthesis -> x ]
            ];
        jackknifea:
            [
                [ LIDENT "remove"; ":"; x = integer_or_float -> `Select x ] |
                [ LIDENT "resample"; ":"; x = INT -> `Resample x ]
            ];
        (* Shared items *)
        left_parenthesis: [ [ "(" ] ];
        right_parenthesis: [ [ ")" ] ];
        identifiers:
            [
                [ LIDENT "not" ; LIDENT "files"; ":"; left_parenthesis; x = LIST0 STRING SEP ","; 
                    right_parenthesis -> `Files (false, x) ] |
                [ x = old_identifiers -> (x :> identifiers) ] |
                [ LIDENT "files"; ":"; left_parenthesis; x = LIST0 STRING SEP ","; 
                    right_parenthesis -> `Files (true, x) ] 
            ];
        old_identifiers:
            [
                [ LIDENT "all" -> `All ] |
                [ LIDENT "not"; LIDENT "names"; ":"; left_parenthesis; x = LIST0 STRING SEP ","; 
                    right_parenthesis -> `Names (false, x) ] |
                [ LIDENT "not"; LIDENT "codes"; ":"; left_parenthesis; x = LIST0 INT SEP ","; 
                    right_parenthesis -> `Some (false, List.map int_of_string x) ] |
                [ LIDENT "names"; ":"; left_parenthesis; x = LIST0 STRING SEP ","; 
                    right_parenthesis -> `Names (true, x) ] |
                [ LIDENT "codes"; ":"; left_parenthesis; x = LIST0 INT SEP ","; 
                    right_parenthesis -> `Some (true, List.map int_of_string x) ] |
                [ LIDENT "static" -> `AllStatic ] | 
                [ LIDENT "dynamic" -> `AllDynamic ] |
                [ LIDENT "missing"; ":"; x = INT -> 
                    `Missing (true, 100 - int_of_string x) ] |
                [ LIDENT "not"; LIDENT "missing"; ":"; x = INT -> `Missing
                (false, 100 - int_of_string x) ] |
                [ LIDENT "_random"; ":"; x = INT -> `Random (int_of_string x) ]
            ];
        optional_integer_or_float: 
            [
                [ ":"; x = integer_or_float -> x ]
            ];
        neg_integer_or_float:
            [
                [ "-"; x = integer_or_float -> "-" ^ x ] |
                [ x = integer_or_float -> x ]
            ];
        integer_or_float:
            [
                [ x = INT -> x ] |
                [ x = FLOAT -> x ]
            ];
        string_or_ident:
            [
                [ x = STRING -> x ]
            |   [ x = LIDENT -> x ]
            ];
    END
    ;
    expr

let ( --> ) a b = b a

let rec process_commands optimize command = 
    match command with
    | `ChangeWDir dir -> let dir = simplify_directory dir in Sys.chdir dir; [command]
    | `ReadScript files -> read_script_files false files
    | x -> [x]

and read_script_files optimize files = 
    let res = 
        List.map
        (fun f -> 
            try
                let comm = of_file false f in
                `Echo ("Running file " ^ f, `Information) :: comm
            with
            | Stdpp.Exc_located ((a, b), Stream.Error err) 
            | Stdpp.Exc_located ((a, b), Token.Error err) ->
                    let is_unknown = "illegal begin of expr" = err in
                    let msg = "@[<v 4>@[Command@ error@ in@ file@ @{<b>" ^
                    f ^ "@}@ line@ @{<b>" ^ string_of_int a.Lexing.pos_lnum ^ 
                    "@}@ between@ characters@ @{<b>" ^ 
                    string_of_int (a.Lexing.pos_cnum - a.Lexing.pos_bol)
                    ^ "@} and @{<b>" ^
                    string_of_int (b.Lexing.pos_cnum - b.Lexing.pos_bol)
                    ^ "@} :@]@,@[" ^
                    (if is_unknown then "Unknown command" else
                        err) ^ "@]@]\n" in
                    Status.user_message Status.Error msg;
                    failwith "Script execution stopped"
            | err -> 
                    Status.user_message Status.Error 
                    ("Error@ while@ processing@ script@  " ^ f);
                    raise err) 
        files 
    in
    do_analysis optimize (List.flatten res)

and do_analysis optimize res =
    if optimize then Analyzer.analyze res
    else res

and simplify_directory dir = 
    Str.global_replace (Str.regexp "\\\\ ") " " dir 

and of_stream optimize str =
    let cur_directory = Sys.getcwd () in
    let lexer = Plexer.gmake () in
    let expr = create_expr lexer in
    let res = 
        str 
        --> Grammar.Entry.parse expr
        --> transform_all_commands
        --> List.map (process_commands false)
        --> List.flatten
        --> do_analysis optimize
    in
    let cur_directory = simplify_directory cur_directory in
    Sys.chdir cur_directory;
    res

and of_channel optimize ch = 
    of_stream optimize (Stream.of_channel ch)

and of_file optimize f =
    let ch = open_in f in
    let r = of_channel optimize ch in
    close_in ch;
    r

and of_string optimize str =
    if StatusCommon.redirect_information () then
        let file = StatusCommon.redirect_filename () in
        Status.user_message (Status.Output ((Some file), false, [])) (str ^ "@\n")
    else ();
    of_stream optimize (Stream.of_string str)
