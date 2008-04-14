module Nodes = AllDirNode.AllDirF
module Edges = Edge.LazyEdge
module TreeOps = AllDirChar.F
module CharOps = AllDirChar.CharScripting

module M = Scripting.Make (Nodes) (Edges) (TreeOps) (CharOps)

open M
include M

let () =
    Status.user_message Status.Information Version.string

let seed = truncate (Unix.time ())

let _ = process_random_seed_set (empty ()) seed
