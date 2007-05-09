(* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    *)
(* Copyright (C) 2007  Andrés Varón, Le Sy Vinh, Illya Bomash, Ward Wheeler,  *)
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

module type S = sig
    type a 
    type b 
    type c
    type d

    type inform_master = Do of (string * exn) | Dont
    type checkpoint = 
        (Tree.u_tree, Data.d, d, c) 
        Methods.checkpoints

    type parallel_results = 
        (Tree.u_tree Sexpr.t, Data.d, c Sexpr.t, d Sexpr.t)
        Methods.parallel_input

    type parallel_results_data = parallel_results list

    val send_dead_signal_and_exit : inform_master -> unit
    val send_dead_signal : inform_master -> unit

    val n_tasks :
      string -> string ->
          [< checkpoint ] -> int -> parallel_results_data

    val distribute :
      string -> string -> [< checkpoint ] -> Tree.u_tree
      Sexpr.t list -> parallel_results_data 

    val broadcast : [< checkpoint] -> unit

    val broadcast_verbosity : Methods.verbosity -> unit


    val parallel_pipeline :  
        string ->
            string ->
                ((a, b, c) Scripting.run -> Methods.script -> (a, b, c)
                Scripting.run) ->
                    ((a, b, c) Scripting.run ->
                        (Tree.u_tree Sexpr.t, Data.d, c Sexpr.t,
                        d Sexpr.t)
                        Methods.parallel_input -> (a, b, c) Scripting.run) ->
                            (a, b, c) Scripting.run 
                            -> int * Methods.script list * Methods.script list *
                            Methods.script list -> (a, b, c) Scripting.run


    val answer : [< checkpoint] ->
        unit

    val all_down : unit -> unit

    val distribute_sexpr : string -> string ->
        checkpoint Sexpr.t -> checkpoint Sexpr.t

    val process_error_message : Methods.parallel_special_condition -> unit

    val report_exception : string -> exn -> [< `Caught | `Uncaught ] -> unit

    val initialize_random_seed : (string -> [`Random_Seed of int]) -> unit

end

module Make (Node : NodeSig.S) (Edge : Edge.EdgeSig with type n = Node.n) 
    (CScrp : CharacterScripting.S with type n = Node.n) 
    (P : Process.S with type a = Node.n with type b = Edge.e with type c =
        CScrp.cs with type d = float) : 
        S with type a = Node.n with type b = Edge.e 
        with type c = CScrp.cs with type d = float
