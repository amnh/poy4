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

let () = SadmanOutput.register "UtlGrappa" "$Revision: 1165 $"

(** utlGrappa module provides functions for rearrangement 
* operations such as computing inversion, breakpoint distances 
* between two gene orders arrays *)

let fprintf = Printf.fprintf 

(** [standardize genomeX genomeY] standardizes  
 * gene order [genomeX=(x1, x2,..., xk)] and  one of its 
 * permutations [genomeY=(y1, y2,...,yk)] 
 * into [sta_X] and [sta_Y] such that [genomeX=(1,...,k)]) 
 * For example: [genomeX] = (5, -3, 2) and [genomeY] = (-2, 3, 5)
 * [sta_X] = (1, 2, 3) and [sta_Y] = (-3, -2, 1 *)
let standardize genomeX genomeY = 
    let max_index = Array.fold_left 
        (fun max_gene gene -> max max_gene (abs gene) ) (-1) genomeX 
    in 

    let num_gene = Array.length genomeX in  
    let sta_genomeX = Array.init num_gene (fun index -> index + 1) in

    let index_arr = Array.make (max_index + 1) 0 in     
    for idx = 0 to num_gene - 1 do
        match genomeX.(idx) > 0 with
        | true -> index_arr.( genomeX.(idx) ) <- (idx + 1)
        | false -> index_arr.( -genomeX.(idx) ) <- -(idx + 1)
    done; 

    let sta_genomeY = Array.init num_gene 
        (fun idx -> 
             match genomeY.(idx) > 0 with 
             | true -> index_arr.(genomeY.(idx)) 
             | false -> -index_arr.(-genomeY.(idx)) 
        )
    in  
    sta_genomeX, sta_genomeY



(** [cmp_inversion_dis genomeX genomeY circular] computes
 * the inversion distance between two given gene orders 
 * [genomeX=(x1, x2, ... xk)] and [genomeY=(y1, y2,..yk)]. 
 * Note [genomeY] is a permutation of [genomeX].
 * Compute the inversion distance between [genomeX] and [genomeY] using GRAPPA functions
 * For example: [genomeX] = (-6, 1, 5), [genomeY] = (-5, 1, 6) *)
let cmp_inversion_dis (genomeX : int array) (genomeY : int array) circular  =
    let sta_genomeX, sta_genomeY = standardize genomeX genomeY in
                        
    let num_gen = Array.length genomeX in  
    let genome_arr = Grappa.c_create_empty_genome_arr 2 num_gen in  
    for index = 0 to num_gen - 1 do
        Grappa.c_set genome_arr 0 index sta_genomeX.(index);   
        Grappa.c_set genome_arr 1 index sta_genomeY.(index);   
        
    done;

    let g0 = Grappa.c_get_one_genome genome_arr 0 in
    let g1 = Grappa.c_get_one_genome genome_arr 1 in  

    let inv_dis = Grappa.c_cmp_inv_dis g0 g1 num_gen circular in   
    inv_dis 
 



(** [cmp_breakpoint_dis genomeX genomeY circular] computes
 * the breakpoint distance between two given gene orders 
 * [genomeX=(x1, x2, ... xk)] and [genomeY=(y1, y2,..yk)]. 
 * Note that orientations are ignored. *)
let cmp_breakpoint_dis (genomeX : int array) (genomeY : int array) circular = 

    let _, sta_genomeY = standardize genomeX genomeY in 
                        
    let sta_genomeY = Array.map (fun gene -> abs gene) sta_genomeY in 

    let num_gene = Array.length sta_genomeY in     
    let dis = ref 0 in 
    for pos = 0 to num_gene - 2 do
        if abs(sta_genomeY.(pos) - sta_genomeY.(pos + 1) ) != 1 then 
            dis := !dis + 1
    done;
    

    match circular with
    | 0 -> !dis 
    | _ -> 
          let cir_dis = abs ( sta_genomeY.(0) - sta_genomeY.(num_gene - 1) ) in 
          if (cir_dis = 1) || (cir_dis = num_gene - 1) then !dis
          else !dis + 1


(** [cmp_oriented_breakpoint_dis genomeX genomeY circular] computes
 * the breakpoint distance between two given gene orders 
 * [genomeX=(x1, x2, ... xk)] and [genomeY=(y1, y2,..yk)]. 
 * Note that orientations are taken into account. 
 * For example: [genomeX] = (-6, 1, 5), [genomeY] = (-5, 1, 6) *)
let cmp_oriented_breakpoint_dis (genomeX : int array) (genomeY : int array)
        circular = 
    let _, sta_genomeY = standardize genomeX genomeY in
                        
    let num_gene = Array.length sta_genomeY in 
    let dis = ref 0 in 
    for pos = 0 to num_gene - 2 do
        if sta_genomeY.(pos) + 1 != sta_genomeY.(pos + 1) then
            dis := !dis + 1
    done;
    

    match circular with
    | 0 -> !dis 
    | _ -> 
          let fi_g = sta_genomeY.(0) 
          and la_g = sta_genomeY.(num_gene -1 ) in 
          if ( (la_g = num_gene) && (fi_g = 1) ) || 
              ( (la_g = -1) && (fi_g = -num_gene) ) || 
              (la_g + 1 = fi_g) then !dis
          else !dis + 1

(** [get_ordered_permutation genomeX] returns 
 * the ordered permutation [genomeY]=(y1,..,yk | y1 < ... < yk) 
 * of [genomeX] =(x1, x2, ... xk), 
 * For example. [genomeX] = (-6, 1, 5), [genomeY] = (1, 5, 6) *)
let get_ordered_permutation genomeX = 
    let max_index = Array.fold_left 
        (fun max_gene gene -> max max_gene (abs gene) ) (-1) genomeX 
    in 

    let num_gene = Array.length genomeX in  
    let index_arr = Array.make (max_index + 1) (-1) in 
    for idx = 0 to num_gene - 1 do
        index_arr.( abs genomeX.(idx) ) <- idx;
    done; 

    let genomeY_ls = 
        Array.fold_right (fun idx genomeY ->
                          match idx with
                          | -1 -> genomeY
                          | _ -> (abs genomeX.(idx))::genomeY) index_arr []
    in 
    Array.of_list genomeY_ls
    

(** [cmp_self_inversion_dis genome circular] 
 * computes the inversion distance between a gene orders [genome=(x1, x2, ... xk)]
 * and [Y=(y1,..,yk | y1 < ... < yk) using GRAPPA functions
 * where [Y] is an ordered permutation of |genome|
 * For example. [genome] = (-6, 1, 5), Y = (1, 5, 6) *)
let cmp_self_inversion_dis (genome : int array)  circular  =
    let ordered_permutation = get_ordered_permutation genome in     
    let dis = cmp_inversion_dis genome ordered_permutation circular in 
    dis


(** [cmp_self_breakpoint_dis genome circular] 
 * computes the breakpoint distance between a gene orders [genome=(x1, x2, ... xk)]
 * and [Y=(y1,..,yk | y1 < ... < yk) using GRAPPA functions
 * where [Y] is an ordered permutation of |genome|
 * For example. [genome] = (-6, 1, 5), Y = (1, 5, 6) *)
let cmp_self_breakpoint_dis (genome : int array)  circular  =
    let ordered_permutation = get_ordered_permutation genome in 
    cmp_breakpoint_dis genome ordered_permutation circular



(** [cmp_self_oriented_breakpoint_dis genome circular] 
 * computes the breakpoint distance between a gene orders [genome=(x1, x2, ... xk)]
 * and [Y=(y1,..,yk | y1 < ... < yk) using GRAPPA functions
 * where [Y] is an ordered permutation of |genome|
 * For example. [genome] = (-6, 1, 5), Y = (1, 5, 6) *)
let cmp_self_oriented_breakpoint_dis (genome : int array)  circular  =
    let ordered_permutation = get_ordered_permutation genome in 
    cmp_oriented_breakpoint_dis genome ordered_permutation circular


let is_odd x = 1 = (1 land x)

let create_breakpoint_graph genome = 
    let len = 2 * ((Array.length genome) + 1) in
    Array.make len (-1)

let debug = false 


let add_grey_edge a b breakpoint_graph = 
    let apos =
        (2 * (abs a)) - (if a < 0 then 1 else 0)
    in
    let bpos = 
        (2 * (abs b)) - (if b > 0 then 1 else 0)
    in
    if debug then Printf.printf "Adding the edge %d - %d yields %d - %d\n%!" a b apos bpos;
    breakpoint_graph.(apos) <- (bpos);
    breakpoint_graph.(bpos) <- (apos)

type color = Black | Grey

let rec find_end color pos bp = 
    match color with
    | Black -> 
            let pos =
                if is_odd pos then pos - 1
                else pos + 1
            in
            find_end Grey pos bp
    | Grey -> 
            if bp.(pos) = ~-1 then pos
            else find_end Black (bp.(pos)) bp

let print_breakpoint_graph bp =
    Printf.printf "Will print the breakpoint graph\n%!";
    Array.iteri (Printf.printf "%d\t%d\n%!") bp

let count_cycles bp =
    let counter = ref 1 
    and start = ref 0
    and cycle_len = ref 0 
    and current_starting_point = ref 0 
    and bp_len = Array.length bp 
    and color = ref Black in
    while !current_starting_point < bp_len do
        if bp.(!start) = ~-1 then begin
            (* We have found a cicle *)
            while !current_starting_point < bp_len && 
                bp.(!current_starting_point) = ~-1 do
                incr current_starting_point;
            done;
            start := !current_starting_point;
            cycle_len := 0;
            if !start < bp_len && !cycle_len > 2 then incr counter;
        end else begin
            let start_contents = bp.(!start) in
            incr cycle_len;
            bp.(!start) <- ~-1;
            match !color with
            | Black -> 
                    (* We are pursuing the black edge *)
                    if is_odd !start then decr start 
                    else incr start;
                    color := Grey;
            | Grey ->
                    start := start_contents;
                    color := Black;
        end;
    done;
    !counter

let make_breakpoint_graph genome = 
    let len = Array.length genome in
    let bp = create_breakpoint_graph genome in 
    for i = 0 to len - 2 do
        add_grey_edge genome.(i) genome.(i + 1) bp;
    done;
    if debug then print_breakpoint_graph bp;
    let grey_first_cap = 
        if genome.(0) > 0 then (2 * genome.(0)) - 1
        else (abs genome.(0)) * 2
    in
    let grey_last_cap =
        if genome.(len - 1) > 0 then (2 * genome.(len - 1))
        else ((abs genome.(len - 1)) * 2) - 1
    in
    let bp_len = Array.length bp in
    bp.(0) <- grey_first_cap;
    if debug then Printf.printf "The grey first cap is %d and the other is %d\n%!"
    grey_first_cap grey_last_cap;
    bp.(grey_first_cap) <- 0;
    bp.(bp_len - 1) <- grey_last_cap;
    bp.(grey_last_cap) <- bp_len - 1;
    bp

let cmp_cicles genome1 genome2 circular = 
    let genome2 = snd (standardize genome1 genome2) in
    let graph = make_breakpoint_graph genome2 in
    count_cycles graph 

let count_relevant_grays graph =
    let cnt = ref 0 in
    let len = Array.length graph in
    for i = 0 to  len - 1 do 
        if 1 < (abs (graph.(i) - i)) then incr cnt;
    done;
    (!cnt) / 2

let cmp_oriented_dcj genome1 genome2 circular = 
    if genome2 = genome1 then 0
    else
        let genome1, genome2 = standardize genome1 genome2 in
        let graph = make_breakpoint_graph genome2 in
        if debug then print_breakpoint_graph graph;
        let cicles = count_cycles (Array.copy graph) in
        let breakpoints = count_relevant_grays graph in
        let res = breakpoints - cicles in
        if res < 0 then begin
            Array.iter (Printf.printf "%d ") genome1;
            print_newline ();
            Array.iter (Printf.printf "%d ") genome2;
            print_newline ();
            print_int breakpoints;
            print_newline ();
            print_int cicles;
            print_breakpoint_graph graph;
            assert false;
        end else res


