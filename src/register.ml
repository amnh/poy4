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

(* $Id: register.ml 1644 2007-02-14 19:05:47Z andres $ *)
let () = SadmanOutput.register "Register" "$Revision: 1644 $"


exception Func_Not_Found;;

type all_parallel =
    | Two_on_array_ret_float of 
        ( ( int -> int ) -> (int -> float) -> int array ref -> float )
    | One_on_array_ret_array of 
        ( ( int -> int ) -> int array ref -> int array ref )
    | Two_on_array_ret_array of 
        ( ( int -> int) -> (int -> float) -> int array ref -> float -> int array ref)
    | One_bool_on_array_ret_array of 
        ( (int -> bool) -> int array ref -> int array ref)

    | Int_ret_int of (int -> int)
    | Int_ret_float of (int -> float)
    | Int_ret_bool of (int -> bool)
    | Nil

(* This is the basic functionality for registered functions in parallel
* processing. This functions should probably be organized in a binary tree for
* faster access to the codes employeed, but should be done later. *)
(*let _functions = ref [] *)
let _fun_counter = ref (0)
and _reg_counter = ref (100);;

(* Some standard tags not associated with functions *)
let process_failed = 1
and process_failed_dummy = 2  
and exec_error = 3
and exec_error_dummy = 4 

and gather_within_threshold_tag = 5 
and gather_within_threshold_resp = 6;;
 

let map_tag = 7
and map_resp = 8
and scan_tag = 9
and scan_resp = 10
and all_maximize_tag = 11
and all_maximize_resp = 12
and all_minimize_tag = 13
and all_minimize_resp = 14
and gather_maximum_tag = 15
and gather_maximum_resp = 16
and gather_minimum_tag = 17
and gather_minimum_resp = 18

and set_pair_sum_tag = 19
and set_pair_sum_resp = 20
and set_pair_fold_tag = 21 
and set_pair_fold_resp = 22

let _next_fun () = 
    _fun_counter := !_fun_counter + 1;
    !_fun_counter;;

let _next_reg () = 
    _reg_counter := !_reg_counter + 1;
    !_reg_counter;;

(* use this function to register the function tags that are used for tagging
* purposes. it is the first function that has to be called before calling any 
* other functions in the Register library. *)

    
let function_registery = ref (Hashtbl.create 24) ;;
let tag_registery = ref (Hashtbl.create 24);;
let resp_registery = ref (Hashtbl.create 24);;


let register_init () = 
    while (!_fun_counter <= 14) do
        let tag = _next_fun () 
        and ans = _next_fun () in
        
        Hashtbl.add !tag_registery tag (Nil, tag, ans) ; 
        Hashtbl.add !resp_registery ans (Nil, tag, ans) ;
    done;;
    


let register f =
    let tag = _next_reg () 
    and ans = _next_reg () in
    
    Hashtbl.add !function_registery f (f,tag,ans);
    Hashtbl.add !tag_registery tag (f,tag,ans); 
    Hashtbl.add !resp_registery ans (f,tag,ans);
    
    (tag, ans);;


let register_asynch f =
    let tag = _next_fun () 
    and ans = _next_fun () in
    
    Hashtbl.add !function_registery f (f, tag, ans) ;
    Hashtbl.add !tag_registery tag (f, tag, ans) ; 
    Hashtbl.add !resp_registery ans (f, tag, ans) ;
    
    (tag, ans);;
    
let tag response =
    try
        Hashtbl.find !resp_registery response 
    with
    |Not_found ->
            let error_message = ("No such registered tag") in 
            Status.user_message Status.Error error_message; 
            raise (Not_found);;

let funct _f =
    try
        Hashtbl.find !function_registery _f 
    with
    |Not_found ->
            let error_message = ("No such registered function") in 
            Status.user_message Status.Error error_message; 
            raise (Not_found);;

(* get a tag, return the triple*)
let response tag =
    try
        Hashtbl.find !tag_registery tag 
    with
    |Not_found ->
            let error_message = ("No such registered response tag") in 
            Status.user_message Status.Error error_message;
            raise (Not_found);;

(* vim:sw=4 et tw=80
 *)
