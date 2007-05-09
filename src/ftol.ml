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

let () = SadmanOutput.register "Ftol" "$Revision: 1644 $"

let debug = false

exception Cannot_send_message;;

type role = | Slave | Master | Serial

type rank = Nil | Rank of int

let task_array = ref[||] 
let mr= ref 0
let number_of_proc = ref 0
let nbr_controller = ref 0
let initialized_array = ref [||]
let communicators_array = ref[||]
let intercommunicators_array = ref [||]
let status_communicators_array = ref [||]
let roles_array = ref[||]
let list_of_slaves = ref []
let bosses_array = ref[||]

let my_rank () = Rank !mr

let not_assigned = -3

let int_of_rank = function
    | Rank a -> a
    | Nil -> not_assigned

let rank_of_int int_rank = 
    if int_rank >= 0 then Rank int_rank
    else Nil

let add_to_array original new_element = Array.append !original [|new_element|]
(* [add_to_array original new_element] returns a fresh array the contents
of the [array ref original] and the [new_element] appended to the end. *)


(* Code for logging error messages *)
let errfile = ref stderr

(* [make_intercommunicator tid] attempts to create a new intercommunicator
between the calling process communicator (saved in
[!communicators_array.(!mr)]) and the process with rank [rank] in
[comm_world].*) 
let make_intercommunicator rankk=
    let msg = "Creating@ intercommunicator@ with@ rank@ " ^ string_of_int rankk 
    in
    if debug then begin
        print_endline msg;
        flush stdout;
        Status.user_message Status.Information msg;
    end;
    try
        if not !status_communicators_array.(rankk) then begin
            !communicators_array.(rankk) <- 
                Mpi.intercomm_create !communicators_array.(!mr) 0 
                Mpi.comm_world rankk 0;
                !status_communicators_array.(rankk) <- true;
            end else ()
    with
    |Mpi.Error s ->
            let message = ("There@ was@ an@ error@ in@ MPI@ when@ trying@ to@ \
            create@ an@ intercommunicator@ between@ the@ process@ with@ rank@ " ^ 
            string_of_int rankk ^ "@ and@ me.@ The@ MPI@ message@ was:@ " ^ s) in
            Status.user_message Status.Error message;
            raise (Mpi.Error s)

let fill_communicators_array () =
    let my_role = !roles_array.(!mr) 
    and my_master = 0
    and size = Mpi.comm_size Mpi.comm_world  
    and my_controller = 
        match !bosses_array.(!mr) with
        | Rank a -> a
        | Nil -> -3
    in
    match my_role with
    | Slave ->
            let msg = "I'm@ a@ slave!" in
            if debug then begin 
                print_endline msg;
                flush stdout;
                Status.user_message Status.Information msg;
            end;
            make_intercommunicator my_master;
            if (my_controller <> my_master) then begin
                assert (my_controller <> !mr);
                make_intercommunicator my_controller;
            end else begin
                for i = !mr + 1 to (size - 1) do
                    assert (i <> !mr);
                    if (!bosses_array.(i) = Rank !mr) then 
                        make_intercommunicator i;
                done;       
            end
    | Master ->
            let msg = "I'm@ the@ master!" in
            if debug then begin
                print_endline msg;
                flush stdout;
                Status.user_message Status.Information msg;
            end;
            for i = 1 to !nbr_controller do
                assert (i <> !mr);
                make_intercommunicator i;
            done;
            for i = (!nbr_controller + 1) to (size - 1) do
                assert (i <> !mr);
                make_intercommunicator i;
            done
    | Serial -> failwith "I shouldn't fill a communicator array!"

(* use the mod of mr and nbr_controller to search the controller for each 
* slave process. if mr=0 then function returns Nil, cause i am the
* master if i am a controller then the function returns 0, which means that my
* controller is the master.  *)
let who_is_my_controller mrank nbr_controller =
    if (mrank = 0) then Nil
    else if (mrank <= nbr_controller) then Rank 0
    else Rank (if nbr_controller  <> 0 then mrank mod nbr_controller else 0)

let set_workers lst = 
    list_of_slaves := lst

let set_master mst = 
    !bosses_array.(!mr) <- mst

let my_master () = !bosses_array.(!mr)

(*intialization of arrays*)
let initialize_processes_arrays nbr_controller =
    let size = !number_of_proc in
    for i = 1 to size - 1 do
         list_of_slaves := !list_of_slaves @ [Rank i];
    done;
    communicators_array := Array.make size Mpi.comm_world;
    begin
    try
        !communicators_array.(!mr) <- Mpi.comm_split Mpi.comm_world !mr 0;
    with
    | e -> assert false;
    end;
    status_communicators_array := Array.make size false;
    initialized_array := Array.make size false;
    !initialized_array.(0) <- true;
    roles_array := Array.make size Slave;
    bosses_array := Array.make size Nil ;
    let roles index = if (index = 0) then Master else Slave in
    (* And finally run the function on every element in the [bosses_array]
     * and setup the initial set of roles assigned to each process. *)
    for i = 0 to size - 1 do
        !roles_array.(i) <- (roles i);
        try
        !bosses_array.(i) <- who_is_my_controller i nbr_controller;
        with
        | e -> assert false;
    done;
    ()


let any_tag = Mpi.any_tag ;;

let recv msg_tag src_rank=
    let int_rank = int_of_rank src_rank in
    try
        Mpi.receive 0 msg_tag !communicators_array.(int_rank)
    with
    |Mpi.Error s ->
            let error_message = ("Receiving@ message@ comming@ from@ the@ " ^
            "process@ " ^ string_of_int (int_rank) ^ 
            "wasn't@ successful@ due@ to@ " ^ 
            "an@ MPI@ error.@ The@ MPI@ message@ was:@ " ^ s ^ 
            "@ ...@ sorry@ ...@ I@ will@ mark@ src_rank@ as" ^ 
            "@ a@ dead@ process.") in
            Status.user_message Status.Error error_message;
            raise (Mpi.Error s);;

let rec prob msg_tag src_rank =
    begin match src_rank with
    | Nil ->
            let i  =  ref 0 
            and ret_val =  ref (-5)
            and tag_val = ref (any_tag)
            and rank_val = ref Nil
            and looping = ref true
            and length = Array.length !communicators_array in
            while !looping && (!i <= length - 1) do
                match prob msg_tag (rank_of_int !i) with
                | None -> incr i;
                | Some (tag, rank) ->
                        rank_val := rank;
                        ret_val := !i;
                        tag_val := tag;
                        looping := false;
            done;
            
            if (!ret_val < 0) then 
                None
            else
                Some (!tag_val, !rank_val)

    | Rank int_rank ->
            try
                let did_come, _, tag = Mpi.iprobe 0 msg_tag
                !communicators_array.(int_rank) in
                if did_come then Some (tag, src_rank)
                else None
            with
            | (Mpi.Error s) as err ->
                let error_message = ("Something@ went@ wrong@ with@ MPI@ while"
                ^ "@ trying@ to@ probe@ for@ a@ message@ with@ source@ " ^ 
                string_of_int int_rank ^ ".@ The@ MPI@ error@ message@ was:@ " ^
                s ^ "@ ... sorry ...@ I@ will@ mark@ the@ process@ as@ dead.") 
                in 
                Status.user_message Status.Error error_message;
                raise err
    end;;
                
let send msg_tag msg rank = 
    if (msg_tag <> Methods.do_job) && (msg_tag <> Methods.process_management) 
        && (msg_tag <> Methods.io) && (msg_tag <> Methods.debugging) then
        failwith "Attempt to send illegal message"
    else begin
        let int_rank = int_of_rank rank in 
        if debug then begin
            let msg = ("Send@ msg@ " ^ string_of_int
                msg_tag ^ "@ to@ " ^ string_of_int int_rank)
            in
            Status.user_message Status.Information msg;
        end;
        try
            Mpi.send msg 0 msg_tag !communicators_array.(int_rank);
    (*
            if Mpi.isend msg 0 msg_tag !communicators_array.(int_rank) then () 
            else begin
                let msg =  ("Attempting to send a message with tag " ^
                string_of_int msg_tag ^ " to process " ^ string_of_int (int_of_rank
                rank)) in
                Status.user_message Status.Error msg;
                flush stdout;
                raise Cannot_send_message
            end
    *)
        with
        | Mpi.Error s ->
                let error_message =  ("Sending@ message@ " ^
                string_of_int msg_tag ^ "@ to@ the@ rank@ " ^ 
                string_of_int int_rank ^ "wasn't@ successful@ due@ to@ an@ " ^ 
                "MPI@ error.@ MPI@ message@ was:@ "
                ^ s ^ "@ ...@ sorry.") in 
                Status.user_message Status.Error error_message;
                raise (Mpi.Error s)
    end

let dead rank = 
    number_of_proc := !number_of_proc - 1;;

let number_of_processes () = 
    !number_of_proc;;

let any_source =
   Nil ;;
    
(*first things first*)
let init () =
    mr := Mpi.comm_rank Mpi.comm_world;
    Status.rank !mr;
    (*in mpitol there is a -1 for the number of processors, but why?*)
    number_of_proc := (Mpi.comm_size Mpi.comm_world);
    if !number_of_proc > 1 then begin
        let limit_for_slaves =  32. 
        and slack_for_limit =  1. 
        (*assume only one level of controllers for the sake of simplicity.
        * This formula will not work if there are more than a level of controllers,
        * where nbr_controller>limit_for_slaves+slack_for_limit*)
        and fl_nbr_proc = float_of_int !number_of_proc in 
        nbr_controller := 
            (if !number_of_proc < (int_of_float limit_for_slaves) then 0
            else 
                int_of_float (ceil (( fl_nbr_proc -. 1.) /.  
                (limit_for_slaves +. slack_for_limit +. 1. ))));
                initialize_processes_arrays 0 (* !nbr_controller *); 
                fill_communicators_array () ;
                !roles_array.(!mr)
    end else Serial


let initialize_communications = function
    | Nil -> make_intercommunicator 0
    | Rank x -> make_intercommunicator x
