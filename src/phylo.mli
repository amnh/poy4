module Nodes :
  sig
    type n = AllDirNode.node_data
    type other_n = Node.Standard.n
    val fix_preliminary : n -> n
    val distance :
      ?para:int option -> ?parb:int option -> float -> n -> n -> float
    val median : int option -> int option -> n option -> n -> n -> n
    val median_3 : int option -> n -> n -> n -> n -> n
    val to_string : n -> string
    val total_cost : int option -> n -> float
    val node_cost : int option -> n -> float
    val update_leaf : n -> n
    val taxon_code : n -> int
    val union_distance : n -> n -> float
    val is_collapsable : [ `Any | `Dynamic | `Static ] -> n -> n -> bool
    val to_xml : Data.d -> out_channel -> n -> unit
    val num_height : int option -> n -> int
    val num_otus : int option -> n -> int
    val get_sequences :
      int option ->
      n ->
      (int * Sequence.s * Cost_matrix.Two_D.m * Cost_matrix.Three_D.m *
       Alphabet.a)
      list
    val get_dynamic_preliminary : int option -> n -> DynamicCS.t list
    val get_dynamic_adjusted : int option -> n -> DynamicCS.t list
    val edge_distance : n -> n -> float
    val support_chars : int -> int option -> n -> (float * int) list
    val load_data :
      ?silent:bool -> ?classify:bool -> Data.d -> Data.d * n list
    val n_chars : ?acc:int -> n -> int
    val prioritize : n -> n
    val reprioritize : n -> n -> n
    val f_codes : int list -> n -> n
    val min_child_code : int option -> n -> int
    type e = AllDirNode.exclude
    type nad8 = Node.Standard.nad8
    val new_characters :
      int ->
      nad8 All_sets.IntegerMap.t ->
      (All_sets.IntegerMap.key * int array All_sets.IntegerMap.t list) list ->
      nad8 All_sets.IntegerMap.t
    val build_node :
      nad8 All_sets.IntegerMap.t -> All_sets.Integers.elt list -> n -> n
    val set_exclude_info : e -> n -> n
    val excludes_median : int option -> n -> n -> e
    val has_excluded : e -> bool
    module T : sig val add_exclude : All_sets.Integers.t -> n -> n end
    module Union :
      sig
        type u = AllDirNode.AllDirF.Union.u
        val union : int option -> n -> u -> u -> u
        val union_preliminary : int option -> u -> n -> u
        val union_final : int option -> u -> n -> u
        val leaf : int option -> int option -> n -> u
        val distance : u -> u -> float
        val saturation : u -> float
        val distance_node : int option -> n -> u -> float
        val compare : u -> u -> int
        val get_sequence : int option -> int -> u -> SeqCS.union_element
      end
    val compare : n -> n -> int
    val for_support : int -> (int * n) list -> int list -> int list -> n list
    val root_cost : n -> float
    val to_single : n option -> int option -> n -> int option -> n -> n
    val character_costs :
      int option -> n -> ([ `Add | `NonAdd | `Sank ] * int * float) list
    val get_nonadd_8 : int option -> n -> (NonaddCS8.t * NonaddCS8.t) list
    val get_nonadd_16 : int option -> n -> (NonaddCS16.t * NonaddCS16.t) list
    val get_nonadd_32 : int option -> n -> (NonaddCS32.t * NonaddCS32.t) list
    val get_add : int option -> n -> (AddCS.t * AddCS.t) list
    val get_sank : int option -> n -> (SankCS.t * SankCS.t) list
    val get_dynamic : int option -> n -> (DynamicCS.t * DynamicCS.t) list
    val recode : (int -> int) -> n -> n
    val to_other : n -> other_n
    val force : n -> n
  end
module Edges :
  sig
    type e = AllDirNode.OneDirF.n
    type n = AllDirNode.AllDirF.n
    val has_information : bool
    val to_node : int -> int * int -> e -> n
    val of_node : int option -> n -> e
    val recode : (int -> int) -> e -> e
    val force : e -> e
  end
module TreeOps :
  sig
    type a = AllDirNode.AllDirF.n
    type b = AllDirNode.OneDirF.n
    val break_fn : (a, b) Ptree.break_fn
    val join_fn : (a, b) Ptree.join_fn
    val cost_fn : (a, b) Ptree.cost_fn
    val reroot_fn : (a, b) Ptree.reroot_fn
    val string_of_node : a -> string
    val features :
      Methods.local_optimum ->
      (string * string) list -> (string * string) list
    val clear_internals : (a, b) Ptree.p_tree -> (a, b) Ptree.p_tree
    val downpass : (a, b) Ptree.p_tree -> (a, b) Ptree.p_tree
    val uppass : (a, b) Ptree.p_tree -> (a, b) Ptree.p_tree
    val incremental_uppass :
      (a, b) Ptree.p_tree -> Ptree.incremental list -> (a, b) Ptree.p_tree
    val to_formatter :
      Xml.attributes -> Data.d -> (a, b) Ptree.p_tree -> Xml.xml
    val root_costs : (a, b) Ptree.p_tree -> (Tree.edge * float) list
    val unadjust : (a, b) Ptree.p_tree -> (a, b) Ptree.p_tree
  end
module CharOps :
  sig
    type cs = CharacterScripting.Standard.cs
    type n = AllDirNode.AllDirF.n
    type character_input_output =
        [ `Characters of cs Sexpr.t | `Floats of float Sexpr.t ]
    val distance : float -> cs -> cs -> float
    val median : cs -> cs -> cs
    val scriptchar_operations :
      n list ->
      Data.d ->
      [ `Distance of
          (CharacterScripting.characters * CharacterScripting.characters) *
          CharacterScripting.characters
      | `Median of
          (CharacterScripting.characters * CharacterScripting.characters) *
          CharacterScripting.characters ] ->
      character_input_output
    val filter_char_operations :
      n list ->
      Data.d ->
      CharacterScripting.taxa * CharacterScripting.taxa ->
      CharacterScripting.characters -> (cs Sexpr.t * cs Sexpr.t) list
    val extract_character : int -> n -> cs
    val character_operations :
      [ `Distance of cs Sexpr.t * cs Sexpr.t
      | `Median of cs Sexpr.t * cs Sexpr.t ] ->
      [ `Characters of cs Sexpr.t | `Floats of float Sexpr.t ]
  end
module M :
  sig
    type a = Nodes.n
    type b = Edges.e
    type c = CharOps.cs
    type tree = (a, b) Ptree.p_tree
    module Kml : sig
        type phylogeny = tree

        module GIS : sig 

            type point = { 
                latitude : float;
                longitude : float;
                altitude : float;
            }

            type triangle = (point * point * point)

            (* Estimate the horizontal distance between points, in meters *)
            val horizontal_distance : point -> point -> float
            
            (* Calculate a point located in the center between two points *)
            val center_points : point -> point -> point

            (* Find the centroid of a triangle *)
            val center_triangle : triangle -> point

        end

        module TemporalGIS : sig
            type date = (int * int * int) option (* Year month day *)

            type sample = {
                coordinates : GIS.point;
                date : date;
            }

            (* Compare a pair of dates and return the minimum *)
            val min_date : date -> date -> date

            (* Produce a hash table of terminals and their corresponding samples, as
            * read from a csv file *)
            val csv : string -> (Xml.unstructured, sample) Hashtbl.t
        end

        module KTree : sig
            (* A module to easily process trees for KML generation *)

            (* POY uses internally a relatively complex data structure to hold the
            * trees, this is a sipmlified version that has all the information in XML
            * like format. Only binary trees are alowed, and each is a tuple, consisting
            * of the contents of the vertex in the phylogenetic tree, and the temporal
            * and geographic information associated with it. The leaves are exactly the
            * input data, while the interior vertices are computed by POY or user
            * provided plugins. *)
            type simplified_topology = (Xml.xml * TemporalGIS.sample) Parser.Tree.t

            (* The represenatation of the name of a node. We don't use plain strings
            * because they would make the generation of the XML a little bit too verbose
            * *)
            type node_name = Xml.unstructured 

            (* The topology of a tree, with the simplified version of the topology, and
            * quick access to the nodes of the tree, and the ancestors. This suplies
            * some functions that the simplified_topology can nos perform (like finding
            * the ancestor of a tree or quickly reaching a particular vertex of the
            * tree). *)
            type topology =
                { ancestors : (Xml.unstructured, Xml.xml Xml.contents option) Hashtbl.t;
                nodes : (Xml.unstructured, Xml.xml) Hashtbl.t;
                topo : simplified_topology }


            (* The default tree adjustment function *)
            val adjust_tree : simplified_topology -> simplified_topology

            (* [process data csv phylogeny] producess a topology consisting of the
            * contents computed in the [phylogeny] tree, with temporal and geographic
            * information contained in the CSV file [csv], and all the data
            * representation [data]. *)
            val process : Data.d -> string -> phylogeny -> topology

            (* [ancestor topology vertex] gets the ancestor of the [vertex] in the
            * [topology]. The output is optional as the root of the tree has no
            * ancestor. *)
            val ancestor : topology -> node_name -> Xml.xml Xml.contents option

            (* [children topology vertex] gets the pair of the vertex [vertex] in the
            * [topology]. The output is optional as the leaves of the tree have no
            * children. *)
            val children : topology -> node_name -> (node_name * node_name) option

            (* [sister topology vertex] gets the sister group of the [vertex] in the
            * [topology] (that is, the other child of the ancestor of [vertex]). 
            * The output is optional as the root has no sister. *)
            val sister : topology -> node_name ->  node_name option

            (** [node topology vertex] extracts all the data about [vertex] contained 
            * in the original phylogeny as stored in [topology]. *)
            val node : topology -> node_name -> Xml.xml


            (** [is_root topology vertex] is true iff [vertex] is the root of the
            * [topology] *)
            val is_root : topology -> node_name -> bool

            (** [extract_gis simplified_topology] extracts the temporal and gis
            * information stored in the root vertex of the [simplified_topology]. *)
            val extract_gis : simplified_topology -> TemporalGIS.sample

        end

        module KFile : sig
            (** The following types are needed to produce a Plugin for POY. *)

            (** [node_information data topology vertex] produces an HTML-equivalent
            * structure with the information that should be printed about the [vertex] in
            * the [topology]. [data] is provided in case the specification of some of
            * the characters in [vertex] or the [vertex] itself is needed. *)
            type node_information = 
                Data.d -> KTree.topology -> Xml.unstructured -> 
                    [ Xml.unstructured | Xml.xml Xml.structured ]

            (** [create_node node_information data topology vertex parent_sample
            *   child1_sample child2_sample vertex_sample] produces the XML structure
            *   with the representation of [vertex] in the KML file. The information
            *   about the vertex should be generated and enclosed in a CDATA using the
            *   [node_information] function provided in the argument. The
            *   [parent_sample], [child1_sample], and [child2_sample] are provided for
            *   convenience, as the main goal of this function is to print the node and
            *   edges connected with it. *)
            type create_node =
                    node_information -> Data.d -> KTree.topology ->
                    Xml.xml -> TemporalGIS.sample option -> 
                        TemporalGIS.sample option ->
                        TemporalGIS.sample option ->TemporalGIS.sample -> 
                            Xml.xml Sexpr.t


            (** [adjust_tree simple_topology] beautifies the location of the vertices in
             * the tree *)
            type adjust_tree = KTree.simplified_topology -> KTree.simplified_topology

            (** [styles ()] produces all the styles used in the KML. *)
            type styles = unit -> Xml.xml Sexpr.t

            type folder = {
                name : string;
                node_information : node_information;
                create_node : create_node option;
            }

            type plugin = {
                folders : folder list;
                adjust_tree : adjust_tree;
                styles : styles;
            }

            (** [default ] is the default plugin *)
            val default : plugin

            (** [register_plugin name plugin] registers the [plugin] under the [name]
             * provided. This plugin will be usable in the user interface using the
             * command report (kml:name). *)
            val register_plugin : string -> plugin -> unit

            (** [has_plugin name] is [true] iff [register_plugin name plugin] has been
            * called before *)
            val has_plugin : string -> bool

            (** [kml ?plugin name output data csv tree] dumps in the file [output] a
            * KML file using the [plugin] selected for the tree [tree] and using the
            * [csv] file with the geographic and temporal information. The KML will be
            * registerd with the [name] provided. If not [plugin] is given, then
            * [default] is selected .*)
            val kml : ?plugin:string -> string -> string -> Data.d -> string ->
                phylogeny Sexpr.t -> unit


            val create_line : TemporalGIS.sample option -> TemporalGIS.sample -> 
                Xml.xml Xml.contents
        end
    end
    type r = (a, b, c) Scripting.run
    type minimum_spanning_tree = tree
    type build = minimum_spanning_tree list
    type minimum_spanning_family = minimum_spanning_tree list
    type build_optimum = tree list
    type script = Methods.script
    val empty : unit -> r
    val args : string array
    val run :
      ?folder:(r -> script -> r) ->
      ?output_file:string -> ?start:r -> script list -> r
    val update_mergingscript :
      (r -> script -> r) -> script list -> r -> r -> r
    val process_input : r -> Methods.input -> r
    val get_dump : ?file:string -> unit -> r * script list
    val restart : ?file:string -> unit -> r
    val process_random_seed_set : r -> int -> r
    val console_run : string -> unit
    val parsed_run : script list -> unit
    val channel_run : in_channel -> unit
    val get_console_run : unit -> r
    val update_trees_to_data : ?classify:bool -> bool -> bool -> r -> r
    val set_console_run : r -> unit
    module PhyloTree :
      sig
        type phylogeny = (a, b) Ptree.p_tree
        val get_cost : phylogeny -> float
        val fold_edges :
          ('a -> Tree.edge -> 'a) -> 'a -> (a, b) Ptree.p_tree -> 'a
        val fold_nodes :
          ('a -> Tree.node -> 'a) -> 'a -> (a, b) Ptree.p_tree -> 'a
        val fold_vertices :
          ('a -> int -> 'a) -> 'a -> (a, b) Ptree.p_tree -> 'a
        val add_node_data : int -> a -> phylogeny -> phylogeny
        val get_node_data : int -> phylogeny -> a
        val add_edge_data : Tree.edge -> b -> phylogeny -> phylogeny
        val get_edge_data : Tree.edge -> phylogeny -> b
        val get_parent : int -> phylogeny -> int
        val get_neighs : int -> phylogeny -> int list
        val join :
          Tree.join_jxn ->
          Tree.join_jxn -> phylogeny -> phylogeny * Tree.join_delta
        val break :
          Tree.break_jxn -> phylogeny -> phylogeny * Tree.break_delta
        val reroot : Tree.edge -> phylogeny -> phylogeny
        val downpass : phylogeny -> phylogeny
        val uppass : phylogeny -> phylogeny
        val of_string : string -> Data.d -> a list -> phylogeny list
        val to_string : bool -> phylogeny -> Data.d -> string list
        val of_file : string -> Data.d -> a list -> phylogeny list
        val of_nodes : Data.d -> a list -> phylogeny
        val build : Data.d -> a list -> phylogeny list
        val spr :
          ((phylogeny * float) list -> unit) ->
          Data.d -> phylogeny -> phylogeny list
        val tbr :
          ((phylogeny * float) list -> unit) ->
          Data.d -> phylogeny -> phylogeny list
      end
    module Runtime :
      sig
        type phylogeny = (a, b) Ptree.p_tree
        val min_cost : unit -> float option
        val max_cost : unit -> float option
        val all_costs : unit -> float list
        val trees : unit -> phylogeny list
        val set_trees : phylogeny list -> unit
        val data : unit -> Data.d
        val to_string : bool -> string list list
        val of_string : string -> unit
      end
    module Node :
      sig
        type n = a
        type other_n =
            Scripting.Make(Nodes)(Edges)(TreeOps)(CharOps).Node.other_n
        val fix_preliminary : n -> n
        val distance :
          ?para:int option -> ?parb:int option -> float -> n -> n -> float
        val median : int option -> int option -> n option -> n -> n -> n
        val median_3 : int option -> n -> n -> n -> n -> n
        val to_string : n -> string
        val total_cost : int option -> n -> float
        val node_cost : int option -> n -> float
        val update_leaf : n -> n
        val taxon_code : n -> int
        val union_distance : n -> n -> float
        val is_collapsable : [ `Any | `Dynamic | `Static ] -> n -> n -> bool
        val to_xml : Data.d -> out_channel -> n -> unit
        val num_height : int option -> n -> int
        val num_otus : int option -> n -> int
        val get_sequences :
          int option ->
          n ->
          (int * Sequence.s * Cost_matrix.Two_D.m * Cost_matrix.Three_D.m *
           Alphabet.a)
          list
        val get_dynamic_preliminary : int option -> n -> DynamicCS.t list
        val get_dynamic_adjusted : int option -> n -> DynamicCS.t list
        val edge_distance : n -> n -> float
        val support_chars : int -> int option -> n -> (float * int) list
        val load_data :
          ?silent:bool -> ?classify:bool -> Data.d -> Data.d * n list
        val n_chars : ?acc:int -> n -> int
        val prioritize : n -> n
        val reprioritize : n -> n -> n
        val f_codes : int list -> n -> n
        val min_child_code : int option -> n -> int
        type e = Scripting.Make(Nodes)(Edges)(TreeOps)(CharOps).Node.e
        type nad8 = Scripting.Make(Nodes)(Edges)(TreeOps)(CharOps).Node.nad8
        val new_characters :
          int ->
          nad8 All_sets.IntegerMap.t ->
          (All_sets.IntegerMap.key * int array All_sets.IntegerMap.t list)
          list -> nad8 All_sets.IntegerMap.t
        val build_node :
          nad8 All_sets.IntegerMap.t -> All_sets.Integers.elt list -> n -> n
        val set_exclude_info : e -> n -> n
        val excludes_median : int option -> n -> n -> e
        val has_excluded : e -> bool
        module T : sig val add_exclude : All_sets.Integers.t -> n -> n end
        module Union :
          sig
            type u =
                Scripting.Make(Nodes)(Edges)(TreeOps)(CharOps).Node.Union.u
            val union : int option -> n -> u -> u -> u
            val union_preliminary : int option -> u -> n -> u
            val union_final : int option -> u -> n -> u
            val leaf : int option -> int option -> n -> u
            val distance : u -> u -> float
            val saturation : u -> float
            val distance_node : int option -> n -> u -> float
            val compare : u -> u -> int
            val get_sequence : int option -> int -> u -> SeqCS.union_element
          end
        val compare : n -> n -> int
        val for_support :
          int -> (int * n) list -> int list -> int list -> n list
        val root_cost : n -> float
        val to_single : n option -> int option -> n -> int option -> n -> n
        val character_costs :
          int option -> n -> ([ `Add | `NonAdd | `Sank ] * int * float) list
        val get_nonadd_8 :
          int option -> n -> (NonaddCS8.t * NonaddCS8.t) list
        val get_nonadd_16 :
          int option -> n -> (NonaddCS16.t * NonaddCS16.t) list
        val get_nonadd_32 :
          int option -> n -> (NonaddCS32.t * NonaddCS32.t) list
        val get_add : int option -> n -> (AddCS.t * AddCS.t) list
        val get_sank : int option -> n -> (SankCS.t * SankCS.t) list
        val get_dynamic : int option -> n -> (DynamicCS.t * DynamicCS.t) list
        val recode : (int -> int) -> n -> n
        val to_other : n -> other_n
        val force : n -> n
      end
  end
type a = Nodes.n
type b = Edges.e
type c = CharOps.cs
type tree = (a, b) Ptree.p_tree
module Kml :
  sig
    type phylogeny = tree
    module GIS :
      sig
        type point =
          Scripting.Make(Nodes)(Edges)(TreeOps)(CharOps).Kml.GIS.point = {
          latitude : float;
          longitude : float;
          altitude : float;
        }
        type triangle = point * point * point
        val horizontal_distance : point -> point -> float
        val center_points : point -> point -> point
        val center_triangle : triangle -> point
      end
    module TemporalGIS :
      sig
        type date = (int * int * int) option
        type sample =
          Scripting.Make(Nodes)(Edges)(TreeOps)(CharOps).Kml.TemporalGIS.sample = {
          coordinates : GIS.point;
          date : date;
        }
        val min_date : date -> date -> date
        val csv : string -> (Xml.unstructured, sample) Hashtbl.t
      end
    module KTree :
      sig
        type simplified_topology =
            (Xml.xml * TemporalGIS.sample) Parser.Tree.t
        type node_name = Xml.unstructured
        type topology =
          Scripting.Make(Nodes)(Edges)(TreeOps)(CharOps).Kml.KTree.topology = {
          ancestors :
            (Xml.unstructured, Xml.xml Xml.contents option) Hashtbl.t;
          nodes : (Xml.unstructured, Xml.xml) Hashtbl.t;
          topo : simplified_topology;
        }
        val adjust_tree : simplified_topology -> simplified_topology
        val process : Data.d -> string -> phylogeny -> topology
        val ancestor :
          topology -> node_name -> Xml.xml Xml.contents option
        val children :
          topology -> node_name -> (node_name * node_name) option
        val sister : topology -> node_name -> node_name option
        val node : topology -> node_name -> Xml.xml
        val is_root : topology -> node_name -> bool
        val extract_gis : simplified_topology -> TemporalGIS.sample
      end
    module KFile :
      sig
        type node_information =
            Data.d ->
            KTree.topology ->
            Xml.unstructured ->
            [ `Bool of bool
            | `Delayed of unit -> Xml.xml Sexpr.t
            | `Empty
            | `Float of float
            | `FloatFloatTuple of float * float
            | `Fun of unit -> string
            | `Int of int
            | `IntFloatTuple of int * float
            | `IntTuple of int * int
            | `Set of Xml.xml Sexpr.t list
            | `Single of Xml.xml
            | `String of string ]
        type create_node =
            node_information ->
            Data.d ->
            KTree.topology ->
            Xml.xml ->
            TemporalGIS.sample option ->
            TemporalGIS.sample option ->
            TemporalGIS.sample option ->
            TemporalGIS.sample -> Xml.xml Sexpr.t
        type adjust_tree =
            KTree.simplified_topology -> KTree.simplified_topology
        type styles = unit -> Xml.xml Sexpr.t
        type folder =
          Scripting.Make(Nodes)(Edges)(TreeOps)(CharOps).Kml.KFile.folder = {
          name : string;
          node_information : node_information;
          create_node : create_node option;
        }
        type plugin =
          Scripting.Make(Nodes)(Edges)(TreeOps)(CharOps).Kml.KFile.plugin = {
          folders : folder list;
          adjust_tree : adjust_tree;
          styles : styles;
        }
        val default : plugin
        val register_plugin : string -> plugin -> unit
        val has_plugin : string -> bool
        val kml :
          ?plugin:string ->
          string -> string -> Data.d -> string -> phylogeny Sexpr.t -> unit
        val create_line :
          TemporalGIS.sample option ->
          TemporalGIS.sample -> Xml.xml Xml.contents
      end
  end
type r = (a, b, c) Scripting.run
type minimum_spanning_tree = tree
type build = minimum_spanning_tree list
type minimum_spanning_family = minimum_spanning_tree list
type build_optimum = tree list
type script = Methods.script
val empty : unit -> r
val args : string array
val run :
  ?folder:(r -> script -> r) ->
  ?output_file:string -> ?start:r -> script list -> r
val update_mergingscript : (r -> script -> r) -> script list -> r -> r -> r
val process_input : r -> Methods.input -> r
val get_dump : ?file:string -> unit -> r * script list
val restart : ?file:string -> unit -> r
val process_random_seed_set : r -> int -> r
val console_run : string -> unit
val parsed_run : script list -> unit
val channel_run : in_channel -> unit
val get_console_run : unit -> r
val update_trees_to_data : ?classify:bool -> bool -> bool -> r -> r
val set_console_run : r -> unit
module PhyloTree :
  sig
    type phylogeny = (a, b) Ptree.p_tree
    val get_cost : phylogeny -> float
    val fold_edges :
      ('a -> Tree.edge -> 'a) -> 'a -> (a, b) Ptree.p_tree -> 'a
    val fold_nodes :
      ('a -> Tree.node -> 'a) -> 'a -> (a, b) Ptree.p_tree -> 'a
    val fold_vertices : ('a -> int -> 'a) -> 'a -> (a, b) Ptree.p_tree -> 'a
    val add_node_data : int -> a -> phylogeny -> phylogeny
    val get_node_data : int -> phylogeny -> a
    val add_edge_data : Tree.edge -> b -> phylogeny -> phylogeny
    val get_edge_data : Tree.edge -> phylogeny -> b
    val get_parent : int -> phylogeny -> int
    val get_neighs : int -> phylogeny -> int list
    val join :
      Tree.join_jxn ->
      Tree.join_jxn -> phylogeny -> phylogeny * Tree.join_delta
    val break : Tree.break_jxn -> phylogeny -> phylogeny * Tree.break_delta
    val reroot : Tree.edge -> phylogeny -> phylogeny
    val downpass : phylogeny -> phylogeny
    val uppass : phylogeny -> phylogeny
    val of_string : string -> Data.d -> a list -> phylogeny list
    val to_string : bool -> phylogeny -> Data.d -> string list
    val of_file : string -> Data.d -> a list -> phylogeny list
    val of_nodes : Data.d -> a list -> phylogeny
    val build : Data.d -> a list -> phylogeny list
    val spr :
      ((phylogeny * float) list -> unit) ->
      Data.d -> phylogeny -> phylogeny list
    val tbr :
      ((phylogeny * float) list -> unit) ->
      Data.d -> phylogeny -> phylogeny list
  end
module Runtime :
  sig
    type phylogeny = (a, b) Ptree.p_tree
    val min_cost : unit -> float option
    val max_cost : unit -> float option
    val all_costs : unit -> float list
    val trees : unit -> phylogeny list
    val set_trees : phylogeny list -> unit
    val data : unit -> Data.d
    val to_string : bool -> string list list
    val of_string : string -> unit
  end
module Node :
  sig
    type n = a
    type other_n =
        Scripting.Make(Nodes)(Edges)(TreeOps)(CharOps).Node.other_n
    val fix_preliminary : n -> n
    val distance :
      ?para:int option -> ?parb:int option -> float -> n -> n -> float
    val median : int option -> int option -> n option -> n -> n -> n
    val median_3 : int option -> n -> n -> n -> n -> n
    val to_string : n -> string
    val total_cost : int option -> n -> float
    val node_cost : int option -> n -> float
    val update_leaf : n -> n
    val taxon_code : n -> int
    val union_distance : n -> n -> float
    val is_collapsable : [ `Any | `Dynamic | `Static ] -> n -> n -> bool
    val to_xml : Data.d -> out_channel -> n -> unit
    val num_height : int option -> n -> int
    val num_otus : int option -> n -> int
    val get_sequences :
      int option ->
      n ->
      (int * Sequence.s * Cost_matrix.Two_D.m * Cost_matrix.Three_D.m *
       Alphabet.a)
      list
    val get_dynamic_preliminary : int option -> n -> DynamicCS.t list
    val get_dynamic_adjusted : int option -> n -> DynamicCS.t list
    val edge_distance : n -> n -> float
    val support_chars : int -> int option -> n -> (float * int) list
    val load_data :
      ?silent:bool -> ?classify:bool -> Data.d -> Data.d * n list
    val n_chars : ?acc:int -> n -> int
    val prioritize : n -> n
    val reprioritize : n -> n -> n
    val f_codes : int list -> n -> n
    val min_child_code : int option -> n -> int
    type e = Scripting.Make(Nodes)(Edges)(TreeOps)(CharOps).Node.e
    type nad8 = Scripting.Make(Nodes)(Edges)(TreeOps)(CharOps).Node.nad8
    val new_characters :
      int ->
      nad8 All_sets.IntegerMap.t ->
      (All_sets.IntegerMap.key * int array All_sets.IntegerMap.t list) list ->
      nad8 All_sets.IntegerMap.t
    val build_node :
      nad8 All_sets.IntegerMap.t -> All_sets.Integers.elt list -> n -> n
    val set_exclude_info : e -> n -> n
    val excludes_median : int option -> n -> n -> e
    val has_excluded : e -> bool
    module T : sig val add_exclude : All_sets.Integers.t -> n -> n end
    module Union :
      sig
        type u = Scripting.Make(Nodes)(Edges)(TreeOps)(CharOps).Node.Union.u
        val union : int option -> n -> u -> u -> u
        val union_preliminary : int option -> u -> n -> u
        val union_final : int option -> u -> n -> u
        val leaf : int option -> int option -> n -> u
        val distance : u -> u -> float
        val saturation : u -> float
        val distance_node : int option -> n -> u -> float
        val compare : u -> u -> int
        val get_sequence : int option -> int -> u -> SeqCS.union_element
      end
    val compare : n -> n -> int
    val for_support : int -> (int * n) list -> int list -> int list -> n list
    val root_cost : n -> float
    val to_single : n option -> int option -> n -> int option -> n -> n
    val character_costs :
      int option -> n -> ([ `Add | `NonAdd | `Sank ] * int * float) list
    val get_nonadd_8 : int option -> n -> (NonaddCS8.t * NonaddCS8.t) list
    val get_nonadd_16 : int option -> n -> (NonaddCS16.t * NonaddCS16.t) list
    val get_nonadd_32 : int option -> n -> (NonaddCS32.t * NonaddCS32.t) list
    val get_add : int option -> n -> (AddCS.t * AddCS.t) list
    val get_sank : int option -> n -> (SankCS.t * SankCS.t) list
    val get_dynamic : int option -> n -> (DynamicCS.t * DynamicCS.t) list
    val recode : (int -> int) -> n -> n
    val to_other : n -> other_n
    val force : n -> n
  end
val seed : int
