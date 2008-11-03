type compress = Single of int | Pair of (int * int)
type protocol = [ `Compressed of int | `NoCompression ]
type table = {
  encoding_table : (compress, int) Hashtbl.t;
  decoding_table : (int, compress) Hashtbl.t;
  cnt : int ref;
  state : int option ref;
  sticky_state : int option ref;
  reached_limit : bool ref;
  did_exit : int ref;
}
val initial_table : unit -> table
val latest : [> `Compressed of int ]
val add_to_tables :
  [< `Compressed of int | `NoCompression ] -> table -> compress -> unit
val compress :
  ?initial:int -> ?length:int -> table -> string -> int list -> int list
val decode :
  [< `Compressed of int | `NoCompression > `Compressed ] ->
  table -> Buffer.t -> int -> unit
val decompress :
  [< `Compressed of int | `NoCompression > `Compressed ] ->
  table -> int list -> Buffer.t -> unit
val full_compress : string -> int list
val full_decompress :
  [< `Compressed of int | `NoCompression > `Compressed ] ->
  int list -> string
val magic_number1 : int
val magic_number2 : int
val poy_major_version : int
val poy_minor_version : int
val protocol_major_version : int
val protocol_minor_version : int
val valid_headers :
  ((int * int * int * int * int * int) * [> `Compressed of int ]) list
val valid_protocols :
  ([> `Compressed of int ] * (int * int * int * int * int * int)) list
val output_header : [> `Compressed of int ] -> out_channel -> unit
val detect_type :
  in_channel -> [> `Compressed of int | `NoCompression | `Zlib ]
val skip_header :
  in_channel -> [> `Compressed of int | `NoCompression ] -> unit
