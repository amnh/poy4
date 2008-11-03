type window
external init : unit -> window = "ncurs_CAML_init"
external finalize : unit -> unit = "ncurs_CAML_endwin"
val stdscr : window
val redraw : (unit -> unit) ref
val do_redraw : unit -> unit
val set_redraw_function : (unit -> unit) -> unit
external redrawwin : window -> unit = "ncurs_CAML_redrawwin"
external window_create : window -> int -> int -> int -> int -> window
  = "ncurs_CAML_create"
external getyx : window -> int * int = "ncurs_CAML_getyx"
external getparyx : window -> int * int = "ncurs_CAML_getparyx"
external getmaxyx : window -> int * int = "ncurs_CAML_getmaxyx"
external getbegyx : window -> int * int = "ncurs_CAML_getbegyx"
external box : window -> int -> int -> unit = "ncurs_CAML_box"
external c_wrefresh : window -> unit = "ncurs_CAML_wrefresh"
external scroll : window -> int -> unit = "ncurs_CAML_scroll"
external c_refresh : unit -> unit = "ncurs_CAML_refresh"
val refresh : ?w:window -> unit -> unit
external echo : bool -> unit = "ncurs_CAML_echo"
external raw : unit -> unit = "ncurs_CAML_raw"
external noraw : unit -> unit = "ncurs_CAML_noraw"
external cbreak : unit -> unit = "ncurs_CAML_cbreak"
external c_keypad : window -> bool -> unit = "ncurs_CAML_keypad"
external do_update : unit -> unit = "ncurs_CAML_doupdate"
external wnoutrefresh : window -> unit = "ncurs_CAML_wnoutrefresh"
val keypad : ?w:window -> bool -> unit
external meta : window -> bool -> unit = "ncurs_CAML_meta"
external addstr : string -> unit = "ncurs_CAML_addstr"
external waddstr : window -> string -> unit = "ncurs_CAML_waddstr"
external waddch : window -> int -> unit = "ncurs_CAML_waddch"
val printw : (string, unit, string) format -> string
val wprintw : window -> (string, unit, string) format -> string
external mvaddstr : int -> int -> string -> unit = "ncurs_CAML_mvaddstr"
val mvprintw : int -> int -> (string, unit, string) format -> string
external mvwaddstr : window -> int -> int -> string -> unit
  = "ncurs_CAML_mvwaddstr"
external scrollok : window -> bool -> unit = "ncurs_CAML_scrollok"
external idlok : window -> bool -> unit = "ncurs_CAML_idlok"
val mvwprintw :
  window -> int -> int -> (string, unit, string) format -> string
external getch : unit -> int = "ncurs_CAML_getch"
external wgetch : window -> int = "ncurs_CAML_wgetch"
type attribut =
    STANDOUT
  | UNDERLINE
  | REVERSE
  | BLINK
  | BOLD
  | PROTECT
  | INVIS
  | ALTCHARSET
external attr_standout : window -> bool -> unit = "ncurs_CAML_A_STANDOUT"
external attr_underline : window -> bool -> unit = "ncurs_CAML_A_UNDERLINE"
external attr_reverse : window -> bool -> unit = "ncurs_CAML_A_REVERSE"
external attr_blink : window -> bool -> unit = "ncurs_CAML_A_BLINK"
external attr_bold : window -> bool -> unit = "ncurs_CAML_A_BOLD"
external attr_protect : window -> bool -> unit = "ncurs_CAML_A_PROTECT"
external attr_invis : window -> bool -> unit = "ncurs_CAML_A_INVIS"
external attr_altcharset : window -> bool -> unit = "ncurs_CAML_A_ALTCHARSET"
val attr_single_set : attribut -> window -> bool -> unit
val attr_set_unset : bool -> ?window:window -> attribut list -> unit
val attr_set : ?window:window -> attribut list -> unit
val attr_unset : ?window:window -> attribut list -> unit
external columns : unit -> int = "ncurs_CAML_columns"
external lines : unit -> int = "ncurs_CAML_lines"
external is_term_resized : int -> int -> bool = "ncurs_CAML_istermresized"
external wresize : window -> int -> int -> unit = "ncurs_CAML_wresize"
external mvwin : window -> int -> int -> unit = "ncurs_CAML_mvwin"
external wmove : window -> int -> int -> unit = "ncurs_CAML_wmove"
external delete : window -> unit = "ncurs_CAML_delete"
external wdelch : window -> unit = "ncurs_CAML_wdelch"
val unbox : window -> unit
external nocbreak : unit -> unit = "ncurs_CAML_nocbreak"
external mvwhline : window -> int -> int -> int -> int -> unit
  = "ncurs_CAML_mvwhline"
external has_colors : unit -> bool = "ncurs_CAML_has_colors"
external start_color : unit -> unit = "ncurs_CAML_start_color"
external wdeleteln : window -> unit = "ncurs_CAML_wdeleteln"
external isprint : int -> bool = "ncurs_CAML_isprint"
external keyup : unit -> int = "ncurs_CAML_keyup"
external keydown : unit -> int = "ncurs_CAML_keydown"
external keyleft : unit -> int = "ncurs_CAML_keyleft"
external keyright : unit -> int = "ncurs_CAML_keyright"
external keybackspace : unit -> int = "ncurs_CAML_keybackspace"
external keydc : unit -> int = "ncurs_CAML_keydc"
external keynpage : unit -> int = "ncurs_CAML_keynpage"
external keyppage : unit -> int = "ncurs_CAML_keyppage"
external resize : unit -> int = "ncurs_CAML_resize"
external error : unit -> int = "ncurs_CAML_error"
val next_color_pair : int ref
type color = Red | Green | Yellow | Blue | Cyan | Magenta | White | Black
external init_pair : int -> color -> color -> unit = "ncurs_CAML_init_pair"
module CompareColorPair :
  sig type t = color * color val compare : 'a * 'b -> 'a * 'b -> int end
module ColorMap :
  sig
    type key = CompareColorPair.t
    type 'a t = 'a Map.Make(CompareColorPair).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
val color_pairs : int ColorMap.t ref
val get_color : color -> color -> int
external use_color : int -> window -> bool -> unit = "ncurs_CAML_use_color"
val red_in_black : unit -> unit
val use_red : window -> bool -> unit
val use_green : window -> bool -> unit
val use_blue : window -> bool -> unit
val use_cyan : window -> bool -> unit
val use_yellow : window -> bool -> unit
val use_magenta : window -> bool -> unit
val use_white : window -> bool -> unit
val use_red_in_whit : window -> bool -> unit
val use_green_in_whit : window -> bool -> unit
val use_blue_in_whit : window -> bool -> unit
val use_cyan_in_whit : window -> bool -> unit
val use_yellow_in_whit : window -> bool -> unit
val use_magenta_in_whit : window -> bool -> unit
val use_black_in_whit : window -> bool -> unit
external winsch : window -> int -> unit = "ncurs_CAML_winsch"
external halfdelay : int -> unit = "ncurs_CAML_halfdelay"
external sigwinch_supported : unit -> bool = "ncurs_CAML_sigwinch_supported"
external sigwinch : unit -> int = "ncurs_CAML_sigwinch"
external sigwinchhandler : int -> unit = "ncurs_CAML_sigwinch_handler"
external resize_handler : unit -> unit = "ncurs_CAML_resize_handler"
external wclear : window -> unit = "ncurs_CAML_wclear"
external touchwin : window -> unit = "ncurs_CAML_touchwin"
external clearok : window -> bool -> unit = "ncurs_CAML_clearok"
external werase : window -> unit = "ncurs_CAML_werase"
