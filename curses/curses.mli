type window
and screen
and terminal
and chtype = int
and attr_t = int
and err = bool
module Acs :
  sig
    type acs = {
      ulcorner : chtype;
      llcorner : chtype;
      urcorner : chtype;
      lrcorner : chtype;
      ltee : chtype;
      rtee : chtype;
      btee : chtype;
      ttee : chtype;
      hline : chtype;
      vline : chtype;
      plus : chtype;
      s1 : chtype;
      s9 : chtype;
      diamond : chtype;
      ckboard : chtype;
      degree : chtype;
      plminus : chtype;
      bullet : chtype;
      larrow : chtype;
      rarrow : chtype;
      darrow : chtype;
      uarrow : chtype;
      board : chtype;
      lantern : chtype;
      block : chtype;
      s3 : chtype;
      s7 : chtype;
      lequal : chtype;
      gequal : chtype;
      pi : chtype;
      nequal : chtype;
      sterling : chtype;
    } 
    val bssb : acs -> chtype
    val ssbb : acs -> chtype
    val bbss : acs -> chtype
    val sbbs : acs -> chtype
    val sbss : acs -> chtype
    val sssb : acs -> chtype
    val ssbs : acs -> chtype
    val bsss : acs -> chtype
    val bsbs : acs -> chtype
    val sbsb : acs -> chtype
    val ssss : acs -> chtype
  end
external addch : chtype -> err = "mlcurses_addch"
external waddch : window -> chtype -> err = "mlcurses_waddch"
external mvaddch : int -> int -> chtype -> err = "mlcurses_mvaddch"
external mvwaddch : window -> int -> int -> chtype -> err
  = "mlcurses_mvwaddch"
external echochar : chtype -> err = "mlcurses_echochar"
external wechochar : window -> chtype -> err = "mlcurses_wechochar"
external addchstr : chtype array -> err = "mlcurses_addchstr"
external waddchstr : window -> chtype array -> err = "mlcurses_waddchstr"
external mvaddchstr : int -> int -> chtype array -> err
  = "mlcurses_mvaddchstr"
external mvwaddchstr : window -> int -> int -> chtype array -> err
  = "mlcurses_mvwaddchstr"
external addchnstr : chtype array -> int -> int -> err = "mlcurses_addchnstr"
external waddchnstr : window -> chtype array -> int -> int -> err
  = "mlcurses_waddchnstr"
external mvaddchnstr : int -> int -> chtype array -> int -> int -> err
  = "mlcurses_mvaddchnstr"
external mvwaddchnstr :
  window -> int -> int -> chtype array -> int -> int -> err
  = "mlcurses_mvwaddchnstr_bytecode" "mlcurses_mvwaddchnstr_native"
external addstr : string -> err = "mlcurses_addstr"
external waddstr : window -> string -> err = "mlcurses_waddstr"
external mvaddstr : int -> int -> string -> err = "mlcurses_mvaddstr"
external mvwaddstr : window -> int -> int -> string -> err
  = "mlcurses_mvwaddstr"
external addnstr : string -> int -> int -> err = "mlcurses_addnstr"
external waddnstr : window -> string -> int -> int -> err
  = "mlcurses_waddnstr"
external mvaddnstr : int -> int -> string -> int -> int -> err
  = "mlcurses_mvaddnstr"
external mvwaddnstr : window -> int -> int -> string -> int -> int -> err
  = "mlcurses_mvwaddnstr_bytecode" "mlcurses_mvwaddnstr_native"
external attroff : int -> unit = "mlcurses_attroff"
external wattroff : window -> int -> unit = "mlcurses_wattroff"
external attron : int -> unit = "mlcurses_attron"
external wattron : window -> int -> unit = "mlcurses_wattron"
external attrset : int -> unit = "mlcurses_attrset"
external wattrset : window -> int -> unit = "mlcurses_wattrset"
external standend : unit -> unit = "mlcurses_standend"
external wstandend : window -> unit = "mlcurses_wstandend"
external standout : unit -> unit = "mlcurses_standout"
external wstandout : window -> unit = "mlcurses_wstandout"
external attr_off : attr_t -> unit = "mlcurses_attr_off"
external wattr_off : window -> attr_t -> unit = "mlcurses_wattr_off"
external attr_on : attr_t -> unit = "mlcurses_attr_on"
external wattr_on : window -> attr_t -> unit = "mlcurses_wattr_on"
external attr_set : attr_t -> int -> unit = "mlcurses_attr_set"
external wattr_set : window -> attr_t -> int -> unit = "mlcurses_wattr_set"
external chgat : int -> attr_t -> int -> unit = "mlcurses_chgat"
external wchgat : window -> int -> attr_t -> int -> unit = "mlcurses_wchgat"
external mvchgat : int -> int -> int -> attr_t -> int -> unit
  = "mlcurses_mvchgat"
external mvwchgat : window -> int -> int -> int -> attr_t -> int -> unit
  = "mlcurses_mvwchgat_bytecode" "mlcurses_mvwchgat_native"
external beep : unit -> err = "mlcurses_beep"
external flash : unit -> err = "mlcurses_flash"
external bkgdset : chtype -> unit = "mlcurses_bkgdset"
external wbkgdset : window -> chtype -> unit = "mlcurses_wbkgdset"
external bkgd : chtype -> unit = "mlcurses_bkgd"
external wbkgd : window -> chtype -> unit = "mlcurses_wbkgd"
external getbkgd : window -> chtype = "mlcurses_getbkgd"
external border :
  chtype ->
  chtype -> chtype -> chtype -> chtype -> chtype -> chtype -> chtype -> unit
  = "mlcurses_border_bytecode" "mlcurses_border_native"
external wborder :
  window ->
  chtype ->
  chtype -> chtype -> chtype -> chtype -> chtype -> chtype -> chtype -> unit
  = "mlcurses_wborder_bytecode" "mlcurses_wborder_native"
external box : window -> chtype -> chtype -> unit = "mlcurses_box"
external hline : chtype -> int -> unit = "mlcurses_hline"
external whline : window -> chtype -> int -> unit = "mlcurses_whline"
external vline : chtype -> int -> unit = "mlcurses_vline"
external wvline : window -> chtype -> int -> unit = "mlcurses_wvline"
external mvhline : int -> int -> chtype -> int -> unit = "mlcurses_mvhline"
external mvwhline : window -> int -> int -> chtype -> int -> unit
  = "mlcurses_mvwhline"
external mvvline : int -> int -> chtype -> int -> unit = "mlcurses_mvvline"
external mvwvline : window -> int -> int -> chtype -> int -> unit
  = "mlcurses_mvwvline"
external erase : unit -> unit = "mlcurses_erase"
external werase : window -> unit = "mlcurses_werase"
external clear : unit -> unit = "mlcurses_clear"
external wclear : window -> unit = "mlcurses_wclear"
external clrtobot : unit -> unit = "mlcurses_clrtobot"
external wclrtobot : window -> unit = "mlcurses_wclrtobot"
external clrtoeol : unit -> unit = "mlcurses_clrtoeol"
external wclrtoeol : window -> unit = "mlcurses_wclrtoeol"
external start_color : unit -> err = "mlcurses_start_color"
external init_pair : int -> int -> int -> err = "mlcurses_init_pair"
external init_color : int -> int -> int -> int -> err = "mlcurses_init_color"
external has_colors : unit -> bool = "mlcurses_has_colors"
external can_change_color : unit -> bool = "mlcurses_can_change_color"
external color_content : int -> int * int * int = "mlcurses_color_content"
external pair_content : int -> int * int = "mlcurses_pair_content"
external colors : unit -> int = "mlcurses_colors"
external color_pairs : unit -> int = "mlcurses_color_pairs"
external delch : unit -> err = "mlcurses_delch"
external wdelch : window -> err = "mlcurses_wdelch"
external mvdelch : int -> int -> err = "mlcurses_mvdelch"
external mvwdelch : window -> int -> int -> err = "mlcurses_mvwdelch"
external deleteln : unit -> err = "mlcurses_deleteln"
external wdeleteln : window -> err = "mlcurses_wdeleteln"
external insdelln : int -> err = "mlcurses_insdelln"
external winsdelln : window -> int -> err = "mlcurses_winsdelln"
external insertln : unit -> err = "mlcurses_insertln"
external winsertln : window -> err = "mlcurses_winsertln"
external getch : unit -> int = "mlcurses_getch"
external wgetch : window -> int = "mlcurses_wgetch"
external mvgetch : int -> int -> int = "mlcurses_mvgetch"
external mvwgetch : window -> int -> int -> int = "mlcurses_mvwgetch"
external ungetch : int -> err = "mlcurses_ungetch"
external getstr : string -> err = "mlcurses_getstr"
external wgetstr : window -> string -> err = "mlcurses_wgetstr"
external mvgetstr : int -> int -> string -> err = "mlcurses_mvgetstr"
external mvwgetstr : window -> int -> int -> string -> err
  = "mlcurses_mvwgetstr"
external getnstr : string -> int -> int -> err = "mlcurses_getnstr"
external wgetnstr : window -> string -> int -> int -> err
  = "mlcurses_wgetnstr"
external mvgetnstr : int -> int -> string -> int -> int -> err
  = "mlcurses_mvgetnstr"
external mvwgetnstr : window -> int -> int -> string -> int -> int -> err
  = "mlcurses_mvwgetnstr_bytecode" "mlcurses_mvwgetnstr_native"
external getyx : window -> int * int = "mlcurses_getyx"
external getparyx : window -> int * int = "mlcurses_getparyx"
external getbegyx : window -> int * int = "mlcurses_getbegyx"
external getmaxyx : window -> int * int = "mlcurses_getmaxyx"
external inch : unit -> chtype = "mlcurses_inch"
external winch : window -> chtype = "mlcurses_winch"
external mvinch : int -> int -> chtype = "mlcurses_mvinch"
external mvwinch : window -> int -> int -> chtype = "mlcurses_mvwinch"
external inchstr : chtype array -> err = "mlcurses_inchstr"
external winchstr : window -> chtype array -> err = "mlcurses_winchstr"
external mvinchstr : int -> int -> chtype array -> err = "mlcurses_mvinchstr"
external mvwinchstr : window -> int -> int -> chtype array -> err
  = "mlcurses_mvwinchstr"
external inchnstr : chtype array -> int -> int -> err = "mlcurses_inchnstr"
external winchnstr : window -> chtype array -> int -> int -> err
  = "mlcurses_winchnstr"
external mvinchnstr : int -> int -> chtype array -> int -> int -> err
  = "mlcurses_mvinchnstr"
external mvwinchnstr :
  window -> int -> int -> chtype array -> int -> int -> err
  = "mlcurses_mvwinchnstr_bytecode" "mlcurses_mvwinchnstr_native"
external insch : chtype -> err = "mlcurses_insch"
external winsch : window -> chtype -> err = "mlcurses_winsch"
external mvinsch : int -> int -> chtype -> err = "mlcurses_mvinsch"
external mvwinsch : window -> int -> int -> chtype -> err
  = "mlcurses_mvwinsch"
external initscr : unit -> window = "mlcurses_initscr"
external endwin : unit -> unit = "mlcurses_endwin"
external isendwin : unit -> bool = "mlcurses_isendwin"
external newterm : string -> Unix.file_descr -> Unix.file_descr -> screen
  = "mlcurses_newterm"
external set_term : screen -> unit = "mlcurses_set_term"
external delscreen : screen -> unit = "mlcurses_delscreen"
external stdscr : unit -> window = "mlcurses_stdscr"
external insstr : string -> err = "mlcurses_insstr"
external winsstr : window -> string -> err = "mlcurses_winsstr"
external mvinsstr : int -> int -> string -> err = "mlcurses_mvinsstr"
external mvwinsstr : window -> int -> int -> string -> err
  = "mlcurses_mvwinsstr"
external insnstr : string -> int -> int -> err = "mlcurses_insnstr"
external winsnstr : window -> string -> int -> int -> err
  = "mlcurses_winsnstr"
external mvinsnstr : int -> int -> string -> int -> int -> err
  = "mlcurses_mvinsnstr"
external mvwinsnstr : window -> int -> int -> string -> int -> int -> err
  = "mlcurses_mvwinsnstr_bytecode" "mlcurses_mvwinsnstr_native"
external instr : string -> err = "mlcurses_instr"
external winstr : window -> string -> err = "mlcurses_winstr"
external mvinstr : int -> int -> string -> err = "mlcurses_mvinstr"
external mvwinstr : window -> int -> int -> string -> err
  = "mlcurses_mvwinstr"
external innstr : string -> int -> int -> err = "mlcurses_innstr"
external winnstr : window -> string -> int -> int -> err = "mlcurses_winnstr"
external mvinnstr : int -> int -> string -> int -> int -> err
  = "mlcurses_mvinnstr"
external mvwinnstr : window -> int -> int -> string -> int -> int -> err
  = "mlcurses_mvwinnstr_bytecode" "mlcurses_mvwinnstr_native"
external cbreak : unit -> err = "mlcurses_cbreak"
external nocbreak : unit -> err = "mlcurses_nocbreak"
external echo : unit -> err = "mlcurses_echo"
external noecho : unit -> err = "mlcurses_noecho"
external halfdelay : int -> err = "mlcurses_halfdelay"
external intrflush : window -> bool -> err = "mlcurses_intrflush"
external keypad : window -> bool -> err = "mlcurses_keypad"
external meta : window -> bool -> err = "mlcurses_meta"
external nodelay : window -> bool -> err = "mlcurses_nodelay"
external raw : unit -> err = "mlcurses_raw"
external noraw : unit -> err = "mlcurses_noraw"
external noqiflush : unit -> unit = "mlcurses_noqiflush"
external qiflush : unit -> unit = "mlcurses_qiflush"
external notimeout : window -> bool -> err = "mlcurses_notimeout"
external timeout : int -> unit = "mlcurses_timeout"
external wtimeout : window -> int -> unit = "mlcurses_wtimeout"
external typeahead : Unix.file_descr -> err = "mlcurses_typeahead"
external notypeahead : unit -> err = "mlcurses_notypeahead"
external def_prog_mode : unit -> unit = "mlcurses_def_prog_mode"
external def_shell_mode : unit -> unit = "mlcurses_def_shell_mode"
external reset_prog_mode : unit -> unit = "mlcurses_reset_prog_mode"
external reset_shell_mode : unit -> unit = "mlcurses_reset_shell_mode"
external resetty : unit -> unit = "mlcurses_resetty"
external savetty : unit -> unit = "mlcurses_savetty"
external getsyx : unit -> int * int = "mlcurses_getsyx"
external setsyx : int -> int -> unit = "mlcurses_setsyx"
external curs_set : int -> err = "mlcurses_curs_set"
external napms : int -> unit = "mlcurses_napms"
external ripoffline : bool -> unit = "mlcurses_ripoffline"
external get_ripoff : unit -> window * int = "mlcurses_get_ripoff"
external mousemask : int -> int * int = "mlcurses_mousemask"
external move : int -> int -> err = "mlcurses_move"
external wmove : window -> int -> int -> err = "mlcurses_wmove"
external clearok : window -> bool -> unit = "mlcurses_clearok"
external idlok : window -> bool -> unit = "mlcurses_idlok"
external idcok : window -> bool -> unit = "mlcurses_idcok"
external immedok : window -> bool -> unit = "mlcurses_immedok"
external leaveok : window -> bool -> unit = "mlcurses_leaveok"
external setscrreg : int -> int -> err = "mlcurses_setscrreg"
external wsetscrreg : window -> int -> int -> err = "mlcurses_wsetscrreg"
external scrollok : window -> bool -> unit = "mlcurses_scrollok"
external nl : unit -> unit = "mlcurses_nl"
external nonl : unit -> unit = "mlcurses_nonl"
external overlay : window -> window -> err = "mlcurses_overlay"
external overwrite : window -> window -> err = "mlcurses_overwrite"
external copywin :
  window -> window -> int -> int -> int -> int -> int -> int -> bool -> err
  = "mlcurses_copywin_bytecode" "mlcurses_copywin_native"
external newpad : int -> int -> window = "mlcurses_newpad"
external subpad : window -> int -> int -> int -> int -> window
  = "mlcurses_subpad"
external prefresh : window -> int -> int -> int -> int -> int -> int -> err
  = "mlcurses_prefresh_bytecode" "mlcurses_prefresh_native"
external pnoutrefresh :
  window -> int -> int -> int -> int -> int -> int -> err
  = "mlcurses_pnoutrefresh_bytecode" "mlcurses_pnoutrefresh_native"
external pechochar : window -> chtype -> err = "mlcurses_pechochar"
external refresh : unit -> err = "mlcurses_refresh"
external wrefresh : window -> err = "mlcurses_wrefresh"
external wnoutrefresh : window -> err = "mlcurses_wnoutrefresh"
external doupdate : unit -> err = "mlcurses_doupdate"
external redrawwin : window -> err = "mlcurses_redrawwin"
external wredrawln : window -> int -> int -> err = "mlcurses_wredrawln"
external wresize : window -> int -> int -> err = "mlcurses_wresize"
external resizeterm : int -> int -> err = "mlcurses_resizeterm"
external scr_dump : string -> err = "mlcurses_scr_dump"
external scr_restore : string -> err = "mlcurses_scr_restore"
external scr_init : string -> err = "mlcurses_scr_init"
external scr_set : string -> err = "mlcurses_scr_set"
external scroll : window -> err = "mlcurses_scroll"
external scrl : int -> err = "mlcurses_scrl"
external wscrl : window -> int -> err = "mlcurses_wscrl"
external slk_init : int -> err = "mlcurses_slk_init"
external slk_set : int -> string -> int -> err = "mlcurses_slk_set"
external slk_refresh : unit -> err = "mlcurses_slk_refresh"
external slk_noutrefresh : unit -> err = "mlcurses_slk_noutrefresh"
external slk_label : int -> string = "mlcurses_slk_label"
external slk_clear : unit -> err = "mlcurses_slk_clear"
external slk_restore : unit -> err = "mlcurses_slk_restore"
external slk_touch : unit -> err = "mlcurses_slk_touch"
external slk_attron : attr_t -> err = "mlcurses_slk_attron"
external slk_attroff : attr_t -> err = "mlcurses_slk_attroff"
external slk_attrset : attr_t -> err = "mlcurses_slk_attrset"
external baudrate : unit -> int = "mlcurses_baudrate"
external erasechar : unit -> char = "mlcurses_erasechar"
external has_ic : unit -> bool = "mlcurses_has_ic"
external has_il : unit -> bool = "mlcurses_has_il"
external killchar : unit -> char = "mlcurses_killchar"
external longname : unit -> string = "mlcurses_longname"
external termattrs : unit -> attr_t = "mlcurses_termattrs"
external termname : unit -> string = "mlcurses_termname"
external tgetent : string -> bool = "mlcurses_tgetent"
external tgetflag : string -> bool = "mlcurses_tgetflag"
external tgetnum : string -> int = "mlcurses_tgetnum"
external tgetstr : string -> bool = "mlcurses_tgetstr"
external tgoto : string -> int -> int -> string = "mlcurses_tgoto"
external setupterm : string -> Unix.file_descr -> err = "mlcurses_setupterm"
external setterm : string -> err = "mlcurses_setterm"
external cur_term : unit -> terminal = "mlcurses_cur_term"
external set_curterm : terminal -> terminal = "mlcurses_set_curterm"
external del_curterm : terminal -> err = "mlcurses_del_curterm"
external restartterm : string -> Unix.file_descr -> err
  = "mlcurses_restartterm"
external putp : string -> err = "mlcurses_putp"
external vidattr : chtype -> err = "mlcurses_vidattr"
external mvcur : int -> int -> int -> int -> err = "mlcurses_mvcur"
external tigetflag : string -> bool = "mlcurses_tigetflag"
external tigetnum : string -> int = "mlcurses_tigetnum"
external tigetstr : string -> string = "mlcurses_tigetstr"
external tputs : string -> int -> (char -> unit) -> err = "mlcurses_tputs"
external vidputs : chtype -> (char -> unit) -> err = "mlcurses_vidputs"
external tparm : string -> int array -> string = "mlcurses_tparm"
external bool_terminfo_variable : int -> string * string * string
  = "mlcurses_bool_terminfo_variable"
external num_terminfo_variable : int -> string * string * string
  = "mlcurses_num_terminfo_variable"
external str_terminfo_variable : int -> string * string * string
  = "mlcurses_str_terminfo_variable"
external touchwin : window -> err = "mlcurses_touchwin"
external touchline : window -> int -> int -> err = "mlcurses_touchline"
external untouchwin : window -> err = "mlcurses_untouchwin"
external wtouchln : window -> int -> int -> bool -> err = "mlcurses_wtouchln"
external is_linetouched : window -> int -> int = "mlcurses_is_linetouched"
external is_wintouched : window -> bool = "mlcurses_is_wintouched"
external unctrl : chtype -> string = "mlcurses_unctrl"
external keyname : int -> string = "mlcurses_keyname"
external filter : unit -> unit = "mlcurses_filter"
external use_env : bool -> unit = "mlcurses_use_env"
external putwin : window -> Unix.file_descr -> err = "mlcurses_putwin"
external getwin : Unix.file_descr -> window = "mlcurses_getwin"
external delay_output : int -> err = "mlcurses_delay_output"
external flushinp : unit -> unit = "mlcurses_flushinp"
external newwin : int -> int -> int -> int -> window = "mlcurses_newwin"
external delwin : window -> err = "mlcurses_delwin"
external mvwin : window -> int -> int -> err = "mlcurses_mvwin"
external subwin : window -> int -> int -> int -> int -> window
  = "mlcurses_subwin"
external derwin : window -> int -> int -> int -> int -> window
  = "mlcurses_derwin"
external mvderwin : window -> int -> int -> err = "mlcurses_mvderwin"
external dupwin : window -> window = "mlcurses_dupwin"
external wsyncup : window -> unit = "mlcurses_wsyncup"
external syncok : window -> bool -> err = "mlcurses_syncok"
external wcursyncup : window -> unit = "mlcurses_wcursyncup"
external wsyncdown : window -> unit = "mlcurses_wsyncdown"
external get_acs_codes : unit -> Acs.acs = "mlcurses_get_acs_codes"
external winch_handler_on : unit -> unit = "mlcurses_winch_handler_on"
external winch_handler_off : unit -> unit = "mlcurses_winch_handler_off"
external get_size : unit -> int * int = "mlcurses_get_size"
external get_size_fd : Unix.file_descr -> int * int = "mlcurses_get_size_fd"
val null_window : window
val bool_terminfo_variables : (string, string * string) Hashtbl.t
val num_terminfo_variables : (string, string * string) Hashtbl.t
val str_terminfo_variables : (string, string * string) Hashtbl.t
module A :
  sig
    val normal : int
    val attributes : int
    val chartext : int
    val color : int
    val standout : int
    val underline : int
    val reverse : int
    val blink : int
    val dim : int
    val bold : int
    val altcharset : int
    val invis : int
    val protect : int
    val horizontal : int
    val left : int
    val low : int
    val right : int
    val top : int
    val vertical : int
    val combine : int list -> int
    val color_pair : int -> int
    val pair_number : int -> int
  end
module WA :
  sig
    val normal : int
    val attributes : int
    val chartext : int
    val color : int
    val standout : int
    val underline : int
    val reverse : int
    val blink : int
    val dim : int
    val bold : int
    val altcharset : int
    val invis : int
    val protect : int
    val horizontal : int
    val left : int
    val low : int
    val right : int
    val top : int
    val vertical : int
    val combine : int list -> int
    val color_pair : int -> int
    val pair_number : int -> int
  end
module Color :
  sig
    val black : int
    val red : int
    val green : int
    val yellow : int
    val blue : int
    val magenta : int
    val cyan : int
    val white : int
  end
module Key :
  sig
    val code_yes : int
    val min : int
    val break : int
    val down : int
    val up : int
    val left : int
    val right : int
    val home : int
    val backspace : int
    val f0 : int
    val dl : int
    val il : int
    val dc : int
    val ic : int
    val eic : int
    val clear : int
    val eos : int
    val eol : int
    val sf : int
    val sr : int
    val npage : int
    val ppage : int
    val stab : int
    val ctab : int
    val catab : int
    val enter : int
    val sreset : int
    val reset : int
    val print : int
    val ll : int
    val a1 : int
    val a3 : int
    val b2 : int
    val c1 : int
    val c3 : int
    val btab : int
    val beg : int
    val cancel : int
    val close : int
    val command : int
    val copy : int
    val create : int
    val end_ : int
    val exit : int
    val find : int
    val help : int
    val mark : int
    val message : int
    val move : int
    val next : int
    val open_ : int
    val options : int
    val previous : int
    val redo : int
    val reference : int
    val refresh : int
    val replace : int
    val restart : int
    val resume : int
    val save : int
    val sbeg : int
    val scancel : int
    val scommand : int
    val scopy : int
    val screate : int
    val sdc : int
    val sdl : int
    val select : int
    val send : int
    val seol : int
    val sexit : int
    val sfind : int
    val shelp : int
    val shome : int
    val sic : int
    val sleft : int
    val smessage : int
    val smove : int
    val snext : int
    val soptions : int
    val sprevious : int
    val sprint : int
    val sredo : int
    val sreplace : int
    val sright : int
    val srsume : int
    val ssave : int
    val ssuspend : int
    val sundo : int
    val suspend : int
    val undo : int
    val mouse : int
    val resize : int
    val max : int
    val f : int -> int
  end
