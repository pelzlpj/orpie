(*  Orpie -- a stack-based RPN calculator for the console
 *  Copyright (C) 2003-2004  Paul Pelzl
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Please send bug reports, patches, etc. to Paul Pelzl at 
 *  <pelzlpj@eecs.umich.edu>.
 *)


open Genlex;;
open Curses;;
open Operations;;


exception Config_failure of string;;
let config_failwith s = raise (Config_failure s);;


(* These hashtables store conversions between curses keys and the operations
 * they are associated with. *)
let table_key_function = Hashtbl.create 20;;
let table_function_key = Hashtbl.create 20;;
let table_key_command  = Hashtbl.create 20;;
let table_command_key  = Hashtbl.create 20;;
let table_key_edit     = Hashtbl.create 20;;
let table_edit_key     = Hashtbl.create 20;;
let table_key_browse   = Hashtbl.create 20;;
let table_browse_key   = Hashtbl.create 20;;
let table_extended_key = Hashtbl.create 20;;
let table_key_extended = Hashtbl.create 20;;
let table_intedit_key  = Hashtbl.create 20;;
let table_key_intedit  = Hashtbl.create 20;;
let table_key_macro    = Hashtbl.create 20;;
let table_key_varedit  = Hashtbl.create 20;;
let table_varedit_key  = Hashtbl.create 20;;


(* Default directory for orpie data *)
let datadir = ref "~/.orpie"
(* Default editor for fullscreen viewing *)
let editor = ref "vi";;
(* Whether or not to hide the help panel *)
let hide_help = ref false;;
(* Whether or not to conserve memory in favor of faster display *)
let conserve_memory = ref false;;
(* Autobinding keys *)
let autobind_keys_list : (int * string * operation_t option * int) list ref = ref [];;
let autobind_keys = ref (Array.make 1 (0, "", None, 0));;
(* List of included rc files *)
let included_rcfiles : (string list) ref = ref [];;


let function_of_key key =
   Hashtbl.find table_key_function key;;
let key_of_function f =
   Hashtbl.find table_function_key f;;
let command_of_key key =
   Hashtbl.find table_key_command key;;
let key_of_command command =
   Hashtbl.find table_command_key command;;
let edit_of_key key =
   Hashtbl.find table_key_edit key;;
let key_of_edit edit_op =
   Hashtbl.find table_edit_key edit_op;;
let browse_of_key key =
   Hashtbl.find table_key_browse key;;
let key_of_browse browse_op =
   Hashtbl.find table_browse_key browse_op;;
let extended_of_key key =
   Hashtbl.find table_key_extended key;;
let key_of_extended ex_op =
   Hashtbl.find table_extended_key ex_op;;
let intedit_of_key key =
   Hashtbl.find table_key_intedit key;;
let key_of_intedit edit_op =
   Hashtbl.find table_intedit_key edit_op;;
let macro_of_key key =
   Hashtbl.find table_key_macro key;;
let varedit_of_key key =
   Hashtbl.find table_key_varedit key;;
let key_of_varedit edit_op =
   Hashtbl.find table_varedit_key edit_op;;


let key_of_operation (op : operation_t) =
   match op with
   |Function _ -> Hashtbl.find table_function_key op
   |Command _  -> Hashtbl.find table_command_key op
   |Edit _     -> Hashtbl.find table_edit_key op
   |Browse _   -> Hashtbl.find table_browse_key op
   |Extend _   -> Hashtbl.find table_extended_key op
   |IntEdit _  -> Hashtbl.find table_intedit_key op
   |VarEdit _  -> Hashtbl.find table_varedit_key op


(* abbreviations used in extended entry mode *)
let extended_commands = ref "";;
let abbrev_command_table = Hashtbl.create 50;;
let command_abbrev_table = Hashtbl.create 50;;

(* Register an abbreviation for an extended command.
 * This updates the string used in regexp matching, and
 * updates the hashtable used to find the corresponding operation. *)
let register_abbrev abbr op =
   (* Dummyproofing: if an abbreviation is a prefix of another
    * abbreviation, then it *must* lie earlier in the search order.
    * If not, it becomes impossible to execute the prefix command. *)
   let regex = Str.regexp ("^" ^ abbr ^ ".*$") in
   (try
      let match_pos = Str.search_forward regex !extended_commands 0 in
      let before = Str.string_before !extended_commands match_pos
      and after  = Str.string_after !extended_commands match_pos in
      extended_commands := before ^ abbr ^ "\n" ^ after
   with Not_found ->
      extended_commands := !extended_commands ^ abbr ^ "\n");
   Hashtbl.add abbrev_command_table abbr op;
   Hashtbl.add command_abbrev_table op abbr;;
   

let translate_extended_abbrev abb =
   Hashtbl.find abbrev_command_table abb;;
let abbrev_of_operation op =
   Hashtbl.find command_abbrev_table op;;




let decode_single_key_string key_string =
   let decode_alias str =
      match str with
      |"<esc>" -> 27
      |"<tab>" -> 9
      |"<enter>" -> Key.enter
      |"<return>" -> 10
      |"<insert>" -> Key.ic
      |"<delete>" -> Key.dc
      |"<home>" -> Key.home
      |"<end>" -> Key.end_
      |"<pageup>" -> Key.ppage
      |"<pagedown>" -> Key.npage
      |"<space>" -> 32
      |"<backspace>" -> Key.backspace
      |"<left>" -> Key.left
      |"<right>" -> Key.right
      |"<up>" -> Key.up
      |"<down>" -> Key.down
      |"<f1>" -> (Key.f 1)
      |"<f2>" -> (Key.f 2)
      |"<f3>" -> (Key.f 3)
      |"<f4>" -> (Key.f 4)
      |"<f5>" -> (Key.f 5)
      |"<f6>" -> (Key.f 6)
      |"<f7>" -> (Key.f 7)
      |"<f8>" -> (Key.f 8)
      |"<f9>" -> (Key.f 9)
      |"<f10>" -> (Key.f 10)
      |"<f11>" -> (Key.f 11)
      |"<f12>" -> (Key.f 12)
      |_ -> 
         if String.length key_string = 1 then
            int_of_char str.[0]
         else
            config_failwith ("Unrecognized key \"" ^ str ^ "\"")
   in
   (* This regexp is used to extract the ctrl and meta characters from a string
    * representing a keypress.
    * It matches \\M\\C or \\C\\M or \\C or \\M (or no such characters) followed
    * by an arbitrary string. *)
   (* Note: is there a way to use raw strings here?  Getting tired of those
    * backslashes...*)
   let cm_re = Str.regexp
   "^\\(\\(\\\\M\\\\C\\|\\\\C\\\\M\\)\\|\\(\\\\M\\)\\|\\(\\\\C\\)\\)?\\(<.+>\\|.\\)"
   in
   if Str.string_match cm_re key_string 0 then
      let has_meta_ctrl =
         try let _ = Str.matched_group 2 key_string in true
         with Not_found -> false
      and has_meta =
         try let _  = Str.matched_group 3 key_string in true
         with Not_found -> false
      and has_ctrl =
         try let _ = Str.matched_group 4 key_string in true
         with Not_found -> false
      and main_key = Str.matched_group 5 key_string in
      if has_meta_ctrl then
         if String.length main_key = 1 then
            let uc_main_key = String.uppercase main_key in
            let mc_chtype = ((int_of_char uc_main_key.[0]) + 64) in
            let mc_str = "M-C-" ^ uc_main_key in
            (mc_chtype, mc_str)
         else
            config_failwith ("Cannot apply \\\\M\\\\C to key \"" ^ main_key ^ "\";\n" ^
                       "octal notation might let you accomplish this.")
      else if has_meta then
         if String.length main_key = 1 then
            let m_chtype = ((int_of_char main_key.[0]) + 128) in
            let m_str = "M-" ^ main_key in
            (m_chtype, m_str)
         else
            config_failwith ("Cannot apply \\\\M to key \"" ^ main_key ^ "\";\n" ^
                       "octal notation might let you accomplish this.")
      else if has_ctrl then
         if String.length main_key = 1 then
            let uc_main_key = String.uppercase main_key in
            let c_chtype = ((int_of_char uc_main_key.[0]) - 64) in
            let c_str = "C-" ^ uc_main_key in
            (c_chtype, c_str)
         else
            config_failwith ("Cannot apply \\\\C to key \"" ^ main_key ^ "\";\n" ^
                       "octal notation might let you accomplish this.")
      else 
         let octal_regex = Str.regexp "^0o" in
         try
            let pos = Str.search_forward octal_regex key_string 0 in
            ((int_of_string key_string), ("\\" ^ Str.string_after key_string
            2))
         with
            _ -> ((decode_alias main_key), main_key)
   else
      config_failwith ("Unable to match binding string with standard regular expression.")



(* Register a key binding.  This adds hash table entries for translation
 * between curses chtypes and commands (in both directions).
 * This can also unregister bindings, if the operation is NoFunc, NoComm, etc. *)
let register_binding_internal k k_string op =
   match op with
   |Function NoFunc ->
      begin try
         let old_op = Hashtbl.find table_key_function k in
         Hashtbl.remove table_key_function k;
         Hashtbl.remove table_function_key old_op
      with Not_found -> ()
      end
   |Function _ ->
      Hashtbl.add table_key_function k op;
      Hashtbl.add table_function_key op k_string
   |Command NoComm ->
      begin try
         let old_op = Hashtbl.find table_key_command k in
         Hashtbl.remove table_key_command k;
         Hashtbl.remove table_command_key old_op
      with Not_found -> ()
      end
   |Command _ ->
      Hashtbl.add table_key_command k op;
      Hashtbl.add table_command_key op k_string
   |Edit NoEdit ->
      begin try
         let old_op = Hashtbl.find table_key_edit k in
         Hashtbl.remove table_key_edit k;
         Hashtbl.remove table_edit_key old_op
      with Not_found -> ()
      end
   |Edit _ ->
      Hashtbl.add table_key_edit k op;
      Hashtbl.add table_edit_key op k_string
   |Browse NoBrowse ->
      begin try
         let old_op = Hashtbl.find table_key_browse k in
         Hashtbl.remove table_key_browse k;
         Hashtbl.remove table_browse_key old_op
      with Not_found -> ()
      end
   |Browse _ ->
      Hashtbl.add table_key_browse k op;
      Hashtbl.add table_browse_key op k_string
   |Extend NoExt ->
      begin try
         let old_op = Hashtbl.find table_key_extended k in
         Hashtbl.remove table_key_extended k;
         Hashtbl.remove table_extended_key old_op
      with Not_found -> ()
      end
   |Extend _ ->
      Hashtbl.add table_key_extended k op;
      Hashtbl.add table_extended_key op k_string
   |IntEdit NoInt ->
      begin try
         let old_op = Hashtbl.find table_key_intedit k in
         Hashtbl.remove table_key_intedit k;
         Hashtbl.remove table_intedit_key old_op
      with Not_found -> ()
      end
   |IntEdit _ ->
      Hashtbl.add table_key_intedit k op;
      Hashtbl.add table_intedit_key op k_string
   |VarEdit NoVarEdit ->
      begin try
         let old_op = Hashtbl.find table_key_varedit k in
         Hashtbl.remove table_key_varedit k;
         Hashtbl.remove table_varedit_key old_op
      with Not_found -> ()
      end
   |VarEdit _ ->
      Hashtbl.add table_key_varedit k op;
      Hashtbl.add table_varedit_key op k_string



(* convenience routine for previous *)
let register_binding key_string op =
   (* given a string that represents a character, find the associated
    * curses chtype *)
   let k, string_rep = decode_single_key_string key_string in
   register_binding_internal k string_rep op



(* Remove a key binding. *)
let remove_binding k op =
   match op with
   |Function _ ->
      Hashtbl.remove table_key_function k;
      Hashtbl.remove table_function_key op
   |Command _ ->
      Hashtbl.remove table_key_command k;
      Hashtbl.remove table_command_key op
   |Edit _ ->
      Hashtbl.remove table_key_edit k;
      Hashtbl.remove table_edit_key op
   |Browse _ ->
      Hashtbl.remove table_key_browse k;
      Hashtbl.remove table_browse_key op
   |Extend _ ->
      Hashtbl.remove table_key_extended k;
      Hashtbl.remove table_extended_key op
   |IntEdit _ ->
      Hashtbl.remove table_key_intedit k;
      Hashtbl.remove table_intedit_key op
   |VarEdit _ ->
      Hashtbl.remove table_key_varedit k;
      Hashtbl.remove table_varedit_key op


(* Register a macro.  This parses the macro string and divides it into multiple
 * whitespace-separated keypresses, then stores the list of keypresses in the
 * appropriate hashtable. *)
let register_macro key keys_string =
   let macro_ch = fst (decode_single_key_string key) in
   let split_regex = Str.regexp "[ \t]+" in
   let keys_list = Str.split split_regex keys_string in
   let ch_of_key_string k_string =
      fst (decode_single_key_string k_string)
   in
   let ch_list = List.rev_map ch_of_key_string keys_list in
   Hashtbl.add table_key_macro macro_ch ch_list
      


(* translate a command string to the command type it represents *)
let operation_of_string command_str =
   begin match command_str with
   |"function_add"                  -> (Function Add)
   |"function_sub"                  -> (Function Sub)
   |"function_mult"                 -> (Function Mult)
   |"function_div"                  -> (Function Div)
   |"function_neg"                  -> (Function Neg)
   |"function_inv"                  -> (Function Inv)
   |"function_pow"                  -> (Function Pow)
   |"function_sq"                   -> (Function Sq)
   |"function_sqrt"                 -> (Function Sqrt)
   |"function_abs"                  -> (Function Abs)
   |"function_arg"                  -> (Function Arg)
   |"function_exp"                  -> (Function Exp)
   |"function_ln"                   -> (Function Ln)
   |"function_10_x"                 -> (Function Ten_x)
   |"function_log10"                -> (Function Log10)
   |"function_conj"                 -> (Function Conj)
   |"function_sin"                  -> (Function Sin)
   |"function_cos"                  -> (Function Cos)
   |"function_tan"                  -> (Function Tan)
   |"function_asin"                 -> (Function Asin)
   |"function_acos"                 -> (Function Acos)
   |"function_atan"                 -> (Function Atan)
   |"function_sinh"                 -> (Function Sinh)
   |"function_cosh"                 -> (Function Cosh)
   |"function_tanh"                 -> (Function Tanh)
   |"function_asinh"                -> (Function Asinh)
   |"function_acosh"                -> (Function Acosh)
   |"function_atanh"                -> (Function Atanh)
   |"function_re"                   -> (Function Re)
   |"function_im"                   -> (Function Im)
   |"function_gamma"                -> (Function Gamma)
   |"function_lngamma"              -> (Function LnGamma)
   |"function_erf"                  -> (Function Erf)
   |"function_erfc"                 -> (Function Erfc)
   |"function_factorial"            -> (Function Fact)
   |"function_transpose"            -> (Function Transpose)
   |"function_mod"                  -> (Function Mod)
   |"function_floor"                -> (Function Floor)
   |"function_ceiling"              -> (Function Ceiling)
   |"function_to_int"               -> (Function ToInt)
   |"function_to_real"              -> (Function ToFloat)
   |"function_solve_linear"         -> (Function SolveLin)
   |"function_eval"                 -> (Function Eval)
   |"function_store"                -> (Function Store)
   |"function_purge"                -> (Function Purge)
   |"function_gcd"                  -> (Function Gcd)
   |"function_lcm"                  -> (Function Lcm)
   |"function_binomial_coeff"       -> (Function Binom)
   |"function_permutation"          -> (Function Perm)
   |"function_total"                -> (Function Total)
   |"function_mean"                 -> (Function Mean)
   |"function_sumsq"                -> (Function Sumsq)
   |"function_var_unbiased"         -> (Function Var)
   |"function_var_biased"           -> (Function VarBias)
   |"function_stdev_unbiased"       -> (Function Stdev)
   |"function_stdev_biased"         -> (Function StdevBias)
   |"function_minimum"              -> (Function Min)
   |"function_maximum"              -> (Function Max)
   |"function_utpn"                 -> (Function Utpn)
   |"function_noop"                 -> (Function NoFunc)
   |"edit_begin_integer"            -> (Edit BeginInteger)
   |"edit_complex"                  -> (Edit BeginComplex)
   |"edit_matrix"                   -> (Edit BeginMatrix)
   |"edit_separator"                -> (Edit Separator)
   |"edit_angle"                    -> (Edit Angle)
   |"edit_minus"                    -> (Edit Minus)
   |"edit_backspace"                -> (Edit Backspace)
   |"edit_enter"                    -> (Edit Enter)
   |"edit_scientific_notation_base" -> (Edit SciNotBase)
   |"edit_noop"                     -> (Edit NoEdit)
   |"command_drop"                  -> (Command Drop)
   |"command_clear"                 -> (Command Clear)
   |"command_swap"                  -> (Command Swap)
   |"command_dup"                   -> (Command Dup)
   |"command_undo"                  -> (Command Undo)
   |"command_begin_browsing"        -> (Command BeginBrowse)
   |"command_begin_extended"        -> (Command BeginExtended)
   |"command_begin_variable"        -> (Command BeginVar)
   |"command_quit"                  -> (Command Quit)
   |"command_rad"                   -> (Command SetRadians)
   |"command_deg"                   -> (Command SetDegrees)
   |"command_rect"                  -> (Command SetRect)
   |"command_polar"                 -> (Command SetPolar)
   |"command_bin"                   -> (Command SetBin)
   |"command_oct"                   -> (Command SetOct)
   |"command_dec"                   -> (Command SetDec)
   |"command_hex"                   -> (Command SetHex)
   |"command_toggle_angle_mode"     -> (Command ToggleAngleMode)
   |"command_toggle_complex_mode"   -> (Command ToggleComplexMode)
   |"command_cycle_base"            -> (Command CycleBase)
   |"command_view"                  -> (Command View)
   |"command_refresh"               -> (Command Refresh)
   |"command_about"                 -> (Command About)
   |"command_enter_pi"              -> (Command EnterPi)
   |"command_rand"                  -> (Command Rand)
   |"command_edit_input"            -> (Command EditInput)
   |"command_cycle_help"            -> (Command CycleHelp)
   |"command_noop"                  -> (Command NoComm)
   |"browse_end"                    -> (Browse EndBrowse)
   |"browse_scroll_left"            -> (Browse ScrollLeft)
   |"browse_scroll_right"           -> (Browse ScrollRight)
   |"browse_prev_line"              -> (Browse PrevLine)
   |"browse_next_line"              -> (Browse NextLine)
   |"browse_echo"                   -> (Browse Echo)
   |"browse_rolldown"               -> (Browse RollDown)
   |"browse_rollup"                 -> (Browse RollUp)
   |"browse_view"                   -> (Browse ViewEntry)
   |"browse_drop"                   -> (Browse Drop1)
   |"browse_dropn"                  -> (Browse DropN)
   |"browse_keep"                   -> (Browse Keep)
   |"browse_keepn"                  -> (Browse KeepN)
   |"browse_edit"                   -> (Browse EditEntry)
   |"browse_noop"                   -> (Browse NoBrowse)
   |"extended_exit"                 -> (Extend ExitExtended)
   |"extended_enter"                -> (Extend EnterExtended)
   |"extended_backspace"            -> (Extend ExtBackspace)
   |"extended_noop"                 -> (Extend NoExt)
   |"integer_cancel"                -> (IntEdit ExitIntEdit)
   |"integer_noop"                  -> (IntEdit NoInt)
   |"variable_cancel"               -> (VarEdit ExitVarEdit)
   |"variable_enter"                -> (VarEdit EnterVarEdit)
   |"variable_backspace"            -> (VarEdit VarEditBackspace)
   |"variable_complete"             -> (VarEdit CompleteVarEdit)
   |"variable_noop"                 -> (VarEdit NoVarEdit)
   |"function_rand"                 -> config_failwith 
                                       "operation \"function_rand\" is deprecated; please replace with \"command_rand\"."
   |_                               -> config_failwith ("Unknown command name \"" ^ command_str ^ "\"")
   end



(* Parse a line from an Orpie configuration file.  This operates on a stream
 * corresponding to a non-empty line from the file.  It will match commands
 * of the form
 *    bind key command
 *    macro key multiple_keys
 * where 'key' is either a quoted string containing a key specifier or an octal
 * key representation of the form \xxx (unquoted), and multiple_keys is a quoted
 * string containing a number of keypresses to simulate.
 *)
let parse_line line_stream = 
   match line_stream with parser
   | [< 'Kwd "include" >] ->
      begin match line_stream with parser
      | [< 'String include_file >] ->
         included_rcfiles := include_file :: !included_rcfiles
      | [< >] ->
         config_failwith ("Expected a filename string after \"include\"")
      end
   | [< 'Kwd "bind" >] -> 
      let bind_key key = 
         begin match line_stream with parser
         | [< 'Ident command_str >] ->
            let command = operation_of_string command_str in
            register_binding key command
         | [< >] ->
            config_failwith ("Expected a command name after \"bind \"" ^ key ^ "\"")
         end
      in
      begin match line_stream with parser
      | [< 'String k >] -> 
         bind_key k
      | [< 'Ident "\\" >] ->
         begin match line_stream with parser
         | [< 'Int octal_int >] ->
            begin
               try
                  let octal_digits = "0o" ^ (string_of_int octal_int) in
                  bind_key octal_digits 
               with 
                  (Failure "int_of_string") -> config_failwith "Expected octal digits after \"\\\""
            end
         | [< >]  ->
            config_failwith "Expected octal digits after \"\\\""
         end
      | [< >] ->
         config_failwith "Expected a key string after keyword \"bind\""
      end
   | [< 'Kwd "autobind" >] ->
      begin match line_stream with parser
      | [< 'String k >] -> 
         let key, key_string = decode_single_key_string k in
         autobind_keys_list := (key, key_string, None, 1) :: !autobind_keys_list
      | [< 'Ident "\\" >] ->
         begin match line_stream with parser
         | [< 'Int octal_int >] ->
            begin
               try
                  let octal_digits = "0o" ^ (string_of_int octal_int) in
                  let key, key_string = decode_single_key_string octal_digits in
                  autobind_keys_list := (key, key_string, None, 1) :: !autobind_keys_list
               with 
                  (Failure "int_of_string") -> config_failwith "Expected octal digits after \"\\\""
            end
         | [< >]  ->
            config_failwith "Expected octal digits after \"\\\""
         end
      | [< >] ->
         config_failwith "Expected a key string after keyword \"bind\""
      end
   | [< 'Kwd "macro" >] ->
      begin match line_stream with parser
      | [< 'String key >] ->
         begin match line_stream with parser
         | [< 'String generated_keys >] ->
            register_macro key generated_keys
         | [< >] ->
            config_failwith ("Expected a key string after \"macro \"" ^ key ^ "\"")
         end
      | [< >] ->
         config_failwith "Expected a key string after keyword \"macro\""
      end
   | [< 'Kwd "abbrev" >] ->
      begin match line_stream with parser
      | [< 'String abbr >] ->
         begin match line_stream with parser
         | [< 'Ident command_str >] ->
            let command = operation_of_string command_str in
            register_abbrev abbr command
         | [< >] ->
            config_failwith ("Expected a command name after \"abbrev \"" ^ abbr ^ "\"")
         end
      end
   | [< 'Kwd "set" >] ->
      begin match line_stream with parser
      | [< 'Ident "datadir" >] ->
         begin match line_stream with parser
         | [< 'Ident "=" >] ->
            begin match line_stream with parser
            | [< 'String dir >] ->
               datadir := dir
            | [< >] ->
               config_failwith ("Expected a directory string after " ^
               "\"set datadir = \"")
            end
         | [< >] ->
            config_failwith ("Expected \"=\" after \"set datadir\"")
         end
      | [< 'Ident "editor" >] ->
         begin match line_stream with parser
         | [< 'Ident "=" >] ->
            begin match line_stream with parser
            | [< 'String executable >] ->
               ( (* Printf.fprintf stderr "using editor \"%s\"\n" executable; *)
               editor := executable)
            | [< >] ->
               config_failwith ("Expected an executable filename string after " ^
               "\"set editor = \"")
            end
         | [< >] ->
            config_failwith ("Expected \"=\" after \"set editor\"")
         end
      | [< 'Ident "hide_help" >] ->
         begin match line_stream with parser
         | [< 'Ident "=" >] ->
            begin match line_stream with parser
            | [< 'String setting >] ->
               if setting = "true" then
                  hide_help := true
               else if setting = "false" then
                  hide_help := false
               else
                  config_failwith ("Expected a boolean argument after " ^
                  "\"set hide_help = \"")
            | [< >] ->
               config_failwith ("Expected a boolean argument after " ^
               "\"set hide_help = \"")
            end
         | [< >] ->
            config_failwith ("Expected \"=\" after \"set hide_help\"")
         end
      | [< 'Ident "conserve_memory" >] ->
         begin match line_stream with parser
         | [< 'Ident "=" >] ->
            begin match line_stream with parser
            | [< 'String setting >] ->
               if setting = "true" then
                  conserve_memory := true
               else if setting = "false" then
                  conserve_memory := false
               else
                  config_failwith ("Expected a boolean argument after " ^
                  "\"set conserve_memory = \"")
            | [< >] ->
               config_failwith ("Expected a boolean argument after " ^
               "\"set conserve_memory = \"")
            end
         | [< >] ->
            config_failwith ("Expected \"=\" after \"set conserve_memory\"")
         end
      | [< >] ->
         config_failwith ("Unmatched variable name after \"set\"")
      end
   | [< 'Kwd "#" >] ->
      ()
   | [< >] ->
      config_failwith "Expected a keyword at start of line";;


(* obtain a valid autobinding array, eliminating duplicate keys *)
let generate_autobind_array () =
   let candidates = Array.of_list (List.rev !autobind_keys_list) in
   let temp_arr = Array.make (Array.length candidates) (0, "", None, 0) in
   let pointer = ref 0 in
   for i = 0 to pred (Array.length candidates) do
      let (c_k, c_ss, c_bound_f, c_age) = candidates.(i) in
      let matched = ref false in
      for j = 0 to !pointer do
         let (t_k, t_ss, t_bound_f, t_age) = temp_arr.(j) in
         if c_k = t_k then matched := true else ()
      done;
      if not !matched then begin
         temp_arr.(!pointer) <- candidates.(i);
         pointer := succ !pointer
      end else
         ()
   done;
   autobind_keys := Array.sub temp_arr 0 !pointer




(* compare a set of autobindings saved to disk to the set loaded from the
 * orpierc file.  If the autobindings match and the hashtbl abbreviations
 * are the same, then use the saved version. *)
let validate_saved_autobindings saved_autobind =
   if Array.length !autobind_keys = Array.length saved_autobind then
      let is_valid = ref true in
      for i = 0 to pred (Array.length saved_autobind) do
         let (s_key, s_key_str, s_bound_f, s_age) = saved_autobind.(i)
         and (n_key, n_key_str, n_bound_f, n_age) = !autobind_keys.(i) in
         if s_key = n_key then begin
            try
               begin match s_bound_f with
               |None -> ()
               |Some op ->
                  let _ = abbrev_of_operation op in ()
               end
            with Not_found ->
               (* if the function has no associated abbreviation, then consider
                * the saved autobindings to be flawed *)
               is_valid := false
         end else
            (* if the autobindings are different from the saved set, then
             * consider the saved set to be flawed. *)
            is_valid := false
      done;
      if !is_valid then begin
         autobind_keys := saved_autobind;
         for i = 0 to pred (Array.length !autobind_keys) do
            let (n_key, n_key_str, n_bound_f, n_age) = !autobind_keys.(i) in
            match n_bound_f with
            |None    -> ()
            |Some op -> register_binding_internal n_key n_key_str op
         done
      end else
         ()
   else
      ()
             


(* try opening the rc file, first looking at $HOME/.orpierc, 
 * then looking at $PREFIX/etc/orpierc *)
let open_rcfile rcfile_op =
   match rcfile_op with
   |None ->
      let home_rcfile =
         let homedir = Sys.getenv "HOME" in
         homedir ^ "/.orpierc"
      in
      let prefix_rcfile = 
         if Install.prefix = "/usr" || Install.prefix = "/usr/" then
            "/etc/orpierc"
         else
            Install.prefix ^ "/etc/orpierc" 
      in
      begin try (open_in home_rcfile, home_rcfile)
      with Sys_error error_str ->
         begin try (open_in prefix_rcfile, prefix_rcfile)
         with Sys_error error_str -> failwith 
            ("Could not open configuration file \"" ^ home_rcfile ^ "\" or \"" ^ 
            prefix_rcfile ^ "\" .")
         end
      end
   |Some file ->
      try (Utility.expand_open_in_ascii file, file)
      with Sys_error error_str -> config_failwith
      ("Could not open configuration file \"" ^ file ^ "\".")



let rec process_rcfile rcfile_op =
   let line_lexer line = 
      make_lexer ["include"; "bind"; "autobind"; "abbrev"; "macro"; "set"; "#"] (Stream.of_string line)
   in
   let empty_regexp = Str.regexp "^[\t ]*$" in
   let config_stream, rcfile_filename = open_rcfile rcfile_op in
   let line_num = ref 0 in
   try
      while true do
         line_num := succ !line_num;
         let line_string = input_line config_stream in
         (* Printf.fprintf stderr "read line %2d: %s\n" !line_num line_string;
         flush stderr; *)
         if Str.string_match empty_regexp line_string 0 then
            (* do nothing on an empty line *)
            ()
         else
            try
               let line_stream = line_lexer line_string in
               parse_line line_stream;
               (* process any included rcfiles as they are encountered *)
               begin match !included_rcfiles with
               |[] -> ()
               |head :: tail -> 
                  included_rcfiles := tail;
                  process_rcfile (Some head)
               end
            with
               |Config_failure s ->
                  (let error_str = Printf.sprintf "Syntax error on line %d of \"%s\": %s"
                  !line_num rcfile_filename s in
                  failwith error_str)
               |Stream.Failure ->
                  failwith (Printf.sprintf "Syntax error on line %d of \"%s\"" 
                  !line_num rcfile_filename)

      done
   with End_of_file ->
      begin
         close_in config_stream;
         generate_autobind_array ()
      end




(* arch-tag: DO_NOT_CHANGE_614115ed-7d1d-4834-bda4-e6cf93ac3fcd *)
