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
let table_key_browse   = Hashtbl.create 20;;
let table_browse_key   = Hashtbl.create 20;;
let table_extended_key = Hashtbl.create 20;;
let table_key_extended = Hashtbl.create 20;;



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
let browse_of_key key =
   Hashtbl.find table_key_browse key;;
let key_of_browse browse_op =
   Hashtbl.find table_browse_key browse_op;;
let extended_of_key key =
   Hashtbl.find table_key_extended key;;
let key_of_extended ex_op =
   Hashtbl.find table_extended_key ex_op;;



(* Register a key binding.  This adds hash table entries for translation
 * between curses chtypes and commands (in both directions). *)
let register_binding key_string op =
   let make_entries k k_string =
      begin
         Printf.fprintf stderr "registering binding %d (%s)\n" k k_string;
         flush stderr;
         match op with
         |Function f ->
            (Hashtbl.add table_key_function k op;
            Hashtbl.add table_function_key op k_string)
         |Command c ->
            (Hashtbl.add table_key_command k op;
            Hashtbl.add table_command_key op k_string)
         |Edit e ->
            Hashtbl.add table_key_edit k op
         |Browse b ->
            (Hashtbl.add table_key_browse k op;
            Hashtbl.add table_browse_key op k_string)
         |Extend e ->
            (Hashtbl.add table_key_extended k op;
            Hashtbl.add table_extended_key op k_string)
      end
   (* given a string that represents a character, find the associated
    * curses chtype *)
   and decode_alias str =
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
   "^\\(\\(\\\\M\\\\C\\|\\\\C\\\\M\\)\\|\\(\\\\M\\)\\|\\(\\\\C\\)\\)?\\(.+\\)"
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
            make_entries mc_chtype mc_str
         else
            config_failwith ("Cannot apply \\\\M\\\\C to key \"" ^ main_key ^ "\";\n" ^
                       "octal notation might let you accomplish this.")
      else if has_meta then
         if String.length main_key = 1 then
            let m_chtype = ((int_of_char main_key.[0]) + 128) in
            let m_str = "M-" ^ main_key in
            make_entries m_chtype m_str
         else
            config_failwith ("Cannot apply \\\\M to key \"" ^ main_key ^ "\";\n" ^
                       "octal notation might let you accomplish this.")
      else if has_ctrl then
         if String.length main_key = 1 then
            let uc_main_key = String.uppercase main_key in
            let c_chtype = ((int_of_char uc_main_key.[0]) - 64) in
            let c_str = "C-" ^ uc_main_key in
            make_entries c_chtype c_str
         else
            config_failwith ("Cannot apply \\\\C to key \"" ^ main_key ^ "\";\n" ^
                       "octal notation might let you accomplish this.")
      else
         make_entries (decode_alias main_key) main_key




(* Parse a line from an rpc2 configuration file.  This operates on a stream
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
   | [< 'Kwd "bind" >] -> 
      begin 
         let bind_key key = 
            begin
               match line_stream with parser
               | [< 'Ident command >] ->
                  begin
                     match command with
                        |"function_add" ->
                           register_binding key (Function Add)
                        |"function_sub" ->
                           register_binding key (Function Sub)
                        |"function_mult" ->
                           register_binding key (Function Mult)
                        |"function_div" ->
                           register_binding key (Function Div)
                        |"function_neg" ->
                           register_binding key (Function Neg)
                        |"function_inv" ->
                           register_binding key (Function Inv)
                        |"function_pow" ->
                           register_binding key (Function Pow)
                        |"function_sq" ->
                           register_binding key (Function Sq)
                        |"function_sqrt" ->
                           register_binding key (Function Sqrt)
                        |"function_abs" ->
                           register_binding key (Function Abs)
                        |"function_arg" ->
                           register_binding key (Function Arg)
                        |"function_exp" ->
                           register_binding key (Function Exp)
                        |"function_ln" ->
                           register_binding key (Function Ln)
                        |"function_10^x" ->
                           register_binding key (Function Ten_x)
                        |"function_log10" ->
                           register_binding key (Function Log10)
                        |"function_conj" ->
                           register_binding key (Function Conj)
                        |"function_sin" ->
                           register_binding key (Function Sin)
                        |"function_cos" ->
                           register_binding key (Function Cos)
                        |"function_tan" ->
                           register_binding key (Function Tan)
                        |"function_asin" ->
                           register_binding key (Function Asin)
                        |"function_acos" ->
                           register_binding key (Function Acos)
                        |"function_atan" ->
                           register_binding key (Function Atan)
                        |"function_sinh" ->
                           register_binding key (Function Sinh)
                        |"function_cosh" ->
                           register_binding key (Function Cosh)
                        |"function_tanh" ->
                           register_binding key (Function Tanh)
                        |"function_re" ->
                           register_binding key (Function Re)
                        |"function_im" ->
                           register_binding key (Function Im)
                        |"edit_integer" ->
                           register_binding key (Edit BeginInteger)
                        |"edit_complex" ->
                           register_binding key (Edit BeginComplex)
                        |"edit_matrix" ->
                           register_binding key (Edit BeginMatrix)
                        |"edit_separator" ->
                           register_binding key (Edit Separator)
                        |"edit_minus" ->
                           register_binding key (Edit Minus)
                        |"edit_backspace" ->
                           register_binding key (Edit Backspace)
                        |"edit_enter" ->
                           register_binding key (Edit Enter)
                        |"edit_scientific_notation_base" ->
                           register_binding key (Edit SciNotBase)
                        |"command_drop" ->
                           register_binding key (Command Drop)
                        |"command_clear" ->
                           register_binding key (Command Clear)
                        |"command_swap" ->
                           register_binding key (Command Swap)
                        |"command_dup" ->
                           register_binding key (Command Dup)
                        |"command_undo" ->
                           register_binding key (Command Undo)
                        |"command_begin_browsing" ->
                           register_binding key (Command BeginBrowse)
                        |"command_begin_extended" ->
                           register_binding key (Command BeginExtended)
                        |"command_quit" ->
                           register_binding key (Command Quit)
                        |"command_rad" ->
                           register_binding key (Command SetRadians)
                        |"command_deg" ->
                           register_binding key (Command SetDegrees)
                        |"command_rect" ->
                           register_binding key (Command SetRect)
                        |"command_polar" ->
                           register_binding key (Command SetPolar)
                        |"browse_end" ->
                           register_binding key (Browse EndBrowse)
                        |"browse_scroll_left" ->
                           register_binding key (Browse ScrollLeft)
                        |"browse_scroll_right" ->
                           register_binding key (Browse ScrollRight)
                        |"browse_prev_line" ->
                           register_binding key (Browse PrevLine)
                        |"browse_next_line" ->
                           register_binding key (Browse NextLine)
                        |"browse_echo" ->
                           register_binding key (Browse Echo)
                        |"browse_rolldown" ->
                           register_binding key (Browse RollDown)
                        |"browse_rollup" ->
                           register_binding key (Browse RollUp)
                        |"extended_exit" ->
                           register_binding key (Extend ExitExtended)
                        |"extended_enter" ->
                           register_binding key (Extend EnterExtended)
                        |"extended_backspace" ->
                           register_binding key (Extend ExtBackspace)
                        |_ ->
                           config_failwith ("Unknown command name \"" ^ command ^ "\"")
                  end
               | [< >] ->
                  config_failwith ("Expected a command name after \"bind \"" ^ key ^ "\"")
            end
         in
         match line_stream with parser
         | [< 'String k >] -> 
            bind_key k
         | [< 'Ident "\\" >] ->
            begin
               match line_stream with parser
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
   | [< 'Kwd "macro" >] ->
      begin
         match line_stream with parser
         | [< 'String key >] ->
            begin
               match line_stream with parser
               | [< 'String generated_keys >] ->
                  (Printf.fprintf stderr "registering macro \"%s\" -> \"%s\"\n"
                  key generated_keys;
                  flush stderr)
               | [< >] ->
                  config_failwith ("Expected a key string after \"macro \"" ^ key ^ "\"")
            end
         | [< >] ->
            config_failwith "Expected a key string after keyword \"macro\""
      end
   | [< 'Kwd "#" >] ->
      ()
   | [< >] ->
      config_failwith "Expected a keyword at start of line";;



let process_rcfile () =
   let line_lexer line = 
      make_lexer ["bind"; "macro"; "#"] (Stream.of_string line)
   in
   let empty_regexp = Str.regexp "^[\t ]*$" in
   let config_stream = 
      try
         open_in "rpc2rc"
      with
         Sys_error error_str -> failwith "Could not find configuration file \"rpc2rc\"."
   in
   let line_num = ref 0 in
   try
      while true do
         line_num := succ !line_num;
         let line_string = input_line config_stream in
         Printf.fprintf stderr "read line %2d: %s\n" !line_num line_string;
         flush stderr;
         if Str.string_match empty_regexp line_string 0 then
            (* do nothing on an empty line *)
            ()
         else
            try
               let line_stream = line_lexer line_string in
               parse_line line_stream
            with
               |Config_failure s ->
                  (let error_str = Printf.sprintf "Syntax error on line %d of \"rpc2rc\": %s"
                  !line_num s in
                  failwith error_str)
               |Stream.Failure ->
                  failwith (Printf.sprintf "Syntax error on line %d of \"rpc2rc\"" 
                  !line_num)

      done
   with
      End_of_file ->
         close_in config_stream




(* arch-tag: DO_NOT_CHANGE_614115ed-7d1d-4834-bda4-e6cf93ac3fcd *)
