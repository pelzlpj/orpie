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
      end
   in
   match key_string with
   |"<esc>" ->
      make_entries 27 "<esc>"
   |"<tab>" ->
      make_entries 9 "<tab>"
   |"<enter>" ->
      make_entries Key.enter "<enter>"
   |"<return>" ->
      make_entries 10 "<return>"
   |"<insert>" ->
      make_entries Key.ic "<insert>"
   |"<delete>" ->
      make_entries Key.dc "<delete>"
   |"<home>" ->
      make_entries Key.home "<home>"
   |"<end>" ->
      make_entries Key.end_ "<end>"
   |"<pageup>" ->
      make_entries Key.ppage "<pageup>"
   |"<pagedown>" ->
      make_entries Key.npage "<pagedown>"
   |"<space>" ->
      make_entries 32 "<space>"
   |"<backspace>" ->
      make_entries Key.backspace "<backspace>"
   |"<left>" ->
      make_entries Key.left "<left>"
   |"<right>" ->
      make_entries Key.right "<right>"
   |"<up>" ->
      make_entries Key.up "<up>"
   |"<down>" ->
      make_entries Key.down "<down>"
   |"<f1>" ->
      make_entries (Key.f 1) "<f1>"
   |"<f2>" ->
      make_entries (Key.f 2) "<f2>"
   |"<f3>" ->
      make_entries (Key.f 3) "<f3>"
   |"<f4>" ->
      make_entries (Key.f 4) "<f4>"
   |"<f5>" ->
      make_entries (Key.f 5) "<f5>"
   |"<f6>" ->
      make_entries (Key.f 6) "<f6>"
   |"<f7>" ->
      make_entries (Key.f 7) "<f7>"
   |"<f8>" ->
      make_entries (Key.f 8) "<f8>"
   |"<f9>" ->
      make_entries (Key.f 9) "<f9>"
   |"<f10>" ->
      make_entries (Key.f 10) "<f10>"
   |"<f11>" ->
      make_entries (Key.f 11) "<f11>"
   |"<f12>" ->
      make_entries (Key.f 12) "<f12>"
   |_ ->
      if String.length key_string = 1 then
         make_entries (int_of_char key_string.[0]) key_string
      else if String.length key_string > 2 then
         if key_string.[0] = '\\' && key_string.[1] = 'C' then
            if String.length key_string = 3 then
               let control_chtype = ((int_of_char key_string.[2]) - 96) and
               control_str = "^" ^ (String.make 1 key_string.[2]) in
               make_entries control_chtype control_str
            else
               config_failwith ("Illegal control key \"" ^ key_string ^ "\"")
         else if key_string.[0] = '0' && key_string.[1] = 'o' then
            (* FIXME: to get the display string, one could do something like
             * let str = unctrl (ungetch key; getch ()), along with a little
             * post-processing ala curses-keys.ml *)
            let substr = String.sub key_string 2 ((String.length key_string) - 2) in
            make_entries (int_of_string key_string) ("\\" ^ substr)
         else
            config_failwith ("Unrecognized key string \"" ^ key_string ^ "\"")
      else
         config_failwith ("Unrecognized key string \"" ^ key_string ^ "\"");;




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
   let backslash_regexp = try Str.regexp "[\\]" with Failure q -> failwith "failed backslash" in
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
         let initial_string = input_line config_stream in
         Printf.fprintf stderr "initial_string = %s\n" initial_string;
         flush stderr;
         (* replace backslashes with doubled backslashes FIXME: not working *)
         let line_string = 
            try
               Str.global_replace backslash_regexp "\\\\" initial_string
            with Failure ff ->
               failwith "failed global_replace"
         in
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
