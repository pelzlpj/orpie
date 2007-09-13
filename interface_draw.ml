(*  Orpie -- a fullscreen RPN calculator for the console
 *  Copyright (C) 2003-2004, 2005, 2006-2007 Paul Pelzl
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License, Version 2,
 *  as published by the Free Software Foundation.
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

(* interface_draw.ml
 * This file has all code concerned with rendering the stack, help panel,
 * and data entry line. *)


open Curses;;
open Printf;;
open Rpc_calc;;
open Rpc_stack;;
open Complex;;
open Big_int;;
open Operations;;
open Interface;;


type abbrev_help_display_t = {functions : string list; 
                              modes : string list;
                              misc : string list}

(* display the stack, where the bottom line of the display
 * corresponds to stack level 'stack_bottom_row' *)
let draw_stack (iface : interface_state_t) =
   let print_numbered_line l_num =
      let num_len = 
         String.length (string_of_int (pred (iface.stack_bottom_row +
         iface.scr.sw_lines)))
      in
      if num_len <= 2 then
         (sprintf "%2d:   %s" l_num)
      else if num_len = 3 then
         (sprintf "%3d:  %s" l_num)
      else if num_len = 4 then
         (sprintf "%4d: %s" l_num)
      else
         (* if the line number is really huge, truncate to least
          * significant digits *)
         let l_num_str = string_of_int l_num in
         let str_len = String.length l_num_str in
         let trunc_num = Str.string_after l_num_str (str_len - 4) in
         (sprintf "%s: %s" trunc_num)
   in
   (* if there is no help window, then print the calculator mode
    * information above the stack *)
   let num_stack_lines =
      begin match iface.scr.help_win with
      |Some win ->
         iface.scr.sw_lines
      |None ->
         let modes = iface.calc#get_modes () in
         assert (wmove iface.scr.stack_win 0 0);
         wclrtoeol iface.scr.stack_win;
         wattron iface.scr.stack_win WA.bold;
         assert (mvwaddstr iface.scr.stack_win 0 2 "angle:      base:      complex:");
         wattroff iface.scr.stack_win WA.bold;
         let angle_str = match modes.angle with
         |Rad -> "RAD"
         |Deg -> "DEG" in
         assert (mvwaddstr iface.scr.stack_win 0 9 angle_str);
         let base_str = match modes.base with
         |Bin -> "BIN"
         |Oct -> "OCT"
         |Hex -> "HEX"
         |Dec -> "DEC" in
         assert (mvwaddstr iface.scr.stack_win 0 20 base_str);
         let complex_str = match modes.complex with
         |Rect -> "REC"
         |Polar -> "POL" in
         assert (mvwaddstr iface.scr.stack_win 0 34 complex_str);
         assert (mvwaddstr iface.scr.stack_win 1 0 (String.make (iface.scr.sw_cols) '-'));
         iface.scr.sw_lines - 2
      end
   in
   (* display the stack data itself *)
   for line = iface.stack_bottom_row to 
   pred (iface.stack_bottom_row + num_stack_lines) do
      let s = iface.calc#get_display_line line in
      let len = String.length s in
      assert (wmove iface.scr.stack_win 
      (iface.scr.sw_lines + iface.stack_bottom_row - 1 - line) 0);
      wclrtoeol iface.scr.stack_win;
      if line = iface.stack_selection && 
      iface.interface_mode = BrowsingMode then
         wattron iface.scr.stack_win WA.reverse
      else
         ();
      if len > iface.scr.sw_cols - 7 then begin
         (* need to truncate the string *)
         let line_string =
            if line = iface.stack_selection && 
            iface.interface_mode = BrowsingMode then
               let sub_s = 
                  if iface.horiz_scroll < len - iface.scr.sw_cols + 7 then
                     String.sub s iface.horiz_scroll (iface.scr.sw_cols - 7)
                  else
                     String.sub s (len - iface.scr.sw_cols + 7) 
                     (iface.scr.sw_cols - 7)
               in
               print_numbered_line line sub_s
            else
               let sub_s = String.sub s 0 (iface.scr.sw_cols - 10) in
               print_numbered_line line (sub_s ^ "...")
         in
         assert (waddstr iface.scr.stack_win line_string)
      end else begin
         let spacer = String.make (iface.scr.sw_cols - 7 - len) ' ' in
         let line_string = print_numbered_line line (spacer ^ s) in 
         assert (waddstr iface.scr.stack_win line_string)
      end;
      if line = iface.stack_selection && 
      iface.interface_mode = BrowsingMode then
         wattroff iface.scr.stack_win WA.reverse
      else
         ();
   done;
   assert (wnoutrefresh iface.scr.stack_win);
   assert (wmove iface.scr.entry_win (iface.scr.ew_lines - 1) (iface.scr.ew_cols - 1))


let draw_update_stack iface =
   draw_stack iface;
   assert (doupdate ())




(* display the data that the user is in the process of entering *)
let draw_entry (iface : interface_state_t) =
   assert (mvwaddstr iface.scr.entry_win 0 0 (String.make iface.scr.ew_cols '-'));
   assert (wmove iface.scr.entry_win 1 0);
   wclrtoeol iface.scr.entry_win;
   (* Safely draw a string into the entry window, with "..." when
    * truncation occurs.  Highlight the first 'highlight_len'
    * characters. *)
   let draw_entry_string str highlight_len =
      let len_str = String.length str in
      begin
         if len_str > iface.scr.ew_cols - 1 then
            let trunc_str = String.sub str (len_str - iface.scr.ew_cols + 4) 
            (iface.scr.ew_cols - 4) in
            assert (mvwaddstr iface.scr.entry_win 1 0 ("..." ^ trunc_str))
         else
            if highlight_len <= len_str then
               begin
                  (* highlight the first 'highlight_len' characters *)
                  wattron iface.scr.entry_win WA.bold;
                  assert (mvwaddstr iface.scr.entry_win 1 (iface.scr.ew_cols - len_str - 1)
                     (Str.string_before str (highlight_len)));
                  wattroff iface.scr.entry_win WA.bold;
                  assert (mvwaddstr iface.scr.entry_win 1 (iface.scr.ew_cols - len_str -
                     1 + highlight_len) (Str.string_after str (highlight_len)))
               end
            else
               assert (mvwaddstr iface.scr.entry_win 1 (iface.scr.ew_cols - len_str - 1) str)
      end;
      assert (wnoutrefresh iface.scr.entry_win)
   in
   (* draw a string for a single floating-point number *)
   let get_float_str is_current mantissa exponent =
      let sign_space =
         if String.length exponent > 0 then
            match exponent.[0] with
            |'-' -> ""
            |'+' -> ""
            |_ -> " "
         else
            " "
      in
      if (is_current && iface.is_entering_exponent) || String.length exponent > 0 then
         mantissa ^ " x10^" ^ sign_space ^ exponent
      else if is_current || String.length mantissa > 0 then
         mantissa
      else 
         "0"
   in
   (* get a string representation of the data that is in the entry buffer *)
   let data_string =
      match iface.entry_type with
      |IntEntry ->
         if iface.is_entering_base then
            "# " ^ iface.int_entry_buffer ^ "`" ^ iface.int_base_string
         else
            "# " ^ iface.int_entry_buffer
      |FloatEntry ->
         let mantissa_str = iface.gen_buffer.(0).re_mantissa
         and exponent_str = iface.gen_buffer.(0).re_exponent in
         let ff = get_float_str true mantissa_str exponent_str in
         if iface.is_entering_units then
            ff ^ "_" ^ iface.units_entry_buffer
         else
            ff
      |ComplexEntry ->
         let buffer = iface.gen_buffer.(0) in
         let cc = 
            if iface.is_entering_imag then
               let temp = get_float_str false buffer.re_mantissa buffer.re_exponent in
               let re_str = 
                  if String.length temp > 0 then temp
                  else "0"
               in
               let im_str = get_float_str true buffer.im_mantissa buffer.im_exponent in
               match buffer.is_polar with
               |false ->
                  "(" ^ re_str ^ ", " ^ im_str ^ ")"
               |true ->
                  "(" ^ re_str ^ " <" ^ im_str ^ ")"
            else
               let re_str = get_float_str true buffer.re_mantissa buffer.re_exponent in
               "(" ^ re_str ^ ")"
         in
         if iface.is_entering_units then
            cc ^ "_" ^ iface.units_entry_buffer
         else
            cc
      |FloatMatrixEntry ->
         let ss = ref "[[" in
         for el = 0 to pred iface.curr_buf do
            let temp_re = get_float_str false iface.gen_buffer.(el).re_mantissa
            iface.gen_buffer.(el).re_exponent in
            if iface.has_multiple_rows && ((succ el) mod iface.matrix_cols) = 0 then
               ss := !ss ^ temp_re ^ "]["
            else
               ss := !ss ^ temp_re ^ ", "
         done;
         let temp_re = get_float_str true iface.gen_buffer.(iface.curr_buf).re_mantissa
         iface.gen_buffer.(iface.curr_buf).re_exponent in
         ss := !ss ^ temp_re ^ "]]";
         if iface.is_entering_units then
            !ss ^ "_" ^ iface.units_entry_buffer
         else
            !ss
      |ComplexMatrixEntry ->
         let ss = ref "[[" in
         for el = 0 to pred iface.curr_buf do
            let temp_re = get_float_str false iface.gen_buffer.(el).re_mantissa
            iface.gen_buffer.(el).re_exponent and
            temp_im = get_float_str false iface.gen_buffer.(el).im_mantissa
            iface.gen_buffer.(el).im_exponent in
            (if iface.has_multiple_rows && ((succ el) mod iface.matrix_cols) = 0 then
               match iface.gen_buffer.(el).is_polar with
               |false ->
                  ss := !ss ^ "(" ^ temp_re ^ ", " ^ temp_im ^ ")]["
               |true ->
                  ss := !ss ^ "(" ^ temp_re ^ " <" ^ temp_im ^ ")]["
            else
               match iface.gen_buffer.(el).is_polar with
               |false ->
                  ss := !ss ^ "(" ^ temp_re ^ ", " ^ temp_im ^ "), "
               |true ->
                  ss := !ss ^ "(" ^ temp_re ^ " <" ^ temp_im ^ "), ")
         done;
         (if iface.is_entering_imag then
            let temp_re = get_float_str false iface.gen_buffer.(iface.curr_buf).re_mantissa
            iface.gen_buffer.(iface.curr_buf).re_exponent and
            temp_im = get_float_str true iface.gen_buffer.(iface.curr_buf).im_mantissa
            iface.gen_buffer.(iface.curr_buf).im_exponent in
            match iface.gen_buffer.(iface.curr_buf).is_polar with
            |false ->
               ss := !ss ^ "(" ^ temp_re ^ ", " ^ temp_im ^ ")]]"
            |true ->
               ss := !ss ^ "(" ^ temp_re ^ " <" ^ temp_im ^ ")]]"
         else
            let temp_re = get_float_str true iface.gen_buffer.(iface.curr_buf).re_mantissa
            iface.gen_buffer.(iface.curr_buf).re_exponent in
            ss := !ss ^ "(" ^ temp_re ^ ")]]");
         if iface.is_entering_units then
            !ss ^ "_" ^ iface.units_entry_buffer
         else
            !ss
      |VarEntry ->
         "@ " ^ iface.variable_entry_buffer
   in
   begin match iface.interface_mode with
   |StandardEditMode ->
      draw_entry_string data_string 0
   |IntEditMode ->
      draw_entry_string data_string 0
   |AbbrevEditMode ->
      let first_abbrev_match =
         if iface.matched_abbrev_list = [] then ""
         else List.hd iface.matched_abbrev_list
      in
      let highlight_len = String.length iface.abbrev_entry_buffer in
      if highlight_len = 0 then
         begin match iface.abbrev_or_const with
         |IsAbbrev -> draw_entry_string "<enter command abbreviation>" 0
         |IsConst  -> draw_entry_string "<enter constant symbol>" 0
         end
      else
         begin match iface.abbrev_or_const with
         |IsAbbrev ->
            let is_function =
               match (Rcfile.translate_abbrev first_abbrev_match) with
               |Function ff -> true
               |_ -> false
            in
            if is_function then
               draw_entry_string (first_abbrev_match ^ "( )") highlight_len
            else
               draw_entry_string first_abbrev_match highlight_len
         |IsConst ->
            draw_entry_string first_abbrev_match highlight_len
         end
   |BrowsingMode ->
      ()
   |VarEditMode ->
      if String.length iface.variable_entry_buffer = 0 then
         draw_entry_string "<enter variable name>" 0
      else
         draw_entry_string data_string 0
   |UnitEditMode ->
      draw_entry_string data_string 0
   end;
   assert (wmove iface.scr.entry_win (iface.scr.ew_lines - 1) (iface.scr.ew_cols - 1))


let draw_update_entry iface =
   draw_entry iface;
   assert (doupdate ())



(* create the lists of abbreviations to display in the abbrev command
 * help screen *)
let generate_abbrev_help () =
   let rec trunc_list lst n =
      if n = 0 then
         []
      else
         match lst with
         |[] ->
            []
         |head :: tail ->
            head :: (trunc_list tail (pred n))
   in
   let get_abbr op =
      try Rcfile.abbrev_of_operation op
      with Not_found -> ""
   in
   let functions_str =
      (get_abbr (Function Sin))       ^ "  " ^ 
      (get_abbr (Function Asin))      ^ "  " ^ 
      (get_abbr (Function Cos))       ^ "  " ^ 
      (get_abbr (Function Acos))      ^ "  " ^ 
      (get_abbr (Function Tan))       ^ "  " ^ 
      (get_abbr (Function Atan))      ^ "  " ^ 
      (get_abbr (Function Exp))       ^ "  " ^ 
      (get_abbr (Function Ln))        ^ "  " ^ 
      (get_abbr (Function Ten_x))     ^ "  " ^ 
      (get_abbr (Function Log10))     ^ "  " ^ 
      (get_abbr (Function Sq))        ^ "  " ^ 
      (get_abbr (Function Sqrt))      ^ "  " ^ 
      (get_abbr (Function Inv))       ^ "  " ^ 
      (get_abbr (Function Gamma))     ^ "  " ^ 
      (get_abbr (Function LnGamma))   ^ "  " ^ 
      (get_abbr (Function Erf))       ^ "  " ^ 
      (get_abbr (Function Erfc))      ^ "  " ^ 
      (get_abbr (Function Transpose)) ^ "  " ^ 
      (get_abbr (Function Re))        ^ "  " ^ 
      (get_abbr (Function Im))        ^ "  " ^
      (get_abbr (Function Mod))       ^ "  " ^
      (get_abbr (Function Floor))     ^ "  " ^
      (get_abbr (Function Ceiling))   ^ "  " ^
      (get_abbr (Function ToInt))     ^ "  " ^
      (get_abbr (Function ToFloat))   ^ "  " ^
      (get_abbr (Function Eval))      ^ "  " ^
      (get_abbr (Function Store))     ^ "  " ^
      (get_abbr (Function Purge))
   in
   let functions_str_wrap = trunc_list 
   (Utility.wordwrap_nspace functions_str 34 2) 5 in
   let modes_str = 
      (get_abbr (Command SetRadians)) ^ "  " ^ 
      (get_abbr (Command SetDegrees)) ^ "  " ^ 
      (get_abbr (Command SetBin))     ^ "  " ^ 
      (get_abbr (Command SetOct))     ^ "  " ^ 
      (get_abbr (Command SetDec))     ^ "  " ^ 
      (get_abbr (Command SetHex))     ^ "  " ^ 
      (get_abbr (Command SetRect))    ^ "  " ^ 
      (get_abbr (Command SetPolar))
   in
   let modes_str_wrap = trunc_list 
   (Utility.wordwrap_nspace modes_str 34 2) 2 in
   let misc_str = 
      (get_abbr (Command EnterPi)) ^ "  " ^ 
      (get_abbr (Command Undo))    ^ "  " ^ 
      (get_abbr (Command View))
   in
   let misc_str_wrap = trunc_list
   (Utility.wordwrap_nspace misc_str 34 2) 1 in
   {functions = functions_str_wrap;
   modes      = modes_str_wrap;
   misc       = misc_str_wrap}



(* create the list of constants to display in the abbrev command
 * help screen *)
let generate_const_help () =
   let rec trunc_list lst n =
      if n = 0 then
         []
      else
         match lst with
         |[] ->
            []
         |head :: tail ->
            head :: (trunc_list tail (pred n))
   in
   let rec make_symbols_string symbols_list symbols_str =
      match symbols_list with
      | [] ->
         symbols_str
      | head :: tail ->
         make_symbols_string tail (head ^ "  " ^ symbols_str)
   in
   let symbols = make_symbols_string !Rcfile.constant_symbols "" in
   trunc_list (Utility.wordwrap_nspace symbols 34 2) 5



(* draw the help page in standard entry mode *)
let draw_help_standard iface win mvwaddstr_safe try_find =
   if iface.help_page = 0 then begin
      wattron win WA.bold;
      assert (mvwaddstr win 5 0 "Common Operations:");
      wattroff win WA.bold;
      mvwaddstr_safe win 6 2  ("enter    : " ^
      try_find (Edit Enter));
      mvwaddstr_safe win 7 2  ("drop     : " ^
      try_find (Command Drop));
      mvwaddstr_safe win 8 2  ("swap     : " ^
      try_find (Command Swap));
      mvwaddstr_safe win 9 2  ("backspace: " ^
      try_find (Edit Backspace));
      mvwaddstr_safe win 10 2 ("add      : " ^
      try_find (Function Add));
      mvwaddstr_safe win 11 2 ("subtract : " ^
      try_find (Function Sub));
      mvwaddstr_safe win 12 2 ("multiply : " ^
      try_find (Function Mult));
      mvwaddstr_safe win 13 2 ("divide   : " ^
      try_find (Function Div));
      mvwaddstr_safe win 14 2 ("y^x      : " ^
      try_find (Function Pow));
      mvwaddstr_safe win 15 2 ("negation : " ^
      try_find (Function Neg));
      wattron win WA.bold;
      mvwaddstr_safe win 16 0 "Miscellaneous:";
      wattroff win WA.bold;
      mvwaddstr_safe win 17 2 ("scientific notation     : " ^
      try_find (Edit SciNotBase));
      mvwaddstr_safe win 18 2 ("abbreviation entry mode : " ^
      try_find (Command BeginAbbrev));
      mvwaddstr_safe win 19 2 ("stack browsing mode     : " ^
      try_find (Command BeginBrowse));
      mvwaddstr_safe win 20 2 ("refresh display         : " ^
      try_find (Command Refresh));
      mvwaddstr_safe win 21 2 ("quit                    : " ^
      try_find (Command Quit));
      assert (wnoutrefresh win)
   end else begin
      let adjust_len s len =
         if String.length s < len then
            s ^ (String.make (len - (String.length s)) ' ')
         else
            Str.string_before s len
      in
      let make_string colon_pos key_string abbr =
         (adjust_len key_string colon_pos) ^ ": " ^ abbr
      in
      wattron win WA.bold;
      mvwaddstr_safe win 5 0 "Autobindings:";
      wattroff win WA.bold;
      if Array.length !Rcfile.autobind_keys <= 0 then
         mvwaddstr_safe win 6 2 "(none)"
      else
         for i = 0 to pred (min (iface.scr.hw_lines - 6) (Array.length
         !Rcfile.autobind_keys)) do
            let (key, key_string, bound_f, age) = !Rcfile.autobind_keys.(i) in
            let abbr = match bound_f with
               |None    -> "(none)"
               |Some op -> Rcfile.abbrev_of_operation op
            in
            mvwaddstr_safe win (i + 6) 2 (make_string 12 key_string abbr)
         done;
      assert (wnoutrefresh win)
   end


(* draw help page in integer editing mode *)
let draw_help_intedit iface win mvwaddstr_safe try_find =
   wattron win WA.bold;
   mvwaddstr_safe win 5 0 "Integer Editing Operations:";
   wattroff win WA.bold;
   mvwaddstr_safe win 6 2 ("enter    : " ^
   try_find (Edit Enter));
   mvwaddstr_safe win 7 2 ("set base : " ^
   try_find (Edit SciNotBase));
   mvwaddstr_safe win 8 2 ("cancel   : " ^
   try_find (IntEdit IntEditExit));
   assert (wnoutrefresh win)


(* draw help page in abbrev/constant entry mode *)
let draw_help_abbrev iface win mvwaddstr_safe try_find =
   if String.length iface.abbrev_entry_buffer = 0 then begin
      let abbr_strings = generate_abbrev_help () in
      let const_strings = generate_const_help () in
      let rec print_help_lines lines v_pos =
         begin match lines with
         |[] ->
            ()
         |head :: tail ->
            mvwaddstr_safe win v_pos 2 head;
            print_help_lines tail (succ v_pos)
         end
      in
      begin match iface.abbrev_or_const with
      |IsAbbrev ->
         wattron win WA.bold;
         mvwaddstr_safe win 5 0 "Abbreviations:";
         wattroff win WA.bold;
         mvwaddstr_safe win 6 1 "Common Functions:";
         print_help_lines abbr_strings.functions 7;
         mvwaddstr_safe win 13 1 "Change Modes:";
         print_help_lines abbr_strings.modes 14;
         mvwaddstr_safe win 17 1 "Miscellaneous:";
         print_help_lines abbr_strings.misc 18;
         mvwaddstr_safe win 20 1 ("execute abbreviation : " ^
         try_find (Abbrev AbbrevEnter));
         mvwaddstr_safe win 21 1 ("cancel abbreviation  : " ^
         try_find (Abbrev AbbrevExit));
      |IsConst ->
         wattron win WA.bold;
         mvwaddstr_safe win 5 0 "Constants:";
         wattroff win WA.bold;
         print_help_lines const_strings 7;
         mvwaddstr_safe win 12 1 ("enter constant : " ^
         try_find (Abbrev AbbrevEnter));
      end;
      assert (wnoutrefresh win)
   end else begin
      wattron win WA.bold;
      assert (mvwaddstr win 5 0 "Matched Abbreviations:");
      wattroff win WA.bold;
      let highlight_len = String.length iface.abbrev_entry_buffer in
      let rec draw_matches v_pos match_list =
         if v_pos < iface.scr.hw_lines then
            begin match match_list with
            |[] ->
               ()
            |m :: tail ->
               begin
                  (* highlight the first 'highlight_len' characters *)
                  wattron win WA.bold;
                  mvwaddstr_safe win v_pos 2
                     (Str.string_before m (highlight_len));
                  wattroff win WA.bold;
                  mvwaddstr_safe win v_pos (2 + highlight_len)
                     (Str.string_after m (highlight_len));
                  draw_matches (succ v_pos) tail
               end
            end
         else
            ()
      in
      draw_matches 6 iface.matched_abbrev_list;
      assert (wnoutrefresh win)
   end 


(* draw help page in variable editing mode *)
let draw_help_varedit iface win mvwaddstr_safe try_find =
   wattron win WA.bold;
   mvwaddstr_safe win 5 0 "Variable Mode Commands:";
   wattroff win WA.bold;
   mvwaddstr_safe win 6 2 ("enter variable   : " ^
   try_find (VarEdit VarEditEnter));
   mvwaddstr_safe win 7 2 ("complete variable: " ^
   try_find (VarEdit VarEditComplete));
   mvwaddstr_safe win 8 2 ("cancel entry     : " ^
   try_find (VarEdit VarEditExit));
   wattron win WA.bold;
   mvwaddstr_safe win 10 0 "Matched variables:";
   wattroff win WA.bold;
   let highlight_len = 
      begin match iface.completion with
      |None   -> String.length iface.variable_entry_buffer 
      |Some _ -> 0
      end
   in
   let rec draw_matches v_pos match_list count =
      if v_pos < iface.scr.hw_lines then
         begin match match_list with
         |[] ->
            ()
         |m :: tail ->
            begin match iface.completion with
            |None ->
               (* highlight the first 'highlight_len' characters *)
               wattron win WA.bold;
               mvwaddstr_safe win v_pos 2
                  (Str.string_before m (highlight_len));
               wattroff win WA.bold;
               mvwaddstr_safe win v_pos (2 + highlight_len)
                  (Str.string_after m (highlight_len));
            |Some num ->
               (* highlight the entire selected match *)
               if count = num then begin
                  wattron win WA.bold;
                  mvwaddstr_safe win v_pos 2 m;
                  wattroff win WA.bold;
               end else
                  mvwaddstr_safe win v_pos 2 m;
            end;
            draw_matches (succ v_pos) tail (succ count)
         end
      else
         ()
   in
   if List.length iface.matched_variables = 0 then
      mvwaddstr_safe win 11 2 "(none)"
   else
      draw_matches 11 iface.matched_variables 0;
   assert (wnoutrefresh win)


(* draw help page in stack browsing mode *)
let draw_help_browsing iface win mvwaddstr_safe try_find =
   wattron win WA.bold;
   mvwaddstr_safe win 5 0 "Browsing Operations:";
   wattroff win WA.bold;
   mvwaddstr_safe win 6 2  ("prev        : " ^
   try_find (Browse PrevLine));
   mvwaddstr_safe win 7 2  ("next        : " ^
   try_find (Browse NextLine));
   mvwaddstr_safe win 8 2  ("scroll left : " ^
   try_find (Browse ScrollLeft));
   mvwaddstr_safe win 9 2  ("scroll right: " ^
   try_find (Browse ScrollRight));
   mvwaddstr_safe win 10 2 ("roll down   : " ^
   try_find (Browse RollDown));
   mvwaddstr_safe win 11 2 ("roll up     : " ^
   try_find (Browse RollUp));
   mvwaddstr_safe win 12 2 ("dup         : " ^
   try_find (Command Dup));
   mvwaddstr_safe win 13 2 ("view        : " ^
   try_find (Browse ViewEntry));
   mvwaddstr_safe win 14 2 ("edit        : " ^
   try_find (Browse EditEntry));
   mvwaddstr_safe win 15 2 ("drop        : " ^
   try_find (Browse Drop1));
   mvwaddstr_safe win 16 2 ("dropn       : " ^
   try_find (Browse DropN));
   mvwaddstr_safe win 17 2 ("keep        : " ^
   try_find (Browse Keep));
   mvwaddstr_safe win 18 2 ("keepn       : " ^
   try_find (Browse KeepN));
   mvwaddstr_safe win 20 1 ("exit browsing mode: " ^
   try_find (Browse EndBrowse));
   assert (wnoutrefresh win)



(* display the help window *)
let draw_help (iface : interface_state_t) =
   let mvwaddstr_safe w vert horiz st =
      let st_trunc =
         if String.length st > 36 then
            Str.string_before st 36
         else
            st
      in
      assert (mvwaddstr w vert horiz st_trunc)
   in
   let modes = iface.calc#get_modes () in
   begin match iface.scr.help_win with
   |None ->
      ()
   |Some win ->
      wclear win;
      wattron win WA.bold;
      let s = sprintf "Orpie v%s" iface.version in
      mvwaddstr_safe win 0 0 s;
      wattroff win WA.bold;
      let h_pos = String.length s in
      mvwaddstr_safe win 0 (h_pos + 1) ("-- " ^ iface.tagline);
      assert (mvwaddstr win 1 0 "--------------------------------------");
      for i = 0 to pred iface.scr.hw_lines do
         assert (mvwaddch win i 38 (int_of_char '|'))
      done;
      wattron win WA.bold;
      assert (mvwaddstr win 2 0 "Calculator Modes:");
      assert (mvwaddstr win 3 2 "angle:      base:      complex:");
      wattroff win WA.bold;
      let angle_str = match modes.angle with
      |Rad -> "RAD"
      |Deg -> "DEG" in
      assert (mvwaddstr win 3 9 angle_str);
      let base_str = match modes.base with
      |Bin -> "BIN"
      |Oct -> "OCT"
      |Hex -> "HEX"
      |Dec -> "DEC" in
      assert (mvwaddstr win 3 20 base_str);
      let complex_str = match modes.complex with
      |Rect -> "REC"
      |Polar -> "POL" in
      assert (mvwaddstr win 3 34 complex_str);
      let try_find op =
         try Rcfile.key_of_operation op
         with Not_found -> "(N/A)"
      in
      begin match iface.interface_mode with
      |StandardEditMode | UnitEditMode -> 
         draw_help_standard iface win mvwaddstr_safe try_find
      |IntEditMode ->
         draw_help_intedit iface win mvwaddstr_safe try_find
      |AbbrevEditMode ->
         draw_help_abbrev iface win mvwaddstr_safe try_find
      |VarEditMode ->
         draw_help_varedit iface win mvwaddstr_safe try_find
      |BrowsingMode ->
         draw_help_browsing iface win mvwaddstr_safe try_find
      end
   end; 
   assert (wmove iface.scr.entry_win (iface.scr.ew_lines - 1) (iface.scr.ew_cols - 1))


let draw_message (iface : interface_state_t) msg =
   draw_stack iface;
   let error_lines = Utility.wordwrap msg (iface.scr.sw_cols-2) in
   let trunc_error_lines = 
      if List.length error_lines > 4 then
         (List.nth error_lines 0) :: (List.nth error_lines 1) ::
         (List.nth error_lines 2) :: (List.nth error_lines 3) :: [] 
      else
         error_lines 
   in
   let top_line =
      begin match iface.scr.help_win with
      |Some win -> 0
      |None     -> 2
      end
   in
   for i = 0 to pred (List.length trunc_error_lines) do
      assert (wmove iface.scr.stack_win (i + top_line) 0);
      wclrtoeol iface.scr.stack_win;
      assert (mvwaddstr iface.scr.stack_win (i + top_line) 1 (List.nth trunc_error_lines i))
   done;
   let s = String.make iface.scr.sw_cols '-' in
   assert (mvwaddstr iface.scr.stack_win ((List.length trunc_error_lines) +
   top_line) 0 s);
   assert (wnoutrefresh iface.scr.stack_win);
   assert (wmove iface.scr.entry_win (iface.scr.ew_lines - 1) (iface.scr.ew_cols - 1))


(* write an error message to the stack window *)
let draw_error (iface : interface_state_t) msg =
   draw_message iface ("Error: " ^ msg)


(* display the "about" screen *)
let draw_about (iface : interface_state_t) =
   erase ();
   (* draw the box outline *)
   let horiz_line = String.make iface.scr.cols '*' in
   let vert_line_piece = String.make iface.scr.cols ' ' in
   vert_line_piece.[0] <- '*';
   vert_line_piece.[pred iface.scr.cols] <- '*';
   assert (mvaddstr 0 0 horiz_line);
   assert (mvaddstr (iface.scr.lines - 2) 0 horiz_line);
   for i = 1 to iface.scr.lines - 3 do
      assert (mvaddstr i 0 vert_line_piece)
   done;
   (* draw the text *)
   let vert_center  = (iface.scr.lines - 2) / 2
   and horiz_center = iface.scr.cols / 2 in
   let left_shift = 17 in
   attron A.bold;
   assert (mvaddstr (vert_center - 6) (horiz_center - left_shift) 
   ("Orpie v" ^ iface.version));
   attroff A.bold;
   assert (mvaddstr (vert_center - 5) (horiz_center - left_shift) 
   "Copyright (C) 2004 Paul Pelzl");
   assert (mvaddstr (vert_center - 3) (horiz_center - left_shift)
   "\"Because, frankly, GUI calculator");
   assert (mvaddstr (vert_center - 2) (horiz_center - left_shift) 
   " programs are pure evil.  ");
   attron A.bold;
   assert (mvaddstr (vert_center - 2) (horiz_center - left_shift + 26) 
   "Orpie");
   attroff A.bold;
   assert (mvaddstr (vert_center - 2) (horiz_center - left_shift + 31) 
   ", on");
   assert (mvaddstr (vert_center - 1) (horiz_center - left_shift)
   " the other hand, is only a little");
   assert (mvaddstr (vert_center + 0) (horiz_center - left_shift)
   " bit evil.\"");
   assert (mvaddstr (vert_center + 2) (horiz_center - left_shift)
   "Orpie comes with ABSOLUTELY NO");
   assert (mvaddstr (vert_center + 3) (horiz_center - left_shift)
   "WARRANTY.  This is free software,");
   assert (mvaddstr (vert_center + 4) (horiz_center - left_shift)
   "and you are welcome to redistribute");
   assert (mvaddstr (vert_center + 5) (horiz_center - left_shift)
   "it under certain conditions; see");
   assert (mvaddstr (vert_center + 6) (horiz_center - left_shift)
   "'COPYING' for details.");
   assert (mvaddstr (iface.scr.lines - 4) (horiz_center - 12)
   "Press any key to continue.");
   assert (move (iface.scr.lines - 1) (iface.scr.cols - 1));
   assert (refresh ())



(* arch-tag: DO_NOT_CHANGE_044cbd96-d20b-48c6-92e7-62709c2aa3df *)
