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


open Curses;;
open Printf;;
open Rpc_calc;;
open Rpc_stack;;
open Complex;;
open Big_int;;
open Operations;;
open Interface;;


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
   for line = iface.stack_bottom_row to 
   pred (iface.stack_bottom_row + iface.scr.sw_lines) do
      let s = iface.calc#get_display_line line in
      let len = String.length s in
      assert (wmove iface.scr.stack_win 
      (iface.scr.sw_lines + iface.stack_bottom_row - 1 - line) 0);
      wclrtoeol iface.scr.stack_win;
      begin
         if line = iface.stack_selection && 
         iface.interface_mode = BrowsingMode then
            wattron iface.scr.stack_win WA.reverse
         else
            ()
      end;
      begin
         if len > iface.scr.sw_cols - 7 then
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
         else
            let spacer = String.make (iface.scr.sw_cols - 7 - len) ' ' in
            let line_string = print_numbered_line line (spacer ^ s) in 
            assert (waddstr iface.scr.stack_win line_string)
      end;
      begin
         if line = iface.stack_selection && 
         iface.interface_mode = BrowsingMode then
            wattroff iface.scr.stack_win WA.reverse
         else
            ()
      end
   done;
   assert (wnoutrefresh iface.scr.stack_win);
   assert (move (iface.scr.lines - 1) (iface.scr.ew_cols - 1))


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
            let trunc_str = String.sub str (len_str - iface.scr.ew_cols + 5) 
            (iface.scr.ew_cols - 5) in
            assert (mvwaddstr iface.scr.entry_win 1 0 ("... " ^ trunc_str))
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
            "# " ^ iface.int_entry_buffer ^ " " ^ iface.int_base_string
         else
            "# " ^ iface.int_entry_buffer
      |FloatEntry ->
         let mantissa_str = iface.gen_buffer.(0).re_mantissa
         and exponent_str = iface.gen_buffer.(0).re_exponent in
         get_float_str true mantissa_str exponent_str
      |ComplexEntry ->
         let buffer = iface.gen_buffer.(0) in
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
      |FloatMatrixEntry ->
         let ss = ref "[[" in
         begin
            for el = 0 to pred iface.curr_buf do
               let temp_re = get_float_str false iface.gen_buffer.(el).re_mantissa
               iface.gen_buffer.(el).re_exponent in
               (if iface.has_multiple_rows && ((succ el) mod iface.matrix_cols) = 0 then
                  ss := !ss ^ temp_re ^ "]["
               else
                  ss := !ss ^ temp_re ^ ", ")
            done;
            let temp_re = get_float_str true iface.gen_buffer.(iface.curr_buf).re_mantissa
            iface.gen_buffer.(iface.curr_buf).re_exponent in
            ss := !ss ^ temp_re ^ "]]";
            !ss
         end
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
         !ss
   in
   begin match iface.interface_mode with
   |StandardEntryMode ->
      draw_entry_string data_string 0
   |ExtendedEntryMode ->
      let highlight_len = String.length iface.extended_entry_buffer in
      if highlight_len = 0 then
         draw_entry_string "<enter extended command>" 0
      else
         let is_function =
            match (Rcfile.translate_extended_abbrev iface.matched_extended_entry) with
            |Function ff -> true
            |_ -> false
         in
         if is_function then
            draw_entry_string (iface.matched_extended_entry ^ 
            "( )") highlight_len
         else
            draw_entry_string iface.matched_extended_entry highlight_len
   |BrowsingMode ->
      ()
   end;
   assert (move (iface.scr.lines - 1) (iface.scr.ew_cols - 1)) 


let draw_update_entry iface =
   draw_entry iface;
   assert (doupdate ())



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
   |Some win ->
      wclear win;
      wattron win WA.bold;
      let s = sprintf "Orpie v%s  --  Press '?' for help" iface.version in
      assert (mvwaddstr win 0 0 s);
      wattroff win WA.bold;
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
      let try_find fn el =
         try fn el
         with Not_found -> ""
      in
      begin
         match iface.help_mode with
         |Standard ->
            wattron win WA.bold;
            assert (mvwaddstr win 5 0 "Hotkeys:");
            wattroff win WA.bold;
            mvwaddstr_safe win 6 2 ("enter number on stack: " ^
            try_find Rcfile.key_of_edit (Edit Enter));
            mvwaddstr_safe win 7 2 ("drop last stack item:  " ^
            try_find Rcfile.key_of_command (Command Drop));
            mvwaddstr_safe win 8 2 ("swap last two items:   " ^
            try_find Rcfile.key_of_command (Command Swap));
            mvwaddstr_safe win 9 2 ("clear stack:           " ^
            try_find Rcfile.key_of_command (Command Clear));
            assert (mvwaddstr win 10 2 "+ : add               - : subtract");
            assert (mvwaddstr win 11 2 "* : multiply          / : divide");
            assert (mvwaddstr win 12 2 "^ : power (x^y)       ! : factorial");
            assert (mvwaddstr win 13 2 "% : mod");
            wattron win WA.bold;
            assert (mvwaddstr win 15 0 "Miscellaneous:");
            wattroff win WA.bold;
            assert (mvwaddstr win 16 2 "<Right> : change sign");
            assert (mvwaddstr win 17 2 "<Space> : scientific notation");
            assert (mvwaddstr win 18 2 "      ' : begin extended command");
            assert (mvwaddstr win 19 2 "   <Up> : enter stack browsing mode");
            assert (mvwaddstr win 20 2 "     ^L : refresh display");
            assert (mvwaddstr win 21 2 "      Q : quit");
            assert (wnoutrefresh win)
         |Extended ->
            if String.length iface.extended_entry_buffer = 0 then
               begin
                  wattron win WA.bold;
                  assert (mvwaddstr win 5 0 "Extended Commands:");
                  wattroff win WA.bold;
                  assert (mvwaddstr win 6 1 "Functions:");
                  assert (mvwaddstr win 7 2 "sin   asin  cos   acos  tan   atan");
                  assert (mvwaddstr win 8 2 "exp   ln    10^x  log10 sq    sqrt");
                  assert (mvwaddstr win 9 2 "sinh  asinh cosh  acosh tanh  atanh");
                  assert (mvwaddstr win 10 2 "erf   erfc  inv   gamma lngamma");
                  assert (mvwaddstr win 11 2 "real  imag  floor ceil  nearint");
                  assert (mvwaddstr win 13 1 "Change Modes:");
                  assert (mvwaddstr win 14 2 "rad   deg   bin   oct   hex   dec");
                  assert (mvwaddstr win 15 2 "rect  polar");
                  assert (mvwaddstr win 17 1 "Miscellaneous:");
                  assert (mvwaddstr win 18 2 "pi   undo  view");
                  assert (mvwaddstr win 20 1 "<Backspace> : exit extended entry");
                  assert (wnoutrefresh win)
               end
            else
               begin
                  wattron win WA.bold;
                  assert (mvwaddstr win 5 0 "Matched Extended Commands:");
                  wattroff win WA.bold;
                  let highlight_len = String.length iface.extended_entry_buffer in
                  let rec draw_matches v_pos match_list =
                     if v_pos < iface.scr.hw_lines then
                        begin match match_list with
                        |[] ->
                           ()
                        |m :: tail ->
                           begin
                              (* highlight the first 'highlight_len' characters *)
                              wattron win WA.bold;
                              let len_str = String.length m in
                              assert (mvwaddstr win v_pos 2
                                 (Str.string_before m (highlight_len)));
                              wattroff win WA.bold;
                              assert (mvwaddstr win v_pos (2 + highlight_len)
                                 (Str.string_after m (highlight_len)));
                              draw_matches (succ v_pos) tail
                           end
                        end
                     else
                        ()
                  in
                  draw_matches 7 iface.matched_extended_entry_list;
                  assert (wnoutrefresh win)
               end 
      end
   |None ->
      ()
   end;
   assert (move (iface.scr.lines - 1) (iface.scr.ew_cols - 1))


(* write an error message to the stack window *)
let draw_error (iface : interface_state_t) msg =
   draw_update_stack iface;
   let error_lines = Utility.wordwrap ("Error: " ^ msg) (iface.scr.sw_cols-2) in
   let trunc_error_lines = 
      if List.length error_lines > 4 then
         (List.nth error_lines 0) :: (List.nth error_lines 1) ::
         (List.nth error_lines 2) :: (List.nth error_lines 3) :: [] 
      else
         error_lines 
   in
   for i = 0 to pred (List.length trunc_error_lines) do
      assert (wmove iface.scr.stack_win i 0);
      wclrtoeol iface.scr.stack_win;
      assert (mvwaddstr iface.scr.stack_win i 1 (List.nth trunc_error_lines i))
   done;
   let s = String.make iface.scr.sw_cols '-' in
   assert (mvwaddstr iface.scr.stack_win (List.length trunc_error_lines) 0 s);
   assert (wnoutrefresh iface.scr.stack_win);
   assert (move (iface.scr.lines - 1) (iface.scr.ew_cols - 1))



(* arch-tag: DO_NOT_CHANGE_044cbd96-d20b-48c6-92e7-62709c2aa3df *)
