(*  rpc2 -- a stack-based RPN calculator for the console
 *  Copyright (C) 2003  Paul Pelzl
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


(* help_win is provided as an option, because it may be dropped if
 * the screen width is too small *)
type rpc_interface_screen = {stdscr:window; lines:int; cols:int;
   help_win:window option; hw_lines:int; hw_cols:int;
   stack_win:window; sw_lines:int; sw_cols:int; 
   entry_win:window; ew_lines:int; ew_cols:int};;

type rpc_interface_help_mode = | Standard | Extended;;


class rpc_interface (c : rpc_calc) (std : rpc_interface_screen) =
object(self)
   val version = "0.10"
   val calc = c
   val mutable scr = std
   val mutable stack_bottom_row = 1
   val mutable help_top_row = 1
   val mutable help_mode = Extended


   method run () =
      wclear scr.stack_win;
      wclear scr.entry_win;
      calc#enter_int (big_int_of_string "5");
      calc#enter_cmpx {Complex.re=1.5; Complex.im=2.4};
      calc#enter_cmpx {Complex.re=(-3.5); Complex.im=1.0};
      calc#enter_float 12.5;
      calc#enter_int (big_int_of_string "2");
      calc#enter_fmat (Gsl_matrix.of_array [|1.0; 2.0; 3.0; 4.0; 5.0; 6.0|] 3 2);
      calc#enter_fmat (Gsl_matrix.of_array [|7.0; 8.0; 9.0; 10.0; 11.0; 12.0|] 2 3);

      calc#enter_cmat (Gsl_matrix_complex.of_array [| {re=1.0; im=1.0}; {re=2.0;
         im=2.0}; {re=3.0; im=3.0}; {re=4.0; im=4.0}; {re=5.0; im=5.0}; {re=6.0;
         im=6.0} |] 3 2);
      calc#enter_cmat (Gsl_matrix_complex.of_array [| {re=2.3; im=3.1}; 
         {re=0.2; im=4.0}; {re=(-3.0); im=10.0}; {re=(-14.0); im=0.0} |] 2 2);
      self#draw_stack ();
      self#draw_help ();
      let k = getch () in
         Printf.printf "%d\n" k
        

   (* display the stack, where the bottom line of the display
    * corresponds to stack level 'stack_bottom_row' *)
   method draw_stack () =
      for line = stack_bottom_row to pred (stack_bottom_row + scr.sw_lines) do
         let s = calc#get_display_line line in
         let len = String.length s in
         if len > scr.sw_cols - 7 then
            (* need to truncate the string *)
            let sub_s = String.sub s 0 (scr.sw_cols - 11) in 
            let line_string = sprintf "%2d:   %s ..." line sub_s in
            assert (mvwaddstr scr.stack_win (scr.sw_lines - line) 0 line_string)
         else
            let spacer = String.make (scr.sw_cols - 7 - len) ' ' in
            let line_num = sprintf "%2d:   " line in
            let line_string = line_num ^ spacer ^ s in
            assert (mvwaddstr scr.stack_win (scr.sw_lines - line) 0 line_string)
      done;
      assert (wrefresh scr.stack_win)


   (* display the help window *)
   method draw_help () =
      let modes = calc#get_modes () in
      match scr.help_win with
      |Some win ->
         wclear win;
         wattron win WA.bold;
         let s = sprintf "rpc2 v%s  --  Press '?' for help" version in
         assert (mvwaddstr win 0 0 s);
         wattroff win WA.bold;
         assert (mvwaddstr win 1 0 "--------------------------------------");
         for i = 0 to pred scr.hw_lines do
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
         begin
            match help_mode with
            |Standard ->
               wattron win WA.bold;
               assert (mvwaddstr win 5 0 "Default Hotkeys:");
               wattroff win WA.bold;
               assert (mvwaddstr win 6 2 "<Enter> : enter number on stack");
               assert (mvwaddstr win 7 2 "      \\ : drop last stack item");
               assert (mvwaddstr win 8 2 " <PgDn> : swap last two stack items");
               assert (mvwaddstr win 9 2 "      | : clear stack");
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
               assert (wrefresh win)
            |Extended ->
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
         end;
      |None ->
         ()


end;;




(* arch-tag: DO_NOT_CHANGE_b4519dd2-7e94-4cbf-931a-bb5f97445cbf *)
