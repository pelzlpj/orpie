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
open Complex;;
open Big_int;;

type rpc_interface_screen = {stdscr:window; lines:int; cols:int;
   help_win:window; hw_lines:int; hw_cols:int; stack_win:window; sw_lines:int;
   sw_cols:int; entry_win:window; ew_lines:int; ew_cols:int};;


class rpc_interface (c : rpc_calc) (std : rpc_interface_screen) =
object(self)
   val calc = c
   val mutable scr = std
   val mutable bottom_row = 1

   method run () =
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
      getch ()



   (* display the stack, where the bottom line of the display
    * corresponds to stack level 'bottom_row' *)
   method draw_stack () =
      for line = bottom_row to pred (bottom_row + scr.sw_lines) do
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


end;;




(* arch-tag: DO_NOT_CHANGE_b4519dd2-7e94-4cbf-931a-bb5f97445cbf *)
