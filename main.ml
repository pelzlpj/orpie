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

open Big_int

(* test *)
let calc = new Rpc_calc.rpc_calc;;

(* calc#enter_int (big_int_of_string
"1234567890123456789123456789123456789123456789123456789123456789");;
calc#enter_int (big_int_of_string "5");;
calc#enter_cmpx {Complex.re=1.5; Complex.im=2.4};;
calc#enter_cmpx {Complex.re=(-3.5); Complex.im=1.0};;
calc#enter_float 12.5;;
calc#enter_int (big_int_of_string "2");;
calc#enter_fmat (Gsl_matrix.of_array [|1.0; 2.0; 3.0; 4.0; 5.0; 6.0|] 3 2);;
calc#enter_fmat (Gsl_matrix.of_array [|7.0; 8.0; 9.0; 10.0; 11.0; 12.0|] 2 3);;

open Complex;;
calc#enter_cmat (Gsl_matrix_complex.of_array [| {re=1.0; im=1.0}; {re=2.0;
   im=2.0}; {re=3.0; im=3.0}; {re=4.0; im=4.0}; {re=5.0; im=5.0}; {re=6.0;
   im=6.0} |] 3 2);;
calc#enter_cmat (Gsl_matrix_complex.of_array [| {re=2.3; im=3.1}; 
   {re=0.2; im=4.0}; {re=(-3.0); im=10.0}; {re=(-14.0); im=0.0} |] 2 2);;

calc#print_stack;;
Printf.printf "\ndividing...\n\n";;
calc#div;;
calc#print_stack;;
Printf.printf "\nmultiplying...\n\n";;
calc#mult;;
calc#print_stack;;
Printf.printf "\nmultiplying...\n\n";;
calc#mult;;
calc#print_stack;;
Printf.printf "\nmultiplying...\n\n";;
calc#mult;;
calc#print_stack;;
Printf.printf "\nmultiplying...\n\n";;
calc#mult;;
calc#print_stack;;
Printf.printf "\ndropping...\n\n";;
calc#drop;;
calc#print_stack;;
Printf.printf "\nmultiplying...\n\n";;
calc#mult;;
calc#print_stack;;
Printf.printf "\nmultiplying...\n\n";;
calc#mult;;
calc#print_stack;;
Printf.printf "\nadding...\n\n";;
calc#add;;
calc#print_stack;;

*)

(* initialize curses and return a record with screen information *)
open Interface;;
open Curses;;

let initialize_screen () =
   let std = initscr () in
   assert (keypad std true);
   assert (cbreak ());
   assert (noecho ());
   let height, width = get_size () in
   if height >= 24 then 
      if width >= 80 then
         (* full two-pane window provided *)
         let left_win   = Some (subwin std (height - 2) 40 0 0) and
         right_win  = subwin std (height - 2) 40 0 40 and
         bottom_win = subwin std 2 80 (height - 2) 0 in
         {stdscr = std; lines = height; cols = width; 
         help_win = left_win; hw_lines = (height - 2); hw_cols = 40;
         stack_win = right_win; sw_lines = (height - 2); sw_cols = 40;
         entry_win = bottom_win; ew_lines = 2; ew_cols = 80}
      else if width >= 40 then
         (* only the stack window is provided *)
         let right_win = subwin std (height - 2) 40 0 0 and
         bottom_win = subwin std 2 width (height - 2) 0 in
         {stdscr = std; lines = height; cols = width; 
         help_win = None; hw_lines = 0; hw_cols = 0;
         stack_win = right_win; sw_lines = (height - 2); sw_cols = 40;
         entry_win = bottom_win; ew_lines = 2; ew_cols = 40}
      else
         (endwin ();
         failwith "Orpie requires at least a 40 column window.")
   else
      (endwin (); 
      failwith "Orpie requires at least a 24 line window.")


let iface = Interface.make calc (initialize_screen ());;
try
   Interface_main.run iface
with error ->
   endwin ();
   Printf.fprintf stderr "Caught error at toplevel: %s\n" (Printexc.to_string error);;


(* For some reason this call fails if it is moved to rpc_interface... *)
endwin ();;



(* arch-tag: DO_NOT_CHANGE_eeac13df-e93f-4359-8b70-44fefc40e225 *)
