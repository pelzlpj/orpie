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
open Rpc_calc;;
open Operations;;
open Complex;;

exception Not_handled;;

(* help_win is provided as an option, because it may be dropped if
 * the screen width is too small *)
type screen_t = {stdscr:window; mutable lines:int; mutable cols:int;
   mutable help_win:window option; mutable hw_lines:int; mutable hw_cols:int;
   mutable stack_win:window; mutable sw_lines:int; mutable sw_cols:int; 
   mutable entry_win:window; mutable ew_lines:int; mutable ew_cols:int};;

type help_mode_t = | Standard | StandardInt | Extended;;
type entry_t     = | IntEntry | FloatEntry | ComplexEntry 
                   | FloatMatrixEntry | ComplexMatrixEntry;;

type interface_mode_t = | StandardEntryMode | IntEditMode | ExtendedEntryMode | BrowsingMode;;

type complex_entry_element_t = 
   {mutable re_mantissa : string; mutable re_exponent : string;
    mutable im_mantissa : string; mutable im_exponent : string; 
    mutable is_polar : bool};;


let max_matrix_size = 1000;;

let all_taglines = [| "RPN for the masses";
                      "'=' is for the weak";
                      "swap drop dup view";
                      "I hate the mouse";
                      "now w/ 800% more stack";
                      "powered by Ocaml";
                      "compute _this_";
                      "interface as art";
                      "kick that data's ass";
                      "Nice.";
                      "configurability is key";
                      ":wq";
                      "the \"Mutt\" of calcs"|];


(* everything you need to know about the interface state goes in this variable *)
type interface_state_t =
   {version                            : string;                        (* program version string *)
   tagline                             : string;
   calc                                : rpc_calc;
   mutable scr                         : screen_t;                      (* curses screen with two or three subwindows *)
   mutable run_calc                    : bool;                          (* exit when run_true becomes false *)
   mutable stack_bottom_row            : int;                           (* controls what portion of the stack is viewable *)
   mutable stack_selection             : int;                           (* in stack browsing mode, this item is selected *)
   mutable interface_mode              : interface_mode_t;              (* standard mode or stack browsing mode *)
   mutable horiz_scroll                : int;                           (* controls how far an element is scrolled left/right *)
   mutable help_mode                   : help_mode_t;                   (* controls the mode of context-sensitive help *)
   mutable has_entry                   : bool;                          (* whether or not the entry buffer has anything in it *)
   mutable entry_type                  : entry_t;                       (* the current type of data being entered *)
   mutable int_entry_buffer            : string;                        (* holds characters entered for int data type *)
   mutable is_entering_base            : bool;                          (* is the user is entering a base *)
   mutable int_base_string             : string;                        (* one-character representation of the base *)
   mutable is_entering_exponent        : bool;                          (* is the user entering a scientific notation exponent *)
   mutable extended_entry_buffer       : string;                        (* stores characters entered in extended entry mode *)
   mutable matched_extended_entry      : string;                        (* stores the command-completed extended entry *)
   mutable matched_extended_entry_list : string list;                   (* stores the list of all possible command completions *)
   gen_buffer                          : complex_entry_element_t array; (* storage for floating-point (array)-based types *)
   mutable curr_buf                    : int;                           (* which element of gen_buffer is being edited *)
   mutable is_entering_imag            : bool;                          (* is the imaginary component being edited *)
   mutable matrix_cols                 : int;                           (* how many cols in the matrix being entered *)
   mutable has_multiple_rows           : bool}                          (* does the matrix being entered have >1 row *)
   


(* create and initialize an interface with default settings *)
let make (c : rpc_calc) (std : screen_t) =
   Random.self_init ();
   let tagline_index = Random.int (Array.length all_taglines) in
   let iface =
      {version = "1.0";
      tagline = all_taglines.(tagline_index);
      calc = c;
      scr = std;
      run_calc = true;
      stack_bottom_row = 1;
      stack_selection = 1;
      interface_mode = StandardEntryMode;
      horiz_scroll = 0;
      help_mode = Standard;
      has_entry = false;
      entry_type = FloatEntry;
      int_entry_buffer = "";
      is_entering_base = false;
      int_base_string = "";
      is_entering_exponent = false;
      extended_entry_buffer = "";
      matched_extended_entry = "";
      matched_extended_entry_list = [];
      gen_buffer = Array.make max_matrix_size
         {re_mantissa = ""; re_exponent = "";
         im_mantissa = ""; im_exponent = ""; is_polar = false};
      curr_buf = 0;
      is_entering_imag = false;
      matrix_cols = 1;
      has_multiple_rows = false}
   in
   iface
                                               



(* arch-tag: DO_NOT_CHANGE_2e912989-cdb2-498a-9bb3-b6d76e94f3a5 *)
