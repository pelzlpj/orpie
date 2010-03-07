(*  Orpie -- a fullscreen RPN calculator for the console
 *  Copyright (C) 2003-2004, 2005, 2006-2007, 2010 Paul Pelzl
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
 *  <pelzlpj@gmail.com>.
 *)

(* interface.ml
 * This file defines the data structure (a record) that stores the
 * interface state.  The state includes information on the curses screen,
 * the current editing mode, the data stored in entry string buffers, etc. *)


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

type entry_t     = | IntEntry | FloatEntry | ComplexEntry 
                   | FloatMatrixEntry | ComplexMatrixEntry | VarEntry;;

type interface_mode_t = | StandardEditMode | IntEditMode | AbbrevEditMode 
                        | VarEditMode | UnitEditMode | BrowsingMode;;

type abbrev_const_t = IsAbbrev | IsConst;;

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
                      "powered by OCaml";
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
   mutable help_page                   : int;                           (* which help page is being viewed *)
   mutable has_entry                   : bool;                          (* whether or not the entry buffer has anything in it *)
   mutable entry_type                  : entry_t;                       (* the current type of data being entered *)
   mutable int_entry_buffer            : string;                        (* holds characters entered for int data type *)
   mutable is_entering_base            : bool;                          (* is the user is entering a base *)
   mutable int_base_string             : string;                        (* one-character representation of the base *)
   mutable is_entering_exponent        : bool;                          (* is the user entering a scientific notation exponent *)
   mutable abbrev_entry_buffer         : string;                        (* stores characters entered in abbrev entry mode *)
   mutable matched_abbrev_list         : string list;                   (* stores the list of all possible command completions *)
   gen_buffer                          : complex_entry_element_t array; (* storage for floating-point (array)-based types *)
   mutable abbrev_or_const             : abbrev_const_t;                (* in AbbrevEntryMode, are we entering an abbrev or a constant? *)
   mutable variable_entry_buffer       : string;                        (* stores characters entered in variable entry mode *)
   mutable variable_entry_buffer_back  : string;                        (* used in variable completion *)
   mutable matched_variables           : string list;                   (* stores the list of all matching variable completions *)
   mutable sorted_variables            : string list;                   (* stores an alphabetically sorted list of all variables *)
   mutable completion                  : int option;                    (* which one of the list elements to complete variables with *)
   mutable curr_buf                    : int;                           (* which element of gen_buffer is being edited *)
   mutable is_entering_imag            : bool;                          (* is the imaginary component being edited *)
   mutable matrix_cols                 : int;                           (* how many cols in the matrix being entered *)
   mutable has_multiple_rows           : bool;                          (* does the matrix being entered have >1 row *)
   mutable units_entry_buffer          : string;                        (* stores unit characters entered *)
   mutable is_entering_units           : bool}                          (* is the user appending units *)
   


(* create and initialize an interface with default settings *)
let make (c : rpc_calc) (std : screen_t) =
   Random.self_init ();
   let tagline_index = Random.int (Array.length all_taglines) in
   let iface = {
      version = Version.version;
      tagline = all_taglines.(tagline_index);
      calc = c;
      scr = std;
      run_calc = true;
      stack_bottom_row = 1;
      stack_selection = 1;
      interface_mode = StandardEditMode;
      horiz_scroll = 0;
      help_page = 0;
      has_entry = false;
      entry_type = FloatEntry;
      int_entry_buffer = "";
      is_entering_base = false;
      int_base_string = "";
      is_entering_exponent = false;
      abbrev_entry_buffer = "";
      matched_abbrev_list = [];
      abbrev_or_const = IsAbbrev;
      variable_entry_buffer = "";
      variable_entry_buffer_back = "";
      matched_variables = [];
      sorted_variables = [];
      completion = None;
      gen_buffer = Array.make max_matrix_size
         {re_mantissa = ""; re_exponent = "";
         im_mantissa = ""; im_exponent = ""; is_polar = false};
      curr_buf = 0;
      is_entering_imag = false;
      matrix_cols = 1;
      has_multiple_rows = false;
      units_entry_buffer = "";
      is_entering_units = false
   } in 
   iface
                                               



(* arch-tag: DO_NOT_CHANGE_2e912989-cdb2-498a-9bb3-b6d76e94f3a5 *)
