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
type screen_t = {stdscr:window; lines:int; cols:int;
   help_win:window option; hw_lines:int; hw_cols:int;
   stack_win:window; sw_lines:int; sw_cols:int; 
   entry_win:window; ew_lines:int; ew_cols:int};;

type help_mode_t = | Standard | Extended;;
type entry_t     = | IntEntry | FloatEntry | ComplexEntry 
                   | FloatMatrixEntry | ComplexMatrixEntry;;

type interface_mode_t = | StandardEntryMode | ExtendedEntryMode | BrowsingMode;;

type complex_entry_element_t = 
   {mutable re_mantissa : string; mutable re_exponent : string;
    mutable im_mantissa : string; mutable im_exponent : string; 
    mutable is_polar : bool};;

let extended_commands =
   ("add\nsub\nmult\ndiv\nneg\ninv\npow\nsq\nsqrt\nabs\narg\nexp\nln\n" ^
    "10^\nlog10\nconj\nsin\ncos\ntan\nasin\nacos\natan\nsinh\ncosh\ntanh\n" ^
    "re\nim\ndrop\nclear\nswap\ndup\nundo\nquit\nrad\ndeg\nrect\npolar\n" ^
    "bin\noct\ndec\nhex\nview\nabout\nrefresh\npi");;

(* abbreviations used in extended entry mode *)
let command_abbrev_table = Hashtbl.create 30;;
Hashtbl.add command_abbrev_table "add" (Function Add);;
Hashtbl.add command_abbrev_table "sub" (Function Sub);;
Hashtbl.add command_abbrev_table "mult" (Function Mult);;
Hashtbl.add command_abbrev_table "div" (Function Div);;
Hashtbl.add command_abbrev_table "neg" (Function Neg);;
Hashtbl.add command_abbrev_table "inv" (Function Inv);;
Hashtbl.add command_abbrev_table "pow" (Function Pow);;
Hashtbl.add command_abbrev_table "sq" (Function Sq);;
Hashtbl.add command_abbrev_table "sqrt" (Function Sqrt);;
Hashtbl.add command_abbrev_table "abs" (Function Abs);;
Hashtbl.add command_abbrev_table "arg" (Function Arg);;
Hashtbl.add command_abbrev_table "exp" (Function Exp);;
Hashtbl.add command_abbrev_table "ln" (Function Ln);;
Hashtbl.add command_abbrev_table "10^" (Function Ten_x);;
Hashtbl.add command_abbrev_table "log10" (Function Log10);;
Hashtbl.add command_abbrev_table "conj" (Function Conj);;
Hashtbl.add command_abbrev_table "sin" (Function Sin);;
Hashtbl.add command_abbrev_table "cos" (Function Cos);;
Hashtbl.add command_abbrev_table "tan" (Function Tan);;
Hashtbl.add command_abbrev_table "sinh" (Function Sinh);;
Hashtbl.add command_abbrev_table "cosh" (Function Cosh);;
Hashtbl.add command_abbrev_table "tanh" (Function Tanh);;
Hashtbl.add command_abbrev_table "asin" (Function Asin);;
Hashtbl.add command_abbrev_table "acos" (Function Acos);;
Hashtbl.add command_abbrev_table "atan" (Function Atan);;
Hashtbl.add command_abbrev_table "re" (Function Re);;
Hashtbl.add command_abbrev_table "im" (Function Im);;
Hashtbl.add command_abbrev_table "drop" (Command Drop);;
Hashtbl.add command_abbrev_table "clear" (Command Clear);;
Hashtbl.add command_abbrev_table "swap" (Command Swap);;
Hashtbl.add command_abbrev_table "dup" (Command Dup);;
Hashtbl.add command_abbrev_table "undo" (Command Undo);;
Hashtbl.add command_abbrev_table "quit" (Command Quit);;
Hashtbl.add command_abbrev_table "rad" (Command SetRadians);;
Hashtbl.add command_abbrev_table "deg" (Command SetDegrees);;
Hashtbl.add command_abbrev_table "rect" (Command SetRect);;
Hashtbl.add command_abbrev_table "polar" (Command SetPolar);;
Hashtbl.add command_abbrev_table "bin" (Command SetBin);;
Hashtbl.add command_abbrev_table "oct" (Command SetOct);;
Hashtbl.add command_abbrev_table "dec" (Command SetDec);;
Hashtbl.add command_abbrev_table "hex" (Command SetHex);;
Hashtbl.add command_abbrev_table "view" (Command View);;
Hashtbl.add command_abbrev_table "about" (Command About);;
Hashtbl.add command_abbrev_table "refresh" (Command Refresh);;
Hashtbl.add command_abbrev_table "pi" (Command EnterPi);;
let translate_extended_abbrev abb =
   Hashtbl.find command_abbrev_table abb;;

let max_matrix_size = 1000;;

(* everything you need to know about the interface state goes in this variable *)
type interface_state_t =
   {version                            : string;                        (* program version string *)
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
   let iface =
      {version = "0.10";
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
