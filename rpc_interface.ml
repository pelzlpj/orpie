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

type rpc_interface_screen = {stdscr:window; lines:int; cols:int;
   help_win:window; hw_lines:int; hw_cols:int; stack_win:window; sw_lines:int;
   sw_cols:int; entry_win:window; ew_lines:int; ew_cols:int};;


let initialize_screen =
   let scr = initscr () in
   let height, width = get_size () in
   if width >= 80 then
      let left_win   = subwin scr (height - 2) 40 0 0 in
      let right_win  = subwin scr (height - 2) 40 0 40 in
      let bottom_win = subwin scr 2 80 (height-2) 0 in
      {stdscr = scr; lines = height; cols = width; 
      help_win = left_win; hw_lines = (height - 2); hw_cols = 40;
      stack_win = right_win; sw_lines = (height - 2); sw_cols = 40;
      entry_win = bottom_win; ew_lines = 2; ew_cols = 80}
   else
      failwith "rpc2 requires an 80 column window.";;

 
let test_screen (scr : rpc_interface_screen) =
   for i = 0 to scr.hw_lines - 1 do
      let s = sprintf "%2d (help window)" i in
      assert (mvwaddstr scr.help_win i 0 s)
   done;
   for i = 0 to scr.sw_lines - 1 do
      let s = sprintf "| %2d:(stack window)" i in
      assert (mvwaddstr scr.stack_win i 0 s)
   done;
   assert (mvwaddstr scr.entry_win 0 0
   "--------------------------------------------------------------------------------");
   assert (mvwaddstr scr.entry_win 1 0 "(entry window)");
   assert (wrefresh scr.help_win);
   assert (wrefresh scr.stack_win);
   assert (wrefresh scr.entry_win);

   fprintf stdout "getching...\n";
   let k = getch () in
   fprintf stdout "done getching.\n";;
   




(* arch-tag: DO_NOT_CHANGE_b4519dd2-7e94-4cbf-931a-bb5f97445cbf *)
