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

(* curses_assist.ml
 * Miscellaneous helper functions functions for dealing with the
 * Curses module *)


open Curses

(* translate a curses key to a printable string *)
let string_of_chtype ch =
   let display_string_of_keyname str =
      match str with
      |"KEY_LEFT"      -> "<left>"
      |"KEY_RIGHT"     -> "<right>"
      |"KEY_UP"        -> "<up>"
      |"KEY_DOWN"      -> "<down>"
      |"KEY_BACKSPACE" -> "<backspace>"
      |"KEY_IC"        -> "<insert>"
      |"KEY_DC"        -> "<delete>"
      |"KEY_HOME"      -> "<home>"
      |"KEY_END"       -> "<end>"
      |"KEY_PPAGE"     -> "<pageup>"
      |"KEY_NPAGE"     -> "<pagedown>"
      |" "             -> "<space>"
      |"KEY_F(1)"      -> "<f1>"
      |"KEY_F(2)"      -> "<f2>"
      |"KEY_F(3)"      -> "<f3>"
      |"KEY_F(4)"      -> "<f4>"
      |"KEY_F(5)"      -> "<f5>"
      |"KEY_F(6)"      -> "<f6>"
      |"KEY_F(7)"      -> "<f7>"
      |"KEY_F(8)"      -> "<f8>"
      |"KEY_F(9)"      -> "<f9>"
      |"KEY_F(10)"     -> "<f10>"
      |"KEY_F(11)"     -> "<f11>"
      |"KEY_F(12)"     -> "<f12>"
      |"KEY_ENTER"     -> "<enter>"
      |"\\" -> "\\\\"
      |_ -> str
   in
   (* regexp to check for meta and/or ctrl prefixes
    * matches either "M-^" or "M-" or "^" followed by some character string *)
   let mc_re = Str.regexp "^\\(\\(M-\\^\\)\\|\\(M-\\)\\|\\(\\^\\)\\)?\\(.+\\)" 
   and key_string = (keyname ch) in
   if Str.string_match mc_re key_string 0 then
      let has_meta_ctrl = 
         try let _ = Str.matched_group 2 key_string in true
         with Not_found -> false
      and has_meta =
         try let _ = Str.matched_group 3 key_string in true
         with Not_found -> false
      and has_ctrl = 
         try let _ = Str.matched_group 4 key_string in true
         with Not_found -> false
      and main_key = Str.matched_group 5 key_string in
      if has_meta_ctrl then
         "\\\\M\\\\C" ^ (display_string_of_keyname main_key)
      else if has_meta then
         "\\\\M" ^ (display_string_of_keyname main_key)
      else if has_ctrl then
         if main_key = "J" then
            "<return>"
         else if main_key = "I" then
            "<tab>"
         else
            "\\\\C" ^ (display_string_of_keyname main_key)
      else
         display_string_of_keyname main_key
   else
      Printf.sprintf "\\%.3o" ch




(* arch-tag: DO_NOT_CHANGE_833e814a-273c-4faa-b6d0-123eca3c608d *)
