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


begin
   let std = initscr () in
   assert (keypad std true);
   assert (cbreak ());
   assert (noecho ());
   assert (curs_set 0);
   assert (mvaddstr 1 0 "Type some keys to see the corresponding octal and string representations:");
   assert (refresh ())
end;
let string_of_chtype ch =
   let display_string_of_keyname str =
      match str with
      |"KEY_LEFT" -> "<left>"
      |"KEY_RIGHT" -> "<right>"
      |"KEY_UP" -> "<up>"
      |"KEY_DOWN" -> "<down>"
      |"KEY_BACKSPACE" -> "<backspace>"
      |"KEY_IC" -> "<insert>"
      |"KEY_DC" -> "<delete>"
      |"KEY_HOME" -> "<home>"
      |"KEY_END" -> "<end>"
      |"KEY_PPAGE" -> "<pageup>"
      |"KEY_NPAGE" -> "<pagedown>"
      |" " -> "<space>"
      |"KEY_F(1)" -> "<f1>"
      |"KEY_F(2)" -> "<f2>"
      |"KEY_F(3)" -> "<f3>"
      |"KEY_F(4)" -> "<f4>"
      |"KEY_F(5)" -> "<f5>"
      |"KEY_F(6)" -> "<f6>"
      |"KEY_F(7)" -> "<f7>"
      |"KEY_F(8)" -> "<f8>"
      |"KEY_F(9)" -> "<f9>"
      |"KEY_F(10)" -> "<f10>"
      |"KEY_F(11)" -> "<f11>"
      |"KEY_F(12)" -> "<f12>"
      |"KEY_ENTER" -> "<enter>"
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
      "(unmatched string; use octal notation for this keypress)"
in
while true do
   let k = getch () in
   let s1 = Printf.sprintf " octal: \\%.3o" k and
   s2 = Printf.sprintf     "string: \"%s\"" (string_of_chtype k) in
   (assert (move 2 0);
   clrtoeol ();
   assert (addstr s1);
   assert (move 3 0);
   clrtoeol ();
   assert (addstr s2);
   assert (refresh ()))
done;
endwin ();;




(* arch-tag: DO_NOT_CHANGE_31cbf03e-49f9-4f66-844c-ceb584edb920 *)
