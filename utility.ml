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

(* utility.ml
 *
 * miscellaneous helper functions that don't really fit elsewhere *)

open Curses;;


(* Word wrap a string to a width of 'cols'.  Breaks lines on whitespace; if a word is too
 * long, it will be broken and hyphenated.  Return value is a list of strings.  *)
let wordwrap s cols =
   if String.length s <= cols then
      s :: []
   else
      let rec process_words (word_list : string list) (wrapped_list : string list) =
         match word_list with
         |word :: word_tail ->
            begin
               match wrapped_list with
               |line :: line_tail ->
                  let new_line = line ^ " " ^ word in
                  if String.length new_line <= cols then
                     process_words word_tail (new_line :: line_tail)
                  else if String.length word <= cols then
                     process_words word_tail (word :: wrapped_list)
                  else
                     let len = String.length word in
                     let partial_word = (String.sub word 0 (cols-1)) ^ "-" and
                     remainder_word = "-" ^ (String.sub word (cols-1)
                     (len-cols+1)) in
                     process_words (remainder_word :: word_tail) 
                     (partial_word :: wrapped_list)
               |[] ->
                  if String.length word <= cols then
                     process_words word_tail (word :: wrapped_list)
                  else
                     let len = String.length word in
                     let partial_word = (String.sub word 0 (cols-1)) ^ "-" and
                     remainder_word = "-" ^ (String.sub word (cols-1)
                     (len-cols+1)) in
                     process_words (remainder_word :: word_tail) 
                     (partial_word :: wrapped_list)
            end
         |[] ->
            wrapped_list
      in
      let words = Str.split (Str.regexp "[ \t]+") s in
      List.rev (process_words words [])


(* Do whatever is necessary to open up a file for writing.  If it already exists,
 * open it as-is.  If it does not exist, make sure that all prefix directories
 * do exist, then open a new file. *)
let open_or_create_out_gen is_binary filename =
   (* If the filename starts with "~", substitute $HOME *)
   let expand_file =
      if Str.string_before filename 2 = "~/" then
         let homedir = Unix.getenv "HOME" in
         homedir ^ Str.string_after filename 1
      else
         filename
   in
   (* Test whether the file exists *)
   if Sys.file_exists expand_file then
      if is_binary then
         open_out_bin expand_file
      else
         open_out expand_file
   else
      (* Check whether all directories exist *)
      let dir_path = Filename.dirname expand_file in
      let dir_list = Str.split (Str.regexp "/+") dir_path in
      (* if necessary, add the first "/" to the first directory *)
      let slash_dir_list =
         if not (Filename.is_relative dir_path) then
            ("/" ^ (List.hd dir_list)) :: (List.tl dir_list)
         else
            dir_list
      in
      let rec make_directories d_list =
         match d_list with
         | [] ->
            ()
         | d :: tail ->
            begin
               try Unix.chdir d
               with Unix.Unix_error (err, msg1, msg2) ->
                  if err = Unix.ENOENT then
                     (Unix.mkdir d 493;  (* equivalent to \755 or rwxr-xr-x *)
                     Unix.chdir d)
                  else
                     raise (Unix.Unix_error (err, msg1, msg2))
            end;
            make_directories tail
      in
      make_directories slash_dir_list;
      if is_binary then
         open_out_bin (Filename.basename expand_file)
      else
         open_out (Filename.basename expand_file)


let open_or_create_out_bin filename =
   open_or_create_out_gen true filename

let open_or_create_out_ascii filename =
   open_or_create_out_gen false filename



(* open a filename, with tilde expansion *)
let expand_open_in_gen is_binary filename =
   (* If the filename starts with "~", substitute $HOME *)
   let expand_file =
      if Str.string_before filename 2 = "~/" then
         let homedir = Unix.getenv "HOME" in
         homedir ^ Str.string_after filename 1
      else
         filename
   in
   if is_binary then
      open_in_bin expand_file
   else
      open_in expand_file


let expand_open_in_bin filename =
   expand_open_in_gen true filename

let expand_open_in_ascii filename =
   expand_open_in_gen false filename



(* translate a curses key to a printable string *)
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
      Printf.sprintf "\\%.3o" ch


(* arch-tag: DO_NOT_CHANGE_a87790db-2dd0-496c-9620-ed968f3253fd *)
