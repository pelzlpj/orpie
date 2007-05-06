(*  Orpie -- a fullscreen RPN calculator for the console
 *  Copyright (C) 2003-2004, 2005, 2006-2007 Paul Pelzl
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
 *  <pelzlpj@eecs.umich.edu>.
 *)

(* utility.ml
 *
 * miscellaneous helper functions that don't really fit elsewhere *)

(* for some reason this is unrecognized if I leave it in txtin_parser.mly *)
exception Txtin_error of string

(* Word wrap a string to a width of 'cols', placing 'num_spaces' spaces between
 * the words.  Breaks lines on whitespace; if a word is too long, it will be
 * broken and hyphenated.  Return value is a list of strings.  *)
let wordwrap_nspace s cols num_spaces =
   let space_str = String.make num_spaces ' ' in
   if String.length s <= cols then
      s :: []
   else
      let rec process_words (word_list : string list) (wrapped_list : string list) =
         match word_list with
         |word :: word_tail ->
            begin
               match wrapped_list with
               |line :: line_tail ->
                  let new_line = line ^ space_str ^ word in
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


(* wordwrap with single space *)
let wordwrap s cols = wordwrap_nspace s cols 1

(* append a file to a directory, with the proper number
 * of slashes *)
let join_path dirname filename =
   let dir_last   = dirname.[String.length dirname - 1]
   and file_first = filename.[0] in
   if dir_last = '/' && file_first = '/' then
      dirname ^ (Str.string_after filename 1)
   else if dir_last <> '/' && file_first <> '/' then
      dirname ^ "/" ^ filename
   else
      dirname ^ filename


(* If the filename starts with "~", substitute $HOME *)
let expand_file filename =
   if Str.string_before filename 2 = "~/" then
      let homedir = Sys.getenv "HOME" in
      homedir ^ Str.string_after filename 1
   else
      filename


(* Do whatever is necessary to open up a file for writing.  If it already exists,
 * open it as-is.  If it does not exist, make sure that all prefix directories
 * do exist, then open a new file. *)
let open_or_create_out_gen is_binary filename =
   let exp_file = expand_file filename in
   (* Test whether the file exists *)
   if Sys.file_exists exp_file then
      if is_binary then
         open_out_bin exp_file
      else
         open_out exp_file
   else
      (* Check whether all directories exist *)
      let dir_path = Filename.dirname exp_file in
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
               try Sys.chdir d
               with Sys_error err_msg ->
                  begin 
                     let _ = Sys.command ("mkdir " ^ d) in
                     Sys.chdir d
                  end
            end;
            make_directories tail
      in
      make_directories slash_dir_list;
      if is_binary then
         open_out_bin (Filename.basename exp_file)
      else
         open_out (Filename.basename exp_file)


let open_or_create_out_bin filename =
   open_or_create_out_gen true filename

let open_or_create_out_ascii filename =
   open_or_create_out_gen false filename



(* open a filename, with tilde expansion *)
let expand_open_in_gen is_binary filename =
   (* If the filename starts with "~", substitute $HOME *)
   if is_binary then
      open_in_bin (expand_file filename)
   else
      open_in (expand_file filename)


let expand_open_in_bin filename =
   expand_open_in_gen true filename

let expand_open_in_ascii filename =
   expand_open_in_gen false filename




(* arch-tag: DO_NOT_CHANGE_a87790db-2dd0-496c-9620-ed968f3253fd *)
