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

let cmpx_of_int i   = {Complex.re=Big_int.float_of_big_int i; Complex.im=0.0}
let cmpx_of_float f = {Complex.re=f; Complex.im=0.0}
let cmat_of_fmat fm =
   let rows, cols = Gsl_matrix.dims fm and
   f_array = Gsl_matrix.to_array fm in
   let c_array = Array.map cmpx_of_float f_array in
   Gsl_matrix_complex.of_array c_array rows cols



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




(* arch-tag: DO_NOT_CHANGE_a87790db-2dd0-496c-9620-ed968f3253fd *)
