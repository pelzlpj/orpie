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

open Big_int
type rpc_data = | RpcInt of Big_int.big_int
                | RpcFloat of float
                | RpcComplex of Complex.t
                | RpcFloatMatrix of Gsl_matrix.matrix 
                | RpcComplexMatrix of Gsl_matrix_complex.matrix 


class rpc_stack =
   object(self)
      val stack = Stack.create ()

      (* generate a list of strings, one representing each stack entry *)
      method get_display_lines =
         let display_lines = ref [] and
         count = ref 0 in
         let print_el gen_el =
            begin
               count := !count + 1;
               match gen_el with
               |RpcInt el ->
                  let line = Printf.sprintf "%2d: # %sd" !count
                     (string_of_big_int el) in
                  display_lines := line :: !display_lines
               |RpcFloat el ->
                  let line = Printf.sprintf "%2d: %g" !count el in
                  display_lines := line :: !display_lines
               |RpcComplex el ->
                  let line = Printf.sprintf "%2d: (%g, %g)" !count
                  el.Complex.re el.Complex.im in
                  display_lines := line :: !display_lines
               |RpcFloatMatrix el ->
                  (* looks like [[ a11, a12 ][ a21, a22 ]] *)
                  let mat = el in
                  let rows, cols = (Gsl_matrix.dims mat) in
                  let initial_string = Printf.sprintf "%2d: [" !count in
                  let line = ref initial_string in
                  for n = 0 to rows - 1 do
                     line := !line ^ "[ ";
                     for m = 0 to cols - 2 do
                        line := !line ^ (Printf.sprintf "%g, " mat.{n, m})
                     done;
                     line := !line ^ (Printf.sprintf "%g ]" mat.{n, cols-1})
                  done;
                  line := !line ^ "]";
                  display_lines := !line :: !display_lines
               |RpcComplexMatrix el ->
                  (* looks like [[ (a11re, a11im), (a12re, a12im) ][ (a21re,
                     a21im), (a22re, a22im) ] *)
                  let mat = el in
                  let rows, cols = (Gsl_matrix_complex.dims mat) in
                  let initial_string = Printf.sprintf "%2d: [" !count in
                  let line = ref initial_string in
                  for n = 0 to rows - 1 do
                     line := !line ^ "[ ";
                     for m = 0 to cols - 2 do
                        line := !line ^ (Printf.sprintf "(%g, %g), " 
                           mat.{n, m}.Complex.re mat.{n, m}.Complex.im)
                     done;
                     line := !line ^ (Printf.sprintf "(%g, %g) ]" 
                        mat.{n, cols-1}.Complex.re mat.{n, cols-1}.Complex.im)
                  done;
                  line := !line ^ "]";
                  display_lines := !line :: !display_lines
            end
         in
         (Stack.iter print_el stack;
         !display_lines)

      method length = Stack.length stack

      method push v =
         Stack.push v stack

      method pop : rpc_data =
         Stack.pop stack

   end;;



(* arch-tag: DO_NOT_CHANGE_59b80e87-dfde-4203-a7a2-8e1f95813151 *)
