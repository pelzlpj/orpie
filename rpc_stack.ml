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

(* rpc_stack.ml -- implementation of internal calculator stack that holds
 *                 multiple data types
 *
 * The stack is implemented as a dynamically-allocated array.  This approach
 * enables non-standard stack operations such as random access and cyclic rotation of
 * elements.
 *)


exception Stack_error of string;;


open Big_int;;
open Printf;;
type rpc_data = | RpcInt of Big_int.big_int
                | RpcFloat of float
                | RpcComplex of Complex.t
                | RpcFloatMatrix of Gsl_matrix.matrix 
                | RpcComplexMatrix of Gsl_matrix_complex.matrix;;

let size_inc = 100;;

class rpc_stack =
   object(self)
      val mutable len = 0
      val mutable stack = Array.make size_inc (RpcFloat 0.0)

      method length = len

      method push v =
         (* allocate a new stack if necessary *)
         begin
            if len >= Array.length stack then
               let new_stack = Array.make ((Array.length stack) + size_inc)
               (RpcFloat 0.0) in
               Array.blit stack 0 new_stack 0 (Array.length stack);
               stack <- new_stack
            else
               ();
            (stack.(len) <- v;
            len <- len + 1)
         end

      method pop =
         (* compact stack memory by size_inc whenever we have 2 * size_inc
          * elements free *)
         begin
            (if len < (Array.length stack) - 2 * size_inc then
               let new_stack = Array.sub stack 0 ((Array.length stack) -
               size_inc) in
               stack <- new_stack
            else
               ()
            );
            if len > 0 then
               (len <- len - 1;
               stack.(len))
            else
               raise (Stack_error "cannot pop empty stack")
         end



      (* generate a string to represent a particular stack element.
       * The top stack element (stack.(len-1)) is defined to be element 
       * number 1. *)
      method get_display_line (line_num : int) (calc_mode : unit) =
         if line_num > 0 && line_num <= len then
            (* this is the actual index into the array *)
            let index = len - line_num and
            make_string gen_el =
               begin
                  match gen_el with
                  |RpcInt el ->
                     sprintf "# %s d" (string_of_big_int el)
                  |RpcFloat el ->
                     sprintf "%g" el
                  |RpcComplex el ->
                     sprintf "(%g, %g)" el.Complex.re el.Complex.im
                  |RpcFloatMatrix el ->
                     (* looks like [[ a11, a12 ][ a21, a22 ]] *)
                     let rows, cols = (Gsl_matrix.dims el) in
                     let initial_string = "[" in
                     let line = ref initial_string in
                     for n = 0 to rows - 1 do
                        line := !line ^ "[ ";
                        for m = 0 to cols - 2 do
                           line := !line ^ (sprintf "%g, " el.{n, m})
                        done;
                        line := !line ^ (sprintf "%g ]" el.{n, cols-1})
                     done;
                     line := !line ^ "]";
                     !line
                  |RpcComplexMatrix el ->
                     (* looks like [[ (a11re, a11im), (a12re, a12im) ][ (a21re,
                        a21im), (a22re, a22im) ] *)
                     let rows, cols = (Gsl_matrix_complex.dims el) in
                     let initial_string = "[" in
                     let line = ref initial_string in
                     for n = 0 to rows - 1 do
                        line := !line ^ "[ ";
                        for m = 0 to cols - 2 do
                           line := !line ^ (sprintf "(%g, %g), " 
                              el.{n, m}.Complex.re el.{n, m}.Complex.im)
                        done;
                        line := !line ^ (sprintf "(%g, %g) ]" 
                           el.{n, cols-1}.Complex.re el.{n, cols-1}.Complex.im)
                     done;
                     line := !line ^ "]";
                     !line
               end
            in
            make_string stack.(index)
         else
            raise (Stack_error ("cannot print nonexistent stack element" ^
               (string_of_int line_num)))


   end;;



(* arch-tag: DO_NOT_CHANGE_59b80e87-dfde-4203-a7a2-8e1f95813151 *)
