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

open Rpc_stack
open Utility
open Big_int

let add (stack : rpc_stack) (do_backup : unit -> unit) =
   if stack#length > 1 then
      begin
         do_backup ();
         let gen_el2 = stack#pop () in
         let gen_el1 = stack#pop () in
         match gen_el1 with 
         |RpcInt el1 -> (
            match gen_el2 with 
            |RpcInt el2 ->
               stack#push (RpcInt (add_big_int el1 el2))
            |RpcFloat el2 ->
               stack#push (RpcFloat ((float_of_big_int el1) +. el2))
            |RpcComplex el2 ->
               let c_el1 = cmpx_of_int el1 in
               stack#push (RpcComplex (Complex.add c_el1 el2))
            |_ ->
               (* if the elements are incompatible, we have to
                  put them back on the stack *)
               (stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "incompatible types for addition"))
            )
         |RpcFloat el1 -> (
            match gen_el2 with 
            |RpcInt el2 ->
               stack#push (RpcFloat (el1 +. float_of_big_int el2))
            |RpcFloat el2 ->
               stack#push (RpcFloat (el1 +. el2))
            |RpcComplex el2 ->
               let c_el1 = cmpx_of_float el1 in
               stack#push (RpcComplex (Complex.add c_el1 el2))
            |_ ->
               (* if the elements are incompatible, we have to
                  put them back on the stack *)
               (stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "incompatible types for addition"))
            )
         |RpcComplex el1 -> (
            match gen_el2 with
            |RpcInt el2 ->
               let c_el2 = cmpx_of_int el2 in
               stack#push (RpcComplex (Complex.add el1 c_el2))
            |RpcFloat el2 ->
               let c_el2 = cmpx_of_float el2 in
               stack#push (RpcComplex (Complex.add el1 c_el2))
            |RpcComplex el2 ->
               stack#push (RpcComplex (Complex.add el1 el2))
            |_ ->
               (* if the elements are incompatible, we have to
                  put them back on the stack *)
               (stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "incompatible types for addition"))
            )
         |RpcFloatMatrix el1 -> (
            match gen_el2 with
            |RpcFloatMatrix el2 ->
               let dim1 = (Gsl_matrix.dims el1) and
               dim2     = (Gsl_matrix.dims el2) in
               if dim1 = dim2 then
                  let result = Gsl_matrix.copy el1 in
                  (Gsl_matrix.add result el2;
                  stack#push (RpcFloatMatrix result))
               else
                  (* clean up *)
                  (stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "incompatible matrix dimensions for addition"))
            |RpcComplexMatrix el2 ->
               let dim1 = (Gsl_matrix.dims el1) and
               dim2     = (Gsl_matrix_complex.dims el2) in
               if dim1 = dim2 then
                  let c_el1 = cmat_of_fmat el1 in
                  (Gsl_matrix_complex.add c_el1 el2;
                  stack#push (RpcComplexMatrix c_el1))
               else
                  (stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "incompatible matrix dimensions for addition"))
            |_ ->
               (* if the elements are incompatible, we have to
                  put them back on the stack *)
               (stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "incompatible types for addition"))
            )
         |RpcComplexMatrix el1 -> (
            match gen_el2 with
            |RpcFloatMatrix el2 ->
               let dim1 = (Gsl_matrix_complex.dims el1) and
               dim2     = (Gsl_matrix.dims el2) in
               if dim1 = dim2 then
                  let c_el2 = cmat_of_fmat el2 in
                  let copy = Gsl_matrix_complex.copy el1 in
                  (Gsl_matrix_complex.add copy c_el2;
                  stack#push (RpcComplexMatrix copy))
               else
                  (stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "incompatible matrix dimensions for addition"))
            |RpcComplexMatrix el2 ->
               let dim1 = (Gsl_matrix_complex.dims el1) and
               dim2     = (Gsl_matrix_complex.dims el2) in
               if dim1 = dim2 then
                  let copy = Gsl_matrix_complex.copy el1 in
                  (Gsl_matrix_complex.add copy el2;
                  stack#push (RpcComplexMatrix copy))
               else
                  (stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "incompatible matrix dimensions for addition"))
            |_ ->
               (* if the elements are incompatible, we have to
                  put them back on the stack *)
               (stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "incompatible types for addition"))
            )
      end
   else
      raise (Invalid_argument "insufficient arguments for addition")

(* arch-tag: DO_NOT_CHANGE_f59cab0f-755e-4d09-9d30-114114945b38 *)
