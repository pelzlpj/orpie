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

let mult (stack : rpc_stack) (do_backup : unit -> unit) =
   if stack#length > 1 then
      begin
         do_backup ();
         let gen_el2 = stack#pop () in
         let gen_el1 = stack#pop () in
         match gen_el1 with
         |RpcInt el1 -> (
            match gen_el2 with
            |RpcInt el2 ->
               stack#push (RpcInt (mult_big_int el1 el2))
            |RpcFloat el2 ->
               stack#push (RpcFloat ((float_of_big_int el1) *. el2))
            |RpcComplex el2 ->
               let c_el1 = cmpx_of_int el1 in
               stack#push (RpcComplex (Complex.mul c_el1 el2))
            |RpcFloatMatrix el2 ->
               let result = Gsl_matrix.copy el2 in
               (Gsl_matrix.scale result (float_of_big_int el1);
               stack#push (RpcFloatMatrix result))
            |RpcComplexMatrix el2 ->
               let c_el1 = cmpx_of_int el1 in
               (Gsl_matrix_complex.scale el2 c_el1;
               stack#push (RpcComplexMatrix el2))
            )
         |RpcFloat el1 -> (
            match gen_el2 with
            |RpcInt el2 ->
               stack#push (RpcFloat (el1 *. float_of_big_int el2))
            |RpcFloat el2 ->
               stack#push (RpcFloat (el1 *. el2))
            |RpcComplex el2 ->
               let c_el1 = cmpx_of_float el1 in
               stack#push (RpcComplex (Complex.mul c_el1 el2))
            |RpcFloatMatrix el2 ->
               let result = Gsl_matrix.copy el2 in
               (Gsl_matrix.scale result el1;
               stack#push (RpcFloatMatrix result))
            |RpcComplexMatrix el2 ->
               let c_el1 = cmpx_of_float el1 in
               (Gsl_matrix_complex.scale el2 c_el1;
               stack#push (RpcComplexMatrix el2))
            )
         |RpcComplex el1 -> (
            match gen_el2 with
            |RpcInt el2 ->
               let c_el2 = cmpx_of_int el2 in
               stack#push (RpcComplex (Complex.mul el1 c_el2))
            |RpcFloat el2 ->
               let c_el2 = cmpx_of_float el2 in
               stack#push (RpcComplex (Complex.mul el1 c_el2))
            |RpcComplex el2 ->
               stack#push (RpcComplex (Complex.mul el1 el2))
            |RpcFloatMatrix el2 ->
               let c_el2 = cmat_of_fmat el2 in
               (Gsl_matrix_complex.scale c_el2 el1;
               stack#push (RpcComplexMatrix c_el2))
            |RpcComplexMatrix el2 ->
               (Gsl_matrix_complex.scale el2 el1;
               stack#push (RpcComplexMatrix el2))
            )
         |RpcFloatMatrix el1 -> (
            match gen_el2 with
            |RpcInt el2 ->
               let result = Gsl_matrix.copy el1 in
               (Gsl_matrix.scale result (float_of_big_int el2);
               stack#push (RpcFloatMatrix result))
            |RpcFloat el2 ->
               let result = Gsl_matrix.copy el1 in
               (Gsl_matrix.scale result el2;
               stack#push (RpcFloatMatrix result))
            |RpcComplex el2 ->
               let c_el1 = cmat_of_fmat el1 in
               (Gsl_matrix_complex.scale c_el1 el2;
               stack#push (RpcComplexMatrix c_el1))
            |RpcFloatMatrix el2 ->
               let n1, m1 = (Gsl_matrix.dims el1) and
               n2, m2     = (Gsl_matrix.dims el2) in
               if m1 = n2 then
                  let result = Gsl_matrix.create n1 m2 in
                  (Gsl_blas.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans 1.0 el1 el2 0.0 result;
                  stack#push (RpcFloatMatrix result))
               else
                  (stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "incompatible matrix dimensions for multiplication"))
            |RpcComplexMatrix el2 ->
               let n1, m1 = (Gsl_matrix.dims el1) and
               n2, m2     = (Gsl_matrix_complex.dims el2) in
               if m1 = n2 then
                  let c_el1 = cmat_of_fmat el1 and
                  result = Gsl_matrix_complex.create n1 m2 in
                  (Gsl_blas.Complex.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
                     Complex.one c_el1 el2 Complex.zero result;
                  stack#push (RpcComplexMatrix result))
               else
                  (stack#push gen_el1; 
                  stack#push gen_el2;
                  raise (Invalid_argument "incompatible matrix dimensions for multiplication"))
            )
         |RpcComplexMatrix el1 -> (
            match gen_el2 with
            |RpcInt el2 ->
               let c_el2 = cmpx_of_int el2 in
               (Gsl_matrix_complex.scale el1 c_el2;
               stack#push (RpcComplexMatrix el1))
            |RpcFloat el2 ->
               let c_el2 = cmpx_of_float el2 in
               (Gsl_matrix_complex.scale el1 c_el2;
               stack#push (RpcComplexMatrix el1))
            |RpcComplex el2 ->
               (Gsl_matrix_complex.scale el1 el2;
               stack#push (RpcComplexMatrix el1))
            |RpcFloatMatrix el2 ->
               let n1, m1 = (Gsl_matrix_complex.dims el1) and
               n2, m2     = (Gsl_matrix.dims el2) in
               if m1 = n2 then
                  let c_el2 = cmat_of_fmat el2 and
                  result = Gsl_matrix_complex.create n1 m2 in
                  (Gsl_blas.Complex.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
                     Complex.one el1 c_el2 Complex.zero result;
                  stack#push (RpcComplexMatrix result))
               else
                  (stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "incompatible matrix dimensions for multiplication"))
            |RpcComplexMatrix el2 ->
               let n1, m1 = (Gsl_matrix_complex.dims el1) and
               n2, m2     = (Gsl_matrix_complex.dims el2) in
               if m1 = n2 then
                  let result = Gsl_matrix_complex.create n1 m2 in
                  (Gsl_blas.Complex.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
                     Complex.one el1 el2 Complex.zero result;
                  stack#push (RpcComplexMatrix result))
               else
                  (stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "incompatible matrix dimensions for multiplication"))
            )
      end
   else
      raise (Invalid_argument "insufficient arguments for multiplication")


(* arch-tag: DO_NOT_CHANGE_5fc03e41-d1d3-40da-8b68-9a85d96148d0 *)
