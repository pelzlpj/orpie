(*  Orpie -- a stack-based RPN calculator for the console
 *  Copyright (C) 2003-2004  Paul Pelzl
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

open Rpc_stack
open Gsl_error
open Gsl_assist
open Big_int

let div (stack : rpc_stack) (evaln : int -> unit) =
   evaln 2;
   let gen_el2 = stack#pop () in
   let gen_el1 = stack#pop () in
   match gen_el1 with
   |RpcInt el1 -> (
      match gen_el2 with
      |RpcInt el2 ->
         stack#push (RpcInt (div_big_int el1 el2))
      |RpcFloatUnit el2 ->
         let fu_el1 = funit_of_float (float_of_big_int el1) in
         stack#push (RpcFloatUnit (Units.div fu_el1 el2))
      |RpcComplexUnit el2 ->
         let cu_el1 = cunit_of_cpx (cmpx_of_int el1) in
         stack#push (RpcComplexUnit (Units.div cu_el1 el2))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for division"))
      )
   |RpcFloatUnit el1 -> (
      match gen_el2 with
      |RpcInt el2 ->
         let fu_el2 = funit_of_float (float_of_big_int el2) in
         stack#push (RpcFloatUnit (Units.div el1 fu_el2))
      |RpcFloatUnit el2 ->
         stack#push (RpcFloatUnit (Units.div el1 el2))
      |RpcComplexUnit el2 ->
         stack#push (RpcComplexUnit (Units.div el1 el2))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for division"))
      )
   |RpcComplexUnit el1 -> (
      match gen_el2 with
      |RpcInt el2 ->
         let cu_el2 = cunit_of_cpx (cmpx_of_int el2) in
         stack#push (RpcComplexUnit (Units.div el1 cu_el2))
      |RpcFloatUnit el2 | RpcComplexUnit el2 ->
         stack#push (RpcComplexUnit (Units.div el1 el2))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for division"))
      )
   |RpcFloatMatrixUnit (el1, u1) -> (
      match gen_el2 with
      |RpcInt el2 ->
         let uquot = Units.div u1 (funit_of_float 
         (float_of_big_int el2)) in
         let result = Gsl_matrix.copy el1 in
         (Gsl_matrix.scale result uquot.Units.coeff.Complex.re);
         stack#push (RpcFloatMatrixUnit (result, unorm uquot))
      |RpcFloatUnit el2 ->
         let uquot = Units.div u1 el2 in
         let result = Gsl_matrix.copy el1 in
         (Gsl_matrix.scale result uquot.Units.coeff.Complex.re);
         stack#push (RpcFloatMatrixUnit (result, unorm uquot))
      |RpcComplexUnit el2 ->
         let uquot = Units.div u1 el2 in
         let c_el1 = cmat_of_fmat el1 in
         Gsl_matrix_complex.scale c_el1 uquot.Units.coeff;
         stack#push (RpcComplexMatrixUnit (c_el1, unorm uquot))
      |RpcFloatMatrixUnit (el2, u2) ->
         let n1, m1 = (Gsl_matrix.dims el1) and
         n2, m2     = (Gsl_matrix.dims el2) in
         if n2 = m2 then
            if m1 = n2 then
               let uquot = Units.div u1 u2 in
               let copy_el2 = Gsl_vectmat.mat_convert ~protect:true (`M el2) and
               perm = Gsl_permut.create m1 and
               inv = Gsl_matrix.create m1 m1 in
               try
                  let sign = Gsl_linalg._LU_decomp copy_el2 perm in
                  Gsl_linalg._LU_invert copy_el2 perm (`M inv);
                  let result = Gsl_matrix.create n1 m2 in
                  Gsl_blas.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
                  uquot.Units.coeff.Complex.re el1 inv 0.0 result;
                  stack#push (RpcFloatMatrixUnit (result, unorm uquot))
               with Gsl_exn _ -> 
                  (stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "divisor matrix is singular"))
            else
               (stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "incompatible dimensions for division"))
         else
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "divisor matrix is non-square"))
      |RpcComplexMatrixUnit (el2, u2) ->
         let n1, m1 = (Gsl_matrix.dims el1) and
         n2, m2     = (Gsl_matrix_complex.dims el2) in
         if n2 = m2 then
            if m1 = n2 then
               let uquot = Units.div u1 u2 in
               let copy_el2 = Gsl_matrix_complex.copy el2 and
               perm = Gsl_permut.create m1 and
               inv = Gsl_matrix_complex.create m1 m1 in
               try
                  let sign = Gsl_linalg.complex_LU_decomp (`CM
                  copy_el2) perm in
                  Gsl_linalg.complex_LU_invert (`CM copy_el2) perm
                  (`CM inv);
                  let result = Gsl_matrix_complex.create n1 m2 in
                  Gsl_blas.Complex.gemm Gsl_blas.NoTrans
                  Gsl_blas.NoTrans uquot.Units.coeff (cmat_of_fmat el1) inv
                  Complex.zero result;
                  stack#push (RpcComplexMatrixUnit (result, unorm uquot))
               with Gsl_exn _ -> 
                  (stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "divisor matrix is singular"))
            else
               (stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "incompatible matrix dimensions for division"))
         else
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "divisor matrix is non-square"))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for division"))
      )
   |RpcComplexMatrixUnit (el1, u1) -> (
      match gen_el2 with
      |RpcInt el2 ->
         let c_el2 = cmpx_of_int el2 in
         let result = Gsl_matrix_complex.copy el1 in
         Gsl_matrix_complex.scale result (Complex.inv c_el2);
         stack#push (RpcComplexMatrixUnit (result, u1))
      |RpcFloatUnit el2 | RpcComplexUnit el2 ->
         let uquot = Units.div u1 el2 in
         let result = Gsl_matrix_complex.copy el1 in
         Gsl_matrix_complex.scale result uquot.Units.coeff;
         stack#push (RpcComplexMatrixUnit (result, unorm uquot))
      |RpcFloatMatrixUnit (el2, u2) ->
         let n1, m1 = (Gsl_matrix_complex.dims el1) and
         n2, m2     = (Gsl_matrix.dims el2) in
         if n2 = m2 then
            if m1 = n2 then
               let uquot = Units.div u1 u2 in
               let copy_el2 = Gsl_matrix.copy el2 and
               perm = Gsl_permut.create m1 and
               inv = Gsl_matrix.create m1 m1 in
               try
                  let sign = Gsl_linalg._LU_decomp (`M copy_el2) perm in
                  Gsl_linalg._LU_invert (`M copy_el2) perm (`M inv);
                  let result = Gsl_matrix_complex.create n1 m2 in
                  Gsl_blas.Complex.gemm Gsl_blas.NoTrans
                  Gsl_blas.NoTrans uquot.Units.coeff el1 (cmat_of_fmat inv)
                  Complex.zero result;
                  stack#push (RpcComplexMatrixUnit (result, unorm uquot))
               with Gsl_exn _ -> 
                  (stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "divisor matrix is singular"))
            else
               (stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "incompatible matrix dimensions for division"))
         else
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "divisor matrix is non-square"))
      |RpcComplexMatrixUnit (el2, u2) ->
         let n1, m1 = (Gsl_matrix_complex.dims el1)
         and n2, m2 = (Gsl_matrix_complex.dims el2) in
         if n2 = m2 then
            if m1 = n2 then
               let uquot = Units.div u1 u2 in
               (* FIXME: do we need to use Gsl_vectmat.cmat_convert here? *)
               let copy_el2 = Gsl_matrix_complex.copy el2 and
               perm = Gsl_permut.create m1 and
               inv = Gsl_matrix_complex.create m1 m1 in
               try
                  let sign = Gsl_linalg.complex_LU_decomp (`CM
                  copy_el2) perm in
                  Gsl_linalg.complex_LU_invert (`CM copy_el2) perm
                  (`CM inv);
                  let result = Gsl_matrix_complex.create n1 m2 in
                  Gsl_blas.Complex.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans 
                  uquot.Units.coeff el1 inv Complex.zero result;
                  stack#push (RpcComplexMatrixUnit (result, unorm uquot))
               with Gsl_exn _ -> 
                  (stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "divisor matrix is singular"))
            else
               (stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "incompatible matrix dimensions for division"))
         else
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "divisor matrix is non-square"))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for division"))
         )
   |_ ->
      (stack#push gen_el1;
      stack#push gen_el2;
      raise (Invalid_argument "incompatible types for division"))


(* arch-tag: DO_NOT_CHANGE_c2535853-756a-4574-8f36-1103a81d053b *)
