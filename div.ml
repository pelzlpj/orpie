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
      |RpcFloatUnit (el2, uu2) ->
         let f_el1 = float_of_big_int el1 in
         stack#push (RpcFloatUnit (f_el1 /. el2, Units.div Units.empty_unit uu2))
      |RpcComplexUnit (el2, uu2) ->
         let c_el1 = cmpx_of_int el1 in
         stack#push (RpcComplexUnit 
            (Complex.div c_el1 el2, Units.div Units.empty_unit uu2))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for division"))
      )
   |RpcFloatUnit (el1, uu1) -> (
      match gen_el2 with
      |RpcInt el2 ->
         let f_el2 = float_of_big_int el2 in
         stack#push (RpcFloatUnit (el1 /. f_el2, uu1))
      |RpcFloatUnit (el2, uu2) ->
         stack#push (RpcFloatUnit (el1 /. el2, Units.div uu1 uu2))
      |RpcComplexUnit (el2, uu2) ->
         let c_el1 = c_of_f el1 in
         stack#push (RpcComplexUnit (Complex.div c_el1 el2, Units.div uu1 uu2))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for division"))
      )
   |RpcComplexUnit (el1, uu1) -> (
      match gen_el2 with
      |RpcInt el2 ->
         let c_el2 = cmpx_of_int el2 in
         stack#push (RpcComplexUnit (Complex.div el1 c_el2, uu1))
      |RpcFloatUnit (el2, uu2) ->
         let c_el2 = c_of_f el2 in
         stack#push (RpcComplexUnit (Complex.div el1 c_el2, Units.div uu1 uu2))
      |RpcComplexUnit (el2, uu2) ->
         stack#push (RpcComplexUnit (Complex.div el1 el2, Units.div uu1 uu2))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for division"))
      )
   |RpcFloatMatrixUnit (el1, uu1) -> (
      match gen_el2 with
      |RpcInt el2 ->
         let f_el2 = float_of_big_int el2 in
         let result = Gsl_matrix.copy el1 in
         (Gsl_matrix.scale result (1.0 /. f_el2));
         stack#push (RpcFloatMatrixUnit (result, uu1))
      |RpcFloatUnit (el2, uu2) ->
         let result = Gsl_matrix.copy el1 in
         (Gsl_matrix.scale result (1.0 /. el2));
         stack#push (RpcFloatMatrixUnit (result, Units.div uu1 uu2))
      |RpcComplexUnit (el2, uu2) ->
         let c_el1 = cmat_of_fmat el1 in
         Gsl_matrix_complex.scale c_el1 (Complex.inv el2);
         stack#push (RpcComplexMatrixUnit (c_el1, Units.div uu1 uu2))
      |RpcFloatMatrixUnit (el2, uu2) ->
         let n1, m1 = (Gsl_matrix.dims el1)
         and n2, m2 = (Gsl_matrix.dims el2) in
         if n2 = m2 then
            if m1 = n2 then
               let copy_el2 = Gsl_vectmat.mat_convert ~protect:true (`M el2)
               and perm = Gsl_permut.create m1
               and inv = Gsl_matrix.create m1 m1 in
               try
                  let _ = Gsl_linalg._LU_decomp copy_el2 perm in
                  Gsl_linalg._LU_invert copy_el2 perm (`M inv);
                  let result = Gsl_matrix.create n1 m2 in
                  Gsl_blas.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
                  1.0 el1 inv 0.0 result;
                  stack#push (RpcFloatMatrixUnit (result, Units.div uu1 uu2))
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
      |RpcComplexMatrixUnit (el2, uu2) ->
         let n1, m1 = (Gsl_matrix.dims el1)
         and n2, m2 = (Gsl_matrix_complex.dims el2) in
         if n2 = m2 then
            if m1 = n2 then
               let copy_el2 = Gsl_matrix_complex.copy el2
               and perm = Gsl_permut.create m1
               and inv = Gsl_matrix_complex.create m1 m1 in
               try
                  let _ = Gsl_linalg.complex_LU_decomp (`CM copy_el2) perm in
                  Gsl_linalg.complex_LU_invert (`CM copy_el2) perm
                  (`CM inv);
                  let result = Gsl_matrix_complex.create n1 m2 in
                  Gsl_blas.Complex.gemm Gsl_blas.NoTrans
                  Gsl_blas.NoTrans Complex.one (cmat_of_fmat el1) inv
                  Complex.zero result;
                  stack#push (RpcComplexMatrixUnit (result, Units.div uu1 uu2))
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
   |RpcComplexMatrixUnit (el1, uu1) -> (
      match gen_el2 with
      |RpcInt el2 ->
         let c_el2 = cmpx_of_int el2 in
         let result = Gsl_matrix_complex.copy el1 in
         Gsl_matrix_complex.scale result (Complex.inv c_el2);
         stack#push (RpcComplexMatrixUnit (result, uu1))
      |RpcFloatUnit (el2, uu2) ->
         let result = Gsl_matrix_complex.copy el1 in
         Gsl_matrix_complex.scale result (Complex.inv (c_of_f el2));
         stack#push (RpcComplexMatrixUnit (result, Units.div uu1 uu2))
      |RpcComplexUnit (el2, uu2) ->
         let result = Gsl_matrix_complex.copy el1 in
         Gsl_matrix_complex.scale result (Complex.inv el2);
         stack#push (RpcComplexMatrixUnit (result, Units.div uu1 uu2))
      |RpcFloatMatrixUnit (el2, uu2) ->
         let n1, m1 = (Gsl_matrix_complex.dims el1)
         and n2, m2 = (Gsl_matrix.dims el2) in
         if n2 = m2 then
            if m1 = n2 then
               let copy_el2 = Gsl_matrix.copy el2
               and perm = Gsl_permut.create m1
               and inv = Gsl_matrix.create m1 m1 in
               try
                  let _ = Gsl_linalg._LU_decomp (`M copy_el2) perm in
                  Gsl_linalg._LU_invert (`M copy_el2) perm (`M inv);
                  let result = Gsl_matrix_complex.create n1 m2 in
                  Gsl_blas.Complex.gemm Gsl_blas.NoTrans
                  Gsl_blas.NoTrans Complex.one el1 (cmat_of_fmat inv)
                  Complex.zero result;
                  stack#push (RpcComplexMatrixUnit (result, Units.div uu1 uu2))
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
      |RpcComplexMatrixUnit (el2, uu2) ->
         let n1, m1 = (Gsl_matrix_complex.dims el1)
         and n2, m2 = (Gsl_matrix_complex.dims el2) in
         if n2 = m2 then
            if m1 = n2 then
               (* FIXME: do we need to use Gsl_vectmat.cmat_convert here? *)
               let copy_el2 = Gsl_matrix_complex.copy el2
               and perm = Gsl_permut.create m1
               and inv = Gsl_matrix_complex.create m1 m1 in
               try
                  let _ = Gsl_linalg.complex_LU_decomp (`CM
                  copy_el2) perm in
                  Gsl_linalg.complex_LU_invert (`CM copy_el2) perm
                  (`CM inv);
                  let result = Gsl_matrix_complex.create n1 m2 in
                  Gsl_blas.Complex.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans 
                  Complex.one el1 inv Complex.zero result;
                  stack#push (RpcComplexMatrixUnit (result, Units.div uu1 uu2))
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
