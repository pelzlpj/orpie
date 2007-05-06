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
open Gsl_assist
open Big_int

let mult (stack : rpc_stack) (evaln : int -> unit) =
   evaln 2;
   let gen_el2 = stack#pop () in
   let gen_el1 = stack#pop () in
   match gen_el1 with
   |RpcInt el1 -> (
      match gen_el2 with
      |RpcInt el2 ->
         stack#push (RpcInt (mult_big_int el1 el2))
      |RpcFloatUnit el2 ->
         let fu_el1 = funit_of_float (float_of_big_int el1) in
         stack#push (RpcFloatUnit (Units.mult fu_el1 el2))
      |RpcComplexUnit el2 ->
         let cu_el1 = cunit_of_cpx (cmpx_of_int el1) in
         stack#push (RpcComplexUnit (Units.mult cu_el1 el2))
      |RpcFloatMatrixUnit (el2, uu) ->
         let result = Gsl_matrix.copy el2 in
         Gsl_matrix.scale result (float_of_big_int el1);
         stack#push (RpcFloatMatrixUnit (result, uu))
      |RpcComplexMatrixUnit (el2, uu) ->
         let c_el1 = cmpx_of_int el1 in
         let result = Gsl_matrix_complex.copy el2 in
         Gsl_matrix_complex.scale result c_el1;
         stack#push (RpcComplexMatrixUnit (result, uu))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for multiplication"))
      )
   |RpcFloatUnit el1 -> (
      match gen_el2 with
      |RpcInt el2 ->
         let fu_el2 = funit_of_float (float_of_big_int el2) in
         stack#push (RpcFloatUnit (Units.mult el1 fu_el2))
      |RpcFloatUnit el2 ->
         stack#push (RpcFloatUnit (Units.mult el1 el2))
      |RpcComplexUnit el2 ->
         stack#push (RpcComplexUnit (Units.mult el1 el2))
      |RpcFloatMatrixUnit (el2, uu) ->
         let uprod = Units.mult el1 uu in
         let result = Gsl_matrix.copy el2 in
         Gsl_matrix.scale result uprod.Units.coeff.Complex.re;
         stack#push (RpcFloatMatrixUnit (result, unorm uprod))
      |RpcComplexMatrixUnit (el2, uu) ->
         let uprod = Units.mult el1 uu in
         let result = Gsl_matrix_complex.copy el2 in
         Gsl_matrix_complex.scale result uprod.Units.coeff;
         stack#push (RpcComplexMatrixUnit (result, unorm uprod))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for multiplication"))
      )
   |RpcComplexUnit el1 -> (
      match gen_el2 with
      |RpcInt el2 ->
         let cu_el2 = cunit_of_cpx (cmpx_of_int el2) in
         stack#push (RpcComplexUnit (Units.mult el1 cu_el2))
      |RpcFloatUnit el2 | RpcComplexUnit el2 ->
         stack#push (RpcComplexUnit (Units.mult el1 el2))
      |RpcFloatMatrixUnit (el2, u2) ->
         let uprod = Units.mult el1 u2 in
         let c_el2 = cmat_of_fmat el2 in
         Gsl_matrix_complex.scale c_el2 uprod.Units.coeff;
         stack#push (RpcComplexMatrixUnit (c_el2, unorm uprod))
      |RpcComplexMatrixUnit (el2, u2) ->
         let uprod = Units.mult el1 u2 in
         let result = Gsl_matrix_complex.copy el2 in
         Gsl_matrix_complex.scale result uprod.Units.coeff;
         stack#push (RpcComplexMatrixUnit (result, unorm uprod))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for multiplication"))
      )
   |RpcFloatMatrixUnit (el1, u1) -> (
      match gen_el2 with
      |RpcInt el2 ->
         let result = Gsl_matrix.copy el1 in
         Gsl_matrix.scale result (float_of_big_int el2);
         stack#push (RpcFloatMatrixUnit (result, u1))
      |RpcFloatUnit el2 ->
         let uprod = Units.mult u1 el2 in
         let result = Gsl_matrix.copy el1 in
         Gsl_matrix.scale result uprod.Units.coeff.Complex.re;
         stack#push (RpcFloatMatrixUnit (result, unorm uprod))
      |RpcComplexUnit el2 ->
         let uprod = Units.mult u1 el2 in
         let c_el1 = cmat_of_fmat el1 in
         Gsl_matrix_complex.scale c_el1 uprod.Units.coeff;
         stack#push (RpcComplexMatrixUnit (c_el1, unorm uprod))
      |RpcFloatMatrixUnit (el2, u2) ->
         let n1, m1 = (Gsl_matrix.dims el1)
         and n2, m2 = (Gsl_matrix.dims el2) in
         if m1 = n2 then
            let uprod = Units.mult u1 u2 in
            let result = Gsl_matrix.create n1 m2 in
            Gsl_blas.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
            uprod.Units.coeff.Complex.re el1 el2 0.0 result;
            stack#push (RpcFloatMatrixUnit (result, unorm uprod))
         else
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "incompatible matrix dimensions for multiplication"))
      |RpcComplexMatrixUnit (el2, u2) ->
         let n1, m1 = (Gsl_matrix.dims el1)
         and n2, m2 = (Gsl_matrix_complex.dims el2) in
         if m1 = n2 then
            let uprod = Units.mult u1 u2 in
            let c_el1 = cmat_of_fmat el1
            and result = Gsl_matrix_complex.create n1 m2 in
            Gsl_blas.Complex.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
            uprod.Units.coeff c_el1 el2 Complex.zero result;
            stack#push (RpcComplexMatrixUnit (result, unorm uprod))
         else
            (stack#push gen_el1; 
            stack#push gen_el2;
            raise (Invalid_argument "incompatible matrix dimensions for multiplication"))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for multiplication"))
      )
   |RpcComplexMatrixUnit (el1, u1) -> (
      match gen_el2 with
      |RpcInt el2 ->
         let c_el2 = cmpx_of_int el2 in
         let result = Gsl_matrix_complex.copy el1 in
         Gsl_matrix_complex.scale result c_el2;
         stack#push (RpcComplexMatrixUnit (result, u1))
      |RpcFloatUnit el2 ->
         let uprod = Units.mult u1 el2 in
         let result = Gsl_matrix_complex.copy el1 in
         Gsl_matrix_complex.scale result uprod.Units.coeff;
         stack#push (RpcComplexMatrixUnit (result, unorm uprod))
      |RpcComplexUnit el2 ->
         let uprod = Units.mult u1 el2 in
         let result = Gsl_matrix_complex.copy el1 in
         Gsl_matrix_complex.scale result uprod.Units.coeff;
         stack#push (RpcComplexMatrixUnit (result, unorm uprod))
      |RpcFloatMatrixUnit (el2, u2) ->
         let n1, m1 = (Gsl_matrix_complex.dims el1)
         and n2, m2 = (Gsl_matrix.dims el2) in
         if m1 = n2 then
            let uprod = Units.mult u1 u2 in
            let c_el2 = cmat_of_fmat el2
            and result = Gsl_matrix_complex.create n1 m2 in
            Gsl_blas.Complex.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
            uprod.Units.coeff el1 c_el2 Complex.zero result;
            stack#push (RpcComplexMatrixUnit (result, unorm uprod))
         else
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "incompatible matrix dimensions for multiplication"))
      |RpcComplexMatrixUnit (el2, u2) ->
         let n1, m1 = (Gsl_matrix_complex.dims el1)
         and n2, m2 = (Gsl_matrix_complex.dims el2) in
         if m1 = n2 then
            let uprod = Units.mult u1 u2 in
            let result = Gsl_matrix_complex.create n1 m2 in
            Gsl_blas.Complex.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
            uprod.Units.coeff el1 el2 Complex.zero result;
            stack#push (RpcComplexMatrixUnit (result, unorm uprod))
         else
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "incompatible matrix dimensions for multiplication"))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for multiplication"))
      )
   |_ ->
      (stack#push gen_el1;
      stack#push gen_el2;
      raise (Invalid_argument "incompatible types for multiplication"))


(* arch-tag: DO_NOT_CHANGE_5fc03e41-d1d3-40da-8b68-9a85d96148d0 *)
