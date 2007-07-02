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

let sub (stack : rpc_stack) (evaln : int -> unit) =
   evaln 2;
   let gen_el2 = stack#pop () in
   let gen_el1 = stack#pop () in
   match gen_el1 with
   |RpcInt el1 -> (
      match gen_el2 with 
      |RpcInt el2 ->
         stack#push (RpcInt (sub_big_int el1 el2))
      |RpcFloatUnit (el2, uu2) ->
         if uu2 <> Units.empty_unit then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "inconsistent units"
         end else
            let (f, u) = funit_of_float ((float_of_big_int el1) -. el2) in
            stack#push (RpcFloatUnit (f, u))
      |RpcComplexUnit (el2, uu2) ->
         if uu2 <> Units.empty_unit then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "inconsistent units"
         end else
            let c_el1 = cmpx_of_int el1 in
            let (c, u) = cunit_of_cpx (Complex.sub c_el1 el2) in
            stack#push (RpcComplexUnit (c, u))
      |_ ->
         (* if the elements are incompatible, we have to
            put them back on the stack *)
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for subtraction"))
      )
   |RpcFloatUnit (el1, uu1) -> (
      match gen_el2 with
      |RpcInt el2 ->
         if uu1 <> Units.empty_unit then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "inconsistent units"
         end else
            let (f, u) = funit_of_float (el1 -. float_of_big_int el2) in
            stack#push (RpcFloatUnit (f, u))
      |RpcFloatUnit (el2, uu2) ->
         begin try
            let conv = Units.conversion_factor uu1 uu2 !Rcfile.unit_table in
            stack#push (RpcFloatUnit (conv *. el1 -. el2, uu2))
         with Units.Units_error s -> 
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid s
         end
      |RpcComplexUnit (el2, uu2) ->
         begin try
            let c_el1 = c_of_f 
               (el1 *. (Units.conversion_factor uu1 uu2 !Rcfile.unit_table))
            in
            stack#push (RpcComplexUnit (Complex.sub c_el1 el2, uu2))
         with Units.Units_error s -> 
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid s
         end
      |_ ->
         (* if the elements are incompatible, we have to
            put them back on the stack *)
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for subtraction"))
      )
   |RpcComplexUnit (el1, uu1) -> (
      match gen_el2 with
      |RpcInt el2 ->
         if uu1 <> Units.empty_unit then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "inconsistent units"
         end else
            let c_el2 = cmpx_of_int el2 in
            let (c, u) = cunit_of_cpx (Complex.sub el1 c_el2) in
            stack#push (RpcComplexUnit (c, u))
      |RpcFloatUnit (el2, uu2) ->
         begin try
            let conv = c_of_f (Units.conversion_factor uu1 uu2 !Rcfile.unit_table) in
            let c_el1 = Complex.mul el1 conv in
            let c_el2 = c_of_f el2 in
            stack#push (RpcComplexUnit (Complex.sub c_el1 c_el2, uu2))
         with Units.Units_error s ->
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid s
         end
      |RpcComplexUnit (el2, uu2) ->
         begin try
            let conv = c_of_f (Units.conversion_factor uu1 uu2 !Rcfile.unit_table) in
            let c_el1 = Complex.mul el1 conv in
            stack#push (RpcComplexUnit (Complex.sub c_el1 el2, uu2))
         with Units.Units_error s ->
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid s
         end
      |_ ->
         (* if the elements are incompatible, we have to
            put them back on the stack *)
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for subtraction"))
      )
   |RpcFloatMatrixUnit (el1, u1) -> (
      match gen_el2 with
      |RpcFloatMatrixUnit (el2, u2) ->
         let dim1 = (Gsl_matrix.dims el1) and
         dim2     = (Gsl_matrix.dims el2) in
         if dim1 = dim2 then
            try
               let conv = Units.conversion_factor u1 u2 !Rcfile.unit_table in
               let result = Gsl_matrix.copy el1 in
               Gsl_matrix.scale result conv;
               Gsl_matrix.sub result el2;
               stack#push (RpcFloatMatrixUnit (result, u2))
            with Units.Units_error s -> 
               stack#push gen_el1;
               stack#push gen_el2;
               raise_invalid s
         else
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "incompatible matrix dimensions for subtraction"))
      |RpcComplexMatrixUnit (el2, u2) ->
         let dim1 = (Gsl_matrix.dims el1) and
         dim2     = (Gsl_matrix_complex.dims el2) in
         if dim1 = dim2 then
            try
               let conv = c_of_f (Units.conversion_factor u1 u2 !Rcfile.unit_table) in
               let c_el1 = cmat_of_fmat el1 in
               Gsl_matrix_complex.scale c_el1 conv;
               Gsl_matrix_complex.sub c_el1 el2;
               stack#push (RpcComplexMatrixUnit (c_el1, u2))
            with Units.Units_error s ->
               stack#push gen_el1;
               stack#push gen_el2;
               raise_invalid s
         else
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "incompatible matrix dimensions for subtraction"))
      |_ ->
         (* if the elements are incompatible, we have to
            put them back on the stack *)
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for subtraction"))
      )
   |RpcComplexMatrixUnit (el1, u1) -> (
      match gen_el2 with 
      |RpcFloatMatrixUnit (el2, u2) ->
         let dim1 = (Gsl_matrix_complex.dims el1) and
         dim2     = (Gsl_matrix.dims el2) in
         if dim1 = dim2 then
            try
               let conv = c_of_f (Units.conversion_factor u1 u2 !Rcfile.unit_table) in
               let c_el2 = cmat_of_fmat el2 in
               let result = Gsl_matrix_complex.copy el1 in
               Gsl_matrix_complex.scale result conv;
               Gsl_matrix_complex.sub result c_el2;
               stack#push (RpcComplexMatrixUnit (result, u2))
            with Units.Units_error s ->
               stack#push gen_el1;
               stack#push gen_el2;
               raise_invalid s
         else
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "incompatible matrix dimensions for subtraction"))
      |RpcComplexMatrixUnit (el2, u2) ->
         let dim1 = (Gsl_matrix_complex.dims el1) and
         dim2     = (Gsl_matrix_complex.dims el2) in
         if dim1 = dim2 then
            try
               let conv = c_of_f (Units.conversion_factor u1 u2 !Rcfile.unit_table) in
               let result = Gsl_matrix_complex.copy el1 in
               Gsl_matrix_complex.scale result conv;
               Gsl_matrix_complex.sub result el2;
               stack#push (RpcComplexMatrixUnit (result, u2))
            with Units.Units_error s ->
               stack#push gen_el1;
               stack#push gen_el2;
               raise_invalid s
         else
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "incompatible matrix dimensions for subtraction"))
      |_ ->
         (* if the elements are incompatible, we have to
            put them back on the stack *)
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types for subtraction"))
      )
   |_ ->
      (stack#push gen_el1;
      stack#push gen_el2;
      raise (Invalid_argument "incompatible types for subtraction"))



(* arch-tag: DO_NOT_CHANGE_f9044e6f-03c7-465a-b8ab-87cf65a0bc37 *)
