(*  Orpie -- a stack-based RPN calculator for the console
 *  Copyright (C) 2003-2004  Paul Pelzl
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
open Gsl_error
open Gsl_assist


let inv (stack : rpc_stack) (evaln : int -> unit) =
   evaln 1;
   let gen_el = stack#pop () in
   match gen_el with
   |RpcFloat el ->
      stack#push (RpcFloat (1.0 /. el))
   |RpcComplex el ->
      stack#push (RpcComplex (Complex.inv el))
   |RpcFloatMatrix el ->
      let n, m = (Gsl_matrix.dims el) in
      if n = m then
         let copy_el1 = Gsl_matrix.copy el
         and copy_el2 = Gsl_matrix.copy el
         and perm     = Gsl_permut.create m
         and inv      = Gsl_matrix.create m m
         and vv       = Gsl_matrix.create m m
         and ss       = Gsl_vector.create m
         and work     = Gsl_vector.create m in
         begin
            (* test for singular matrix *)
            (* first factor out matrix norm, since the GSL SVD algorithm has
             * issues with large magnitude matrices *)
            let norm = Gsl_assist.one_norm copy_el1 in
            Gsl_matrix.scale copy_el1 (1.0 /. norm);
            (* now compute condition number as largest singular value
             * divided by smallest singular value *)
            Gsl_linalg._SV_decomp (`M copy_el1) (`M vv) (`V ss) (`V work);
            let condition_number = 
               (Gsl_vector.get ss 0) /. (Gsl_vector.get ss (pred m)) 
            in
            (* if the condition number is too large for machine precision,
             * then abort with error *)
            if condition_number > 1e14 then
               (stack#push gen_el;
               raise (Invalid_argument "cannot invert ill-conditioned matrix"))
            else
               let sign = Gsl_linalg._LU_decomp (`M copy_el2) perm in
               (Gsl_linalg._LU_invert (`M copy_el2) perm (`M inv);
               stack#push (RpcFloatMatrix inv))
         end
      else
         (stack#push gen_el;
         raise (Invalid_argument "cannot invert non-square matrix"))
   |RpcComplexMatrix el ->
      let n, m = (Gsl_matrix_complex.dims el) in
      if n = m then
         let copy_el = Gsl_vectmat.cmat_convert ~protect:true (`CM el) and
         perm = Gsl_permut.create m and
         inv = Gsl_matrix_complex.create m m in
         try
            let sign = Gsl_linalg.complex_LU_decomp copy_el perm in
            (Gsl_linalg.complex_LU_invert copy_el perm (`CM inv);
            stack#push (RpcComplexMatrix inv))
         with Gsl_exn _ ->
            (stack#push gen_el;
            raise (Invalid_argument "cannot invert singular matrix"))
      else
         (stack#push gen_el;
         raise (Invalid_argument "cannot invert non-square matrix"))
   |_ ->
      (stack#push gen_el;
      raise (Invalid_argument "inversion is undefined for this data
      type"))



(* arch-tag: DO_NOT_CHANGE_d8ce074c-3d77-4448-b3c6-9e239b853aad *)
