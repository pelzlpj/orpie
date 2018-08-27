(*  Orpie -- a fullscreen RPN calculator for the console
 *  Copyright (C) 2003-2004, 2005, 2006-2007, 2010 Paul Pelzl
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
 *  <pelzlpj@gmail.com>.
 *)

open Rpc_stack
open Gsl.Error


let inv (stack : rpc_stack) (evaln : int -> unit) =
   evaln 1;
   let gen_el = stack#pop () in
   match gen_el with
   |RpcFloatUnit (el, uu) ->
      stack#push (RpcFloatUnit (1.0 /. el, Units.div Units.empty_unit uu))
   |RpcComplexUnit (el, uu) ->
      stack#push (RpcComplexUnit 
         (Complex.inv el, Units.div Units.empty_unit uu))
   |RpcFloatMatrixUnit (el, uu) ->
      let new_unit = Units.pow uu (~-. 1.0) in
      let n, m = (Gsl.Matrix.dims el) in
      if n = m then
         let copy_el1 = Gsl.Matrix.copy el
         and copy_el2 = Gsl.Matrix.copy el
         and perm     = Gsl.Permut.create m
         and inv      = Gsl.Matrix.create m m
         and vv       = Gsl.Matrix.create m m
         and ss       = Gsl.Vector.create m
         and work     = Gsl.Vector.create m in
         begin
            (* test for singular matrix *)
            (* first factor out matrix norm, since the GSL SVD algorithm has
             * issues with large magnitude matrices *)
            let norm = Gsl_assist.one_norm copy_el1 in
            Gsl.Matrix.scale copy_el1 (1.0 /. norm);
            (* now compute condition number as largest singular value
             * divided by smallest singular value *)
            Gsl.Linalg._SV_decomp ~a:(`M copy_el1) ~v:(`M vv) ~s:(`V ss) ~work:(`V work);
            let condition_number = 
               (Gsl.Vector.get ss 0) /. (Gsl.Vector.get ss (pred m)) 
            in
            (* if the condition number is too large for machine precision,
             * then abort with error *)
            if condition_number > 1e14 then
               (stack#push gen_el;
               raise (Invalid_argument "cannot invert ill-conditioned matrix"))
            else
               let _ = Gsl.Linalg._LU_decomp (`M copy_el2) perm in
               (Gsl.Linalg._LU_invert (`M copy_el2) perm (`M inv);
               stack#push (RpcFloatMatrixUnit (inv, new_unit)))
         end
      else
         (stack#push gen_el;
         raise (Invalid_argument "cannot invert non-square matrix"))
   |RpcComplexMatrixUnit (el, uu) ->
      let new_unit = Units.pow uu (~-. 1.0) in
      let n, m = (Gsl.Matrix_complex.dims el) in
      if n = m then
         let copy_el = Gsl.Vectmat.cmat_convert ~protect:true (`CM el) and
         perm = Gsl.Permut.create m and
         inv = Gsl.Matrix_complex.create m m in
         try
            let _ = Gsl.Linalg.complex_LU_decomp copy_el perm in
            Gsl.Linalg.complex_LU_invert copy_el perm (`CM inv);
            stack#push (RpcComplexMatrixUnit (inv, new_unit))
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
