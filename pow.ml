(*  Orpie -- a fullscreen RPN calculator for the console
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
open Gsl_assist
open Big_int

let pow (stack : rpc_stack) (evaln : int -> unit) =
   evaln 2;
   let gen_el2 = stack#pop () in
   let gen_el1 = stack#pop () in
   match gen_el1 with
   |RpcInt el1 -> (
      match gen_el2 with
      |RpcInt el2 ->
         if (sign_big_int el2) <> (-1) then
            stack#push (RpcInt 
               (power_big_int_positive_big_int el1 el2))
         else
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "integer power function requires nonnegative power"))
      |RpcFloatUnit el2 ->
         if has_units el2 then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise to a dimensioned power"
         end else
            let f_el1 = float_of_big_int el1 in
            let f_el2 = el2.Units.coeff.Complex.re in
            (* if power is nonnegative or if power is integer *)
            if f_el1 >= 0.0 || f_el2 = float_of_int (int_of_float f_el2) then
               stack#push (RpcFloatUnit (funit_of_float (f_el1 ** f_el2)))
            else
               let c_el1 = cmpx_of_float f_el1 in
               let c_el2 = cmpx_of_float f_el2 in
               stack#push (RpcComplexUnit (cunit_of_cpx
               (Complex.pow c_el1 c_el2)))
      |RpcComplexUnit el2 ->
         if has_units el2 then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise to a dimensioned power"
         end else
            let c_el1 = cmpx_of_int el1 in
            stack#push (RpcComplexUnit (cunit_of_cpx
            (Complex.pow c_el1 el2.Units.coeff)))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types"))
      )
   |RpcFloatUnit el1 -> (
      match gen_el2 with
      |RpcInt el2 ->
         stack#push (RpcFloatUnit (Units.pow el1 (float_of_big_int el2)))
      |RpcFloatUnit el2 ->
         if has_units el2 then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise to a dimensioned power"
         end else
            let new_unit = Units.pow el1 el2.Units.coeff.Complex.re in
            if new_unit.Units.coeff.Complex.im = 0.0 then
               stack#push (RpcFloatUnit new_unit)
            else
               stack#push (RpcComplexUnit new_unit)
      |RpcComplexUnit el2 ->
         if has_units el2 then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise to a dimensioned power"
         end else if has_units el1 then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise dimensioned value to complex power"
         end else
            stack#push (RpcComplexUnit (cunit_of_cpx
            (Complex.pow el1.Units.coeff el2.Units.coeff)))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types"))
      )
   |RpcComplexUnit el1 -> (
      match gen_el2 with
      |RpcInt el2 ->
         stack#push (RpcComplexUnit (Units.pow el1 (float_of_big_int el2)))
      |RpcFloatUnit el2 ->
         if has_units el2 then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise to a dimensioned power"
         end else
            stack#push (RpcComplexUnit 
            (Units.pow el1 el2.Units.coeff.Complex.re))
      |RpcComplexUnit el2 ->
         if has_units el2 then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise to a dimensioned power"
         end else if has_units el1 then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise dimensioned value to complex power"
         end else
            stack#push (RpcComplexUnit (cunit_of_cpx
            (Complex.pow el1.Units.coeff el2.Units.coeff)))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types"))
      )
   |_ ->
      (stack#push gen_el1;
      stack#push gen_el2;
      raise (Invalid_argument "invalid argument"))



(* arch-tag: DO_NOT_CHANGE_55f98700-eb1e-4457-ae73-18170a816984 *)
