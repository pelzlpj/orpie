(*  Orpie -- a fullscreen RPN calculator for the console
 *  Copyright (C) 2003-2004, 2005, 2006-2007, 2010, 2018 Paul Pelzl
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License, Version 3,
 *  as published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  Please send bug reports, patches, etc. to Paul Pelzl at
 *  <pelzlpj@gmail.com>.
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
      |RpcFloatUnit (el2, uu2) ->
         if uu2 <> Units.empty_unit then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise to a dimensioned power"
         end else
            let f_el1 = float_of_big_int el1 in
            let f_el2 = el2 in
            (* if power is nonnegative or if power is integer *)
            if f_el1 >= 0.0 || f_el2 = float_of_int (int_of_float f_el2) then
               let (f, u) = funit_of_float (f_el1 ** f_el2) in
               stack#push (RpcFloatUnit (f, u))
            else
               let c_el1 = cmpx_of_float f_el1 in
               let c_el2 = cmpx_of_float f_el2 in
               let (c, u) = cunit_of_cpx (Complex.pow c_el1 c_el2) in
               stack#push (RpcComplexUnit (c, u))
      |RpcComplexUnit (el2, uu2) ->
         if uu2 <> Units.empty_unit then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise to a dimensioned power"
         end else
            let c_el1 = cmpx_of_int el1 in
            let (c, u) = cunit_of_cpx (Complex.pow c_el1 el2) in
            stack#push (RpcComplexUnit (c, u))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types"))
      )
   |RpcFloatUnit (el1, uu1) -> (
      match gen_el2 with
      |RpcInt el2 ->
         let f_el2 = float_of_big_int el2 in
         stack#push (RpcFloatUnit (el1 ** f_el2, Units.pow uu1 f_el2))
      |RpcFloatUnit (el2, uu2) ->
         if uu2 <> Units.empty_unit then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise to a dimensioned power"
         end else if el2 > 0.0 then begin
            stack#push (RpcFloatUnit (el1 ** el2, Units.pow uu1 el2))
         end else
            let c_el1 = c_of_f el1
            and c_el2 = c_of_f el2 in
            let c_prod = Complex.pow c_el1 c_el2 in
            if c_prod.Complex.im <> 0.0 then
               stack#push (RpcComplexUnit (c_prod, Units.pow uu1 el2))
            else
               stack#push (RpcFloatUnit (c_prod.Complex.re, Units.pow uu1 el2))
      |RpcComplexUnit (el2, uu2) ->
         if uu2 <> Units.empty_unit then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise to a dimensioned power"
         end else if uu1 <> Units.empty_unit then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise dimensioned value to complex power"
         end else
            let c_el1 = c_of_f el1 in
            let (c, u) = cunit_of_cpx (Complex.pow c_el1 el2) in
            stack#push (RpcComplexUnit (c, u))
      |_ ->
         (stack#push gen_el1;
         stack#push gen_el2;
         raise (Invalid_argument "incompatible types"))
      )
   |RpcComplexUnit (el1, uu1) -> (
      match gen_el2 with
      |RpcInt el2 ->
         let f_el2 = float_of_big_int el2 in
         let c_el2 = cmpx_of_int el2 in
         let c_prod = Complex.pow el1 c_el2 in
         stack#push (RpcComplexUnit (c_prod, Units.pow uu1 f_el2))
      |RpcFloatUnit (el2, uu2) ->
         if uu2 <> Units.empty_unit then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise to a dimensioned power"
         end else
            let c_el2 = c_of_f el2 in
            let c_prod = Complex.pow el1 c_el2 in
            stack#push (RpcComplexUnit (c_prod, Units.pow uu1 el2))
      |RpcComplexUnit (el2, uu2) ->
         if uu2 <> Units.empty_unit then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise to a dimensioned power"
         end else if uu1 <> Units.empty_unit then begin
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "cannot raise dimensioned value to complex power"
         end else
            let (c, u) = cunit_of_cpx (Complex.pow el1 el2) in
            stack#push (RpcComplexUnit (c, u))
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
