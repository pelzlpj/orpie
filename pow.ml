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

let pow (stack : rpc_stack) (do_backup : unit -> unit) =
   if stack#length > 1 then
      begin
         do_backup ();
         let gen_el2 = stack#pop () in
         let gen_el1 = stack#pop () in
         match gen_el1 with
         |RpcInt el1 -> (
            match gen_el2 with
            |RpcInt el2 ->
               if (sign_big_int el2) != (-1) then
                  stack#push (RpcInt 
                     (power_big_int_positive_big_int el1 el2))
               else
                  (stack#push gen_el2;
                  stack#push gen_el1;
                  raise (Invalid_argument "invalid argument"))
            |RpcFloat el2 ->
               let f_el1 = float_of_big_int el1 in
               stack#push (RpcFloat (f_el1 ** el2))
            |RpcComplex el2 ->
               let c_el1 = cmpx_of_int el1 in
               stack#push (RpcComplex (Complex.pow c_el1 el2))
            |_ ->
               (stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "incompatible types"))
            )
         |RpcFloat el1 -> (
            match gen_el2 with
            |RpcInt el2 ->
               stack#push (RpcFloat (el1 ** (float_of_big_int el2)))
            |RpcFloat el2 ->
               stack#push (RpcFloat (el1 ** el2))
            |RpcComplex el2 ->
               stack#push (RpcComplex (Complex.pow (cmpx_of_float el1) el2))
            |_ ->
               (stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "incompatible types"))
            )
         |RpcComplex el1 -> (
            match gen_el2 with
            |RpcInt el2 ->
               stack#push (RpcComplex (Complex.pow el1 (cmpx_of_int el2)))
            |RpcFloat el2 ->
               stack#push (RpcComplex (Complex.pow el1 (cmpx_of_float el2)))
            |RpcComplex el2 ->
               stack#push (RpcComplex (Complex.pow el1 el2))
            |_ ->
               (stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "incompatible types"))
            )
         |_ ->
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "invalid argument"))
      end
   else
      raise (Invalid_argument "insufficient arguments")



(* arch-tag: DO_NOT_CHANGE_55f98700-eb1e-4457-ae73-18170a816984 *)
