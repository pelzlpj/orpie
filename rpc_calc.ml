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

open Rpc_stack;;
open Utility;;
open Big_int;;


class rpc_calc =
   object(self)
      val mutable stack = new rpc_stack
      val mutable backup_stack = new rpc_stack
      val mutable modes = {angle = Rad; base = Dec; complex = Rect}

      method backup () =
         backup_stack <- stack#backup ()

      method undo () =
         stack <- backup_stack

      method add () =
         Add.add stack self#backup

      method sub () =
         Sub.sub stack self#backup

      method mult () =
         Mult.mult stack self#backup

      method div () =
         Div.div stack self#backup

      method inv () =
         Inv.inv stack self#backup

      method pow () =
         Pow.pow stack self#backup

      method get_modes () =
         modes

      method get_stack_size () =
         stack#length

      (* Warning: dup() creates multiple references to the same object.
       * Therefore all operations need to leave the original stack elements
       * unaltered. *)
      method dup () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               (stack#push gen_el;
               stack#push gen_el)
            end
         else
            raise (Invalid_argument "empty stack")

      method neg () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcInt (minus_big_int el))
               |RpcFloat el ->
                  stack#push (RpcFloat (0.0 -. el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.negative el))
               |RpcFloatMatrix el ->
                  let copy = Gsl_matrix.copy el in
                  (Gsl_matrix.scale copy (-1.0);
                  stack#push (RpcFloatMatrix copy))
               |RpcComplexMatrix el ->
                  let copy = Gsl_matrix_complex.copy el in
                  (Gsl_matrix_complex.scale copy {Complex.re=(-1.0); Complex.im=0.0};
                  stack#push (RpcComplexMatrix copy))
            end
         else
            raise (Invalid_argument "empty stack")

      method sqrt () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcFloat el ->
                  stack#push (RpcFloat (sqrt el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.sqrt el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")

      method abs () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcInt (abs_big_int el))
               |RpcFloat el ->
                  stack#push (RpcFloat (abs_float el))
               |RpcComplex el ->
                  stack#push (RpcFloat (Gsl_complex.abs el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")

      method arg () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcComplex el ->
                  stack#push (RpcFloat (Gsl_complex.arg el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method exp () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat (exp (float_of_big_int el)))
               |RpcFloat el ->
                  stack#push (RpcFloat (exp el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.exp el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method ln () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat (log (float_of_big_int el)))
               |RpcFloat el ->
                  stack#push (RpcFloat (log el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.log el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method log10 () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat (log10 (float_of_big_int el)))
               |RpcFloat el ->
                  stack#push (RpcFloat (log10 el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.log10 el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method conj () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcInt el)
               |RpcFloat el ->
                  stack#push (RpcFloat el)
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.conjugate el))
               |RpcFloatMatrix el ->
                  stack#push (RpcFloatMatrix el)
               |RpcComplexMatrix el ->
                  (* element-by-element conjugation *)
                  let rows, cols = Gsl_matrix_complex.dims el and
                  arr = Gsl_matrix_complex.to_array el in
                  let conj_arr = Array.map Gsl_complex.conjugate arr in
                  let conj_mat = Gsl_matrix_complex.of_array conj_arr rows cols in
                  stack#push (RpcComplexMatrix conj_mat)
            end
         else
            raise (Invalid_argument "empty stack")


      method sin () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat (sin (float_of_big_int el)))
               |RpcFloat el ->
                  stack#push (RpcFloat (sin el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.sin el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method cos () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat (cos (float_of_big_int el)))
               |RpcFloat el ->
                  stack#push (RpcFloat (cos el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.cos el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method tan () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat (tan (float_of_big_int el)))
               |RpcFloat el ->
                  stack#push (RpcFloat (tan el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.tan el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method get_display_line line_num =
         stack#get_display_line line_num modes

      method drop () = 
         if stack#length > 0 then
            begin
               self#backup ();
               let dummy = stack#pop () in 
               ()
            end
         else
            raise (Invalid_argument "empty stack")

      method swap () =
         if stack#length > 1 then
            begin
               self#backup ();
               let gen_el1 = stack#pop () in
               let gen_el2 = stack#pop () in
               stack#push gen_el1;
               stack#push gen_el2
            end
         else
            raise (Invalid_argument "insufficient arguments for swap")

      method clear () =
         self#backup ();
         for i = 1 to stack#length do
            let dummy = stack#pop () in ()
         done

      method push (v : rpc_data) =
         self#backup ();
         stack#push v

      method echo el_num =
         if el_num <= stack#length then
            let el = stack#peek el_num in
            stack#push el
         else
            raise (Invalid_argument "cannot echo nonexistant element")

      method enter_int i =
         stack#push (RpcInt i)

      method enter_float f =
         stack#push (RpcFloat f)

      method enter_cmpx f =
         stack#push (RpcComplex f)

      method enter_fmat fm =
         stack#push (RpcFloatMatrix fm)

      method enter_cmat cm =
         stack#push (RpcComplexMatrix cm)

(*      method print_stack () =
         let print_el line_num el = Printf.printf "%2d:  %s\n" line_num el in
         for i = stack#length downto 1 do
            print_el i (stack#get_display_line i modes)
         done
*)

   end;;








(* arch-tag: DO_NOT_CHANGE_548916d4-da42-49b4-8941-c0d42306f1b7 *)
