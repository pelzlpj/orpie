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

open Rpc_stack;;
open Gsl_assist;;
open Big_int;;


let pi = 3.14159265358979323846;;

class rpc_calc =
   object(self)
      val mutable stack = new rpc_stack
      val mutable backup_stack = new rpc_stack
      val mutable modes = {angle = Rad; base = Dec; complex = Rect}

      method backup () =
         backup_stack <- stack#backup ()

      method undo () =
         stack <- backup_stack

      method mode_rad () =
         modes <- {angle = Rad; base = modes.base; complex = modes.complex}

      method mode_deg () =
         modes <- {angle = Deg; base = modes.base; complex = modes.complex}

      method mode_rect () =
         modes <- {angle = modes.angle; base = modes.base; complex = Rect}

      method mode_polar () =
         modes <- {angle = modes.angle; base = modes.base; complex = Polar}

      method mode_bin () =
         modes <- {angle = modes.angle; base = Bin; complex = modes.complex}

      method mode_oct () =
         modes <- {angle = modes.angle; base = Oct; complex = modes.complex}

      method mode_dec () =
         modes <- {angle = modes.angle; base = Dec; complex = modes.complex}

      method mode_hex () =
         modes <- {angle = modes.angle; base = Hex; complex = modes.complex}

      method toggle_angle_mode () =
         match modes.angle with
         |Rad -> self#mode_deg ()
         |Deg -> self#mode_rad ()

      method toggle_complex_mode () =
         match modes.complex with
         |Rect  -> self#mode_polar ()
         |Polar -> self#mode_rect ()

      method cycle_base () =
         match modes.base with
         |Bin -> self#mode_oct ()
         |Oct -> self#mode_dec ()
         |Dec -> self#mode_hex ()
         |Hex -> self#mode_bin ()

      method save_state () =
         stack#save_state modes

      method load_state () =
         modes <- stack#load_state ();
         self#backup ()

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


      method sq () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcInt (mult_big_int el el))
               |RpcFloat el ->
                  stack#push (RpcFloat (el *. el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Complex.mul el el))
               |RpcFloatMatrix el ->
                  let n, m = (Gsl_matrix.dims el) in
                  if n = m then
                     let result = Gsl_matrix.create n m in
                     (Gsl_blas.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans 1.0 el el 0.0 result;
                     stack#push (RpcFloatMatrix result))
                  else
                     (stack#push gen_el;
                     raise (Invalid_argument "matrix is non-square"))
               |RpcComplexMatrix el ->
                  let n, m = (Gsl_matrix_complex.dims el) in
                  if m = n then
                     let result = Gsl_matrix_complex.create n m in
                     (Gsl_blas.Complex.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
                        Complex.one el el Complex.zero result;
                     stack#push (RpcComplexMatrix result))
                  else
                     (stack#push gen_el;
                     raise (Invalid_argument "matrix is non-square"))
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


      method ten_pow_x () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat (10.0 ** (float_of_big_int el)))
               |RpcFloat el ->
                  stack#push (RpcFloat (10.0 ** el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Complex.pow (cmpx_of_float 10.0) el))
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
                  stack#push (RpcFloat 
                  begin
                     match modes.angle with
                     |Rad ->
                        sin (float_of_big_int el)
                     |Deg ->
                        sin (pi /. 180.0 *. (float_of_big_int el))
                  end)
               |RpcFloat el ->
                  stack#push (RpcFloat 
                  begin
                     match modes.angle with
                     |Rad ->
                        sin el
                     |Deg ->
                        sin (pi /. 180.0 *. el)
                  end)
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
               stack#push (RpcFloat 
               begin
                  match modes.angle with
                  |Rad ->
                     cos (float_of_big_int el)
                  |Deg ->
                     cos (pi /. 180.0 *. (float_of_big_int el))
               end)
            |RpcFloat el ->
               stack#push (RpcFloat 
               begin
                  match modes.angle with
                  |Rad ->
                     cos el
                  |Deg ->
                     cos (pi /. 180.0 *. el)
               end)
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
               stack#push (RpcFloat 
               begin
                  match modes.angle with
                  |Rad ->
                     tan (float_of_big_int el)
                  |Deg ->
                     tan (pi /. 180.0 *. (float_of_big_int el))
               end)
            |RpcFloat el ->
               stack#push (RpcFloat 
               begin
                  match modes.angle with
                  |Rad ->
                     tan el
                  |Deg ->
                     tan (pi /. 180.0 *. el)
               end)
            |RpcComplex el ->
               stack#push (RpcComplex (Gsl_complex.tan el))
            |_ ->
               (stack#push gen_el;
               raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method asin () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat 
                     begin
                        match modes.angle with
                        |Rad ->
                           asin (float_of_big_int el)
                        |Deg ->
                           180.0 /. pi *. asin (float_of_big_int el)
                     end)
               |RpcFloat el ->
                  stack#push (RpcFloat 
                     begin
                        match modes.angle with
                        |Rad ->
                           asin el
                        |Deg ->
                           180.0 /. pi *. asin el
                     end)
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.arcsin el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method acos () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat 
                     begin
                        match modes.angle with
                        |Rad ->
                           acos (float_of_big_int el)
                        |Deg ->
                           180.0 /. pi *. acos (float_of_big_int el)
                     end)
               |RpcFloat el ->
                  stack#push (RpcFloat 
                     begin
                        match modes.angle with
                        |Rad ->
                           acos el
                        |Deg ->
                           180.0 /. pi *. acos el
                     end)
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.arccos el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method atan () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat 
                     begin
                        match modes.angle with
                        |Rad ->
                           atan (float_of_big_int el)
                        |Deg ->
                           180.0 /. pi *. atan (float_of_big_int el)
                     end)
               |RpcFloat el ->
                  stack#push (RpcFloat 
                     begin
                        match modes.angle with
                        |Rad ->
                           atan el
                        |Deg ->
                           180.0 /. pi *. atan el
                     end)
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.arctan el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method sinh () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat (sinh (float_of_big_int el)))
               |RpcFloat el ->
                  stack#push (RpcFloat (sinh el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.sinh el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method cosh () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat (cosh (float_of_big_int el)))
               |RpcFloat el ->
                  stack#push (RpcFloat (cosh el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.cosh el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method tanh () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat (tanh (float_of_big_int el)))
               |RpcFloat el ->
                  stack#push (RpcFloat (tanh el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.tanh el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method asinh () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat (Gsl_math.asinh (float_of_big_int el)))
               |RpcFloat el ->
                  stack#push (RpcFloat (Gsl_math.asinh el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.arcsinh el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method acosh () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat (Gsl_math.acosh (float_of_big_int el)))
               |RpcFloat el ->
                  stack#push (RpcFloat (Gsl_math.acosh el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.arccosh el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      method atanh () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcFloat (Gsl_math.atanh (float_of_big_int el)))
               |RpcFloat el ->
                  stack#push (RpcFloat (Gsl_math.atanh el))
               |RpcComplex el ->
                  stack#push (RpcComplex (Gsl_complex.arctanh el))
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      (* real part of complex (or complex matrix) *)
      method re () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push gen_el
               |RpcFloat el ->
                  stack#push gen_el
               |RpcComplex el ->
                  stack#push (RpcFloat el.Complex.re)
               |RpcFloatMatrix el ->
                  stack#push gen_el
               |RpcComplexMatrix el ->
                  let n, m = Gsl_matrix_complex.dims el
                  and carr = Gsl_matrix_complex.to_array el in
                  let farr = Array.make (n * m) 0.0 in
                  for i = 0 to pred (n * m) do
                     farr.(i) <- carr.(i).Complex.re
                  done;
                  stack#push (RpcFloatMatrix (Gsl_matrix.of_array farr n m))
            end
         else
            raise (Invalid_argument "empty stack")


      (* imaginary part of complex (or complex matrix) *)
      method im () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  stack#push (RpcInt zero_big_int)
               |RpcFloat el ->
                  stack#push (RpcFloat 0.0)
               |RpcComplex el ->
                  stack#push (RpcFloat el.Complex.im)
               |RpcFloatMatrix el ->
                  let n, m = Gsl_matrix.dims el in
                  let farr = Array.make (n * m) 0.0 in
                  stack#push (RpcFloatMatrix (Gsl_matrix.of_array farr n m))
               |RpcComplexMatrix el ->
                  let n, m = Gsl_matrix_complex.dims el
                  and carr = Gsl_matrix_complex.to_array el in
                  let farr = Array.make (n * m) 0.0 in
                  for i = 0 to pred (n * m) do
                     farr.(i) <- carr.(i).Complex.im
                  done;
                  stack#push (RpcFloatMatrix (Gsl_matrix.of_array farr n m))
            end
         else
            raise (Invalid_argument "empty stack")


      (* Euler gamma function *)
      method gamma () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  begin try
                     stack#push (RpcFloat (Gsl_sf.gamma (float_of_big_int el)))
                  with
                     Gsl_error.Gsl_exn (err, errstr) ->
                        (stack#push gen_el;
                        raise (Invalid_argument errstr))
                  end
               |RpcFloat el ->
                  begin try
                     stack#push (RpcFloat (Gsl_sf.gamma el))
                  with
                     Gsl_error.Gsl_exn (err, errstr) ->
                        (stack#push gen_el;
                        raise (Invalid_argument errstr))
                  end
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      (* log_e of Euler gamma function *)
      method lngamma () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  begin try
                     stack#push (RpcFloat (Gsl_sf.lngamma (float_of_big_int el)))
                  with
                     Gsl_error.Gsl_exn (err, errstr) ->
                        (stack#push gen_el;
                        raise (Invalid_argument errstr))
                  end
               |RpcFloat el ->
                  begin try
                     stack#push (RpcFloat (Gsl_sf.lngamma el))
                  with
                     Gsl_error.Gsl_exn (err, errstr) ->
                        (stack#push gen_el;
                        raise (Invalid_argument errstr))
                  end
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      (* error function *)
      method erf () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  begin try
                     stack#push (RpcFloat (Gsl_sf.erf (float_of_big_int el)))
                  with
                     Gsl_error.Gsl_exn (err, errstr) ->
                        (stack#push gen_el;
                        raise (Invalid_argument errstr))
                  end
               |RpcFloat el ->
                  begin try
                     stack#push (RpcFloat (Gsl_sf.erf el))
                  with
                     Gsl_error.Gsl_exn (err, errstr) ->
                        (stack#push gen_el;
                        raise (Invalid_argument errstr))
                  end
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")



      (* complementary error function *)
      method erfc () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  begin try
                     stack#push (RpcFloat (Gsl_sf.erfc (float_of_big_int el)))
                  with
                     Gsl_error.Gsl_exn (err, errstr) ->
                        (stack#push gen_el;
                        raise (Invalid_argument errstr))
                  end
               |RpcFloat el ->
                  begin try
                     stack#push (RpcFloat (Gsl_sf.erfc el))
                  with
                     Gsl_error.Gsl_exn (err, errstr) ->
                        (stack#push gen_el;
                        raise (Invalid_argument errstr))
                  end
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")



      (* factorial (calls gamma function) *)
      method fact () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcInt el ->
                  begin try
                     stack#push (RpcFloat (Gsl_sf.gamma ((float_of_big_int el) +.
                     1.0)))
                  with
                     Gsl_error.Gsl_exn (err, errstr) ->
                        (stack#push gen_el;
                        raise (Invalid_argument errstr))
                  end
               |RpcFloat el ->
                  begin try
                     stack#push (RpcFloat (Gsl_sf.gamma (el +. 1.0)))
                  with
                     Gsl_error.Gsl_exn (err, errstr) ->
                        (stack#push gen_el;
                        raise (Invalid_argument errstr))
                  end
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
            end
         else
            raise (Invalid_argument "empty stack")


      (* matrix transpose *)
      method transpose () =
         if stack#length > 0 then
            begin
               self#backup ();
               let gen_el = stack#pop () in
               match gen_el with
               |RpcFloatMatrix el ->
                  let n, m = (Gsl_matrix.dims el) in
                  let trans_mat = Gsl_matrix.create m n in
                  Gsl_matrix.transpose trans_mat el;
                  stack#push (RpcFloatMatrix trans_mat)
               |RpcComplexMatrix el ->
                  let n, m = (Gsl_matrix_complex.dims el) in
                  let trans_mat = Gsl_matrix_complex.create m n in
                  Gsl_matrix_complex.transpose trans_mat el;
                  stack#push (RpcComplexMatrix trans_mat)
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "transpose requires a matrix argument"))
            end
         else
            raise (Invalid_argument "empty stack")



      method enter_pi () =
         stack#push (RpcFloat pi)

      method get_display_line line_num =
         stack#get_display_string line_num modes

      method get_fullscreen_display line_num =
         stack#get_fullscreen_display_string line_num modes

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

      method push (v : orpie_data) =
         self#backup ();
         stack#push v

      method echo el_num =
         if el_num <= stack#length then
            let el = stack#peek el_num in
            stack#push el
         else
            raise (Invalid_argument "cannot echo nonexistant element")

      method rolldown i =
         stack#rolldown i

      method rollup i =
         stack#rollup i

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
