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

type interruptable_args_t =
   | Gcd_args of big_int * big_int * orpie_data_t * orpie_data_t
   | Lcm_args of big_int * big_int * big_int * orpie_data_t * orpie_data_t
   | Fact_args of big_int * big_int * orpie_data_t
   | Binom_args of big_int * big_int * big_int * 
                   big_int * orpie_data_t * orpie_data_t
   | NoArgs;;

let pi = 3.14159265358979323846;;

class rpc_calc =
   object(self)
      val mutable stack = new rpc_stack
      val mutable backup_stack = new rpc_stack
      val mutable modes = {angle = Rad; base = Dec; complex = Rect}
      val mutable variables = Hashtbl.create 10
      val mutable interr_args = NoArgs

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

      method get_variables () =
         variables

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
         stack#save_state modes variables

      method load_state () =
         let m, v = stack#load_state () in
         modes     <- m;
         variables <- v;
         self#backup ()

      method abort_computation () =
         match interr_args with
         |Gcd_args (a, b, el1, el2) ->
            stack#push el1;
            stack#push el2;
            interr_args <- NoArgs
         |Lcm_args (coeff, a, b, el1, el2) ->
            stack#push el1;
            stack#push el2;
            interr_args <- NoArgs
         |Fact_args (num, acc, el) ->
            stack#push el;
            interr_args <- NoArgs
         |Binom_args (n, k, num, denom, el1, el2) ->
            stack#push el1;
            stack#push el2;
            interr_args <- NoArgs
         |NoArgs ->
            ()


      (* all calc functions will need to have arguments checked
       * and a backup performed, so we handle it with a little wrapper function *)
      method private check_args (num_args : int) (fn_str : string) (fn : unit -> unit) =
         if stack#length >= num_args then begin
            self#backup ();
            fn ()
         end else if stack#length = 0 then
            raise (Invalid_argument "empty stack")
         else
            raise (Invalid_argument ("insufficient arguments for " ^ fn_str))


      method add () = self#check_args 2 "addition" self#internal_add

      method private internal_add () =
         Add.add stack self#evaln


      method sub () = self#check_args 2 "subtraction" self#internal_sub

      method private internal_sub () =
         Sub.sub stack self#evaln


      method mult () = self#check_args 2 "multiplication" self#internal_mult

      method private internal_mult () =
         Mult.mult stack self#evaln


      method div () = self#check_args 2 "division" self#internal_div

      method private internal_div () =
         Div.div stack self#evaln

      
      method inv () = self#check_args 1 "inv" self#internal_inv

      method private internal_inv () =
         Inv.inv stack self#evaln


      method pow () = self#check_args 2 "pow" self#internal_pow

      method private internal_pow () =
         Pow.pow stack self#evaln


      method get_modes () =
         modes

      method get_stack_size () =
         stack#length


      method dup () = self#check_args 1 "dup" self#internal_dup

      (* Warning: dup() creates multiple references to the same object.
       * Therefore all operations need to leave the original stack elements
       * unaltered. *)
      method private internal_dup () = stack#dup ()


      method neg () = self#check_args 1 "neg" self#internal_neg

      method private internal_neg () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)


      method sq () = self#check_args 1 "sq" self#internal_sq

      method private internal_sq () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)



      method sqrt () = self#check_args 1 "sqrt" self#internal_sqrt

      method private internal_sqrt () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcFloat el ->
            stack#push (RpcFloat (sqrt el))
         |RpcComplex el ->
            stack#push (RpcComplex (Gsl_complex.sqrt el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method abs () = self#check_args 1 "abs" self#internal_abs

      method private internal_abs () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcInt el ->
            stack#push (RpcInt (abs_big_int el))
         |RpcFloat el ->
            stack#push (RpcFloat (abs_float el))
         |RpcComplex el ->
            stack#push (RpcFloat (Gsl_complex.abs el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method arg () = self#check_args 1 "arg" self#internal_arg

      method private internal_arg () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcComplex el ->
            begin match modes.angle with
            |Rad ->
               stack#push (RpcFloat (Gsl_complex.arg el))
            |Deg ->
               stack#push (RpcFloat (180.0 /. pi *. (Gsl_complex.arg el)))
            end
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method exp () = self#check_args 1 "exp" self#internal_exp

      method private internal_exp () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcInt el ->
            stack#push (RpcFloat (exp (float_of_big_int el)))
         |RpcFloat el ->
            stack#push (RpcFloat (exp el))
         |RpcComplex el ->
            stack#push (RpcComplex (Gsl_complex.exp el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method ln () = self#check_args 1 "ln" self#internal_ln

      method private internal_ln () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcInt el ->
            stack#push (RpcFloat (log (float_of_big_int el)))
         |RpcFloat el ->
            stack#push (RpcFloat (log el))
         |RpcComplex el ->
            stack#push (RpcComplex (Gsl_complex.log el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method ten_pow_x () = self#check_args 1 "10_x" self#internal_ten_pow_x

      method private internal_ten_pow_x () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcInt el ->
            stack#push (RpcFloat (10.0 ** (float_of_big_int el)))
         |RpcFloat el ->
            stack#push (RpcFloat (10.0 ** el))
         |RpcComplex el ->
            stack#push (RpcComplex (Complex.pow (cmpx_of_float 10.0) el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))
            

      method log10 () = self#check_args 1 "log10" self#internal_log10

      method private internal_log10 () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcInt el ->
            stack#push (RpcFloat (log10 (float_of_big_int el)))
         |RpcFloat el ->
            stack#push (RpcFloat (log10 el))
         |RpcComplex el ->
            stack#push (RpcComplex (Gsl_complex.log10 el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method conj () = self#check_args 1 "conj" self#internal_conj

      method private internal_conj () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)


      method sin () = self#check_args 1 "sin" self#internal_sin

      method private internal_sin () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method cos () = self#check_args 1 "cos" self#internal_cos

      method private internal_cos () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method tan () = self#check_args 1 "tan" self#internal_tan

      method private internal_tan () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method asin () = self#check_args 1 "asin" self#internal_asin

      method private internal_asin () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method acos () = self#check_args 1 "acos" self#internal_acos

      method private internal_acos () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method atan () = self#check_args 1 "atan" self#internal_atan

      method private internal_atan () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method sinh () = self#check_args 1 "sinh" self#internal_sinh

      method private internal_sinh () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcInt el ->
            stack#push (RpcFloat (sinh (float_of_big_int el)))
         |RpcFloat el ->
            stack#push (RpcFloat (sinh el))
         |RpcComplex el ->
            stack#push (RpcComplex (Gsl_complex.sinh el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method cosh () = self#check_args 1 "cosh" self#internal_cosh

      method private internal_cosh () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcInt el ->
            stack#push (RpcFloat (cosh (float_of_big_int el)))
         |RpcFloat el ->
            stack#push (RpcFloat (cosh el))
         |RpcComplex el ->
            stack#push (RpcComplex (Gsl_complex.cosh el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method tanh () = self#check_args 1 "tanh" self#internal_tanh

      method private internal_tanh () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcInt el ->
            stack#push (RpcFloat (tanh (float_of_big_int el)))
         |RpcFloat el ->
            stack#push (RpcFloat (tanh el))
         |RpcComplex el ->
            stack#push (RpcComplex (Gsl_complex.tanh el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method asinh () = self#check_args 1 "asinh" self#internal_asinh

      method private internal_asinh () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcInt el ->
            stack#push (RpcFloat (Gsl_math.asinh (float_of_big_int el)))
         |RpcFloat el ->
            stack#push (RpcFloat (Gsl_math.asinh el))
         |RpcComplex el ->
            stack#push (RpcComplex (Gsl_complex.arcsinh el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method acosh () = self#check_args 1 "acosh" self#internal_acosh

      method private internal_acosh () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcInt el ->
            stack#push (RpcFloat (Gsl_math.acosh (float_of_big_int el)))
         |RpcFloat el ->
            stack#push (RpcFloat (Gsl_math.acosh el))
         |RpcComplex el ->
            stack#push (RpcComplex (Gsl_complex.arccosh el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method atanh () = self#check_args 1 "atanh" self#internal_atanh

      method private internal_atanh () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcInt el ->
            stack#push (RpcFloat (Gsl_math.atanh (float_of_big_int el)))
         |RpcFloat el ->
            stack#push (RpcFloat (Gsl_math.atanh el))
         |RpcComplex el ->
            stack#push (RpcComplex (Gsl_complex.arctanh el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method re () = self#check_args 1 "re" self#internal_re

      (* real part of complex (or complex matrix) *)
      method private internal_re () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)


      method im () = self#check_args 1 "im" self#internal_im

      (* imaginary part of complex (or complex matrix) *)
      method private internal_im () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)


      method gamma () = self#check_args 1 "gamma" self#internal_gamma

      (* Euler gamma function *)
      method private internal_gamma () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method lngamma () = self#check_args 1 "lngamma" self#internal_lngamma

      (* log_e of Euler gamma function *)
      method private internal_lngamma () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method erf () = self#check_args 1 "erf" self#internal_erf

      (* error function *)
      method private internal_erf () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      method erfc () = self#check_args 1 "erfc" self#internal_erfc

      (* complementary error function *)
      method private internal_erfc () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "invalid argument"))


      (* factorial 
       * calls gamma function for float arguments, and jumps
       * to an interruptible exact implementation for integer
       * arguments.
       * This function is designed to be called multiple times
       * until it returns true.  If computation is aborted, the interface
       * should call abort_computation() to clean up. *)
      method fact () =
         match interr_args with
         |Fact_args (num, acc, el) ->
            if eq_big_int num zero_big_int then begin
               stack#push (RpcInt acc);
               interr_args <- NoArgs;
               true
            end else begin
               let next_num = pred_big_int num
               and next_acc = mult_big_int acc num in
               interr_args <- Fact_args (next_num, next_acc, el);
               false
            end
         |NoArgs ->
            if stack#length > 0 then begin
               self#backup ();
               self#evaln 1;
               let gen_el = stack#pop () in
               begin match gen_el with
               |RpcInt el ->
                  if sign_big_int el >= 0 then begin
                     interr_args <- Fact_args (el, unit_big_int, gen_el);
                     false
                  end else begin
                     stack#push gen_el;
                     raise (Invalid_argument "integer factorial requires non-negative argument")
                  end
               |RpcFloat el ->
                  begin try
                     stack#push (RpcFloat (Gsl_sf.gamma (el +. 1.0)));
                     true
                  with
                     Gsl_error.Gsl_exn (err, errstr) ->
                        (stack#push gen_el;
                        raise (Invalid_argument errstr))
                  end
               |RpcVariable s ->
                  stack#push gen_el;
                  let err_msg = 
                     Printf.sprintf "variable \"%s\" has not been evaluated" s 
                  in
                  raise (Invalid_argument err_msg)
               |_ ->
                  (stack#push gen_el;
                  raise (Invalid_argument "invalid argument"))
               end
            end else
               raise (Invalid_argument "empty stack")
         |_ ->
            (* shouldn't hit this point if interface is well-behaved *)
            self#abort_computation ();
            false


      method transpose () = self#check_args 1 "transpose" self#internal_transpose

      (* matrix transpose *)
      method private internal_transpose () =
         self#evaln 1;
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
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "transpose requires a matrix argument"))


      method mod_int () = self#check_args 2 "mod" self#internal_mod_int

      (* mod (remainder) *)
      method private internal_mod_int () =
         self#evaln 2;
         let gen_el2 = stack#pop () in
         let gen_el1 = stack#pop () in
         match gen_el1 with
         |RpcInt el1 ->
            begin match gen_el2 with
            |RpcInt el2 ->
               stack#push (RpcInt (mod_big_int el1 el2))
            |_ ->
               (stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "mod can only be applied to arguments of type integer"))
            end
         |RpcVariable s ->
            stack#push gen_el1;
            stack#push gen_el2;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "mod can only be applied to arguments of type integer"))


      method floor () = self#check_args 1 "floor" self#internal_floor

      (* floor function *)
      method private internal_floor () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcFloat el ->
            stack#push (RpcFloat (floor el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "floor can only be applied to real data"))


      method ceiling () = self#check_args 1 "ceiling" self#internal_ceiling

      (* ceiling function *)
      method private internal_ceiling () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcFloat el ->
            stack#push (RpcFloat (ceil el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "ceiling can only be applied to real data"))


      method to_int () = self#check_args 1 "toint" self#internal_to_int

      (* coerce to an integer type *)
      method private internal_to_int () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcInt el ->
            stack#push gen_el
         |RpcFloat el ->
            if (abs_float el) < 1e9 then
               stack#push (RpcInt (big_int_of_int (int_of_float el)))
            else
               (stack#push gen_el;
               raise (Invalid_argument "value is too large to convert to integer"))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "to_int can only be applied to real data"))


      method to_float () = self#check_args 1 "toreal" self#internal_to_float

      (* coerce to a floating-point type *)
      method private internal_to_float () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcInt el ->
            stack#push (RpcFloat (float_of_big_int el))
         |RpcVariable s ->
            stack#push gen_el;
            let err_msg = 
               Printf.sprintf "variable \"%s\" has not been evaluated" s 
            in
            raise (Invalid_argument err_msg)
         |_ ->
            (stack#push gen_el;
            raise (Invalid_argument "to_float can only be applied to integer data"))


      method solve_linear () = self#check_args 2 "solvelin"
      self#internal_solve_linear

      (* solve a linear system Ax = b, with input nxn matrix A and output nx1
       * matrix b *)
      method private internal_solve_linear () =
         Solvelin.solve_linear stack self#evaln


      method enter_pi () =
         stack#push (RpcFloat pi)

      method get_display_line line_num =
         stack#get_display_string line_num modes

      method get_fullscreen_display line_num =
         stack#get_fullscreen_display_string line_num modes

      (* fill in the display string lookup table *)
      method launch_fill_in_thread () =
         stack#launch_fill_in_thread ()


      method drop () = self#check_args 1 "drop" self#internal_drop 

      method private internal_drop () =
         let dummy = stack#pop () in 
         ()


      method swap () = self#check_args 2 "swap" self#internal_swap

      method private internal_swap () = stack#swap ()


      method clear () =
         self#backup ();
         for i = 1 to stack#length do
            let dummy = stack#pop () in ()
         done

      method push (v : orpie_data_t) =
         self#backup ();
         stack#push v

      method echo el_num =
         if el_num <= stack#length then
            stack#echo el_num
         else
            raise (Invalid_argument "cannot echo nonexistant element")

      method rolldown i =
         stack#rolldown i

      method rollup i =
         stack#rollup i

      method delete i = 
         stack#delete i

      method deleteN i =
         stack#deleteN i

      method keep i =
         stack#keep i

      method keepN i =
         stack#keepN i

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

      (* evaluate last n variables of the stack (internal use only) *)
      method private evaln (num : int) =
         (* grab the last n stack elements into a list *)
         let rec grab_elements el_lst n =
            if n > 0 then
               let next_el = stack#pop () in
               grab_elements (next_el :: el_lst) (pred n)
            else
               el_lst
         in
         (* eval the list elements one-by-one; if there
          * is a lookup failure, push everything back on the stack. *)
         let rec eval_elements el_lst =
            match el_lst with
            |[] ->
               ()
            |head :: tail ->
               begin match head with
               |RpcVariable s ->
                  begin try
                     let data = Hashtbl.find variables s in
                     stack#push data;
                     eval_elements tail
                  with
                     |Not_found ->
                        let err_msg = Printf.sprintf "variable \"%s\" is not bound" s in
                        List.iter stack#push el_lst;
                        raise (Invalid_argument err_msg)
                  end
               |_ ->
                  stack#push head;
                  eval_elements tail
               end
         in
         let raw_elements = grab_elements [] num in
         eval_elements raw_elements



      method eval () =
         if stack#length > 0 then begin
            (* the extra push and pop is necessary to be able to back up the
             * stack *only* when the eval() changes it *)
            let gen_el = stack#pop () in
            match gen_el with
            |RpcVariable s ->
               stack#push gen_el;
               self#backup ();
               self#evaln 1
            |_ ->
               stack#push gen_el
         end else
            raise (Invalid_argument "empty stack")
         

      method store () = self#check_args 2 "store" self#internal_store

      (* store in a variable *)
      method private internal_store () =
         let gen_el2 = stack#pop () in
         let gen_el1 = stack#pop () in
         match gen_el2 with
         |RpcVariable s ->
            begin match gen_el1 with
            |RpcVariable dummy ->
               stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "cannot store variables inside variables")
            |_ ->
               Hashtbl.remove variables s;
               Hashtbl.add variables s gen_el1
            end
         |_ ->
            stack#push gen_el1;
            stack#push gen_el2;
            raise (Invalid_argument "cannot store inside non-variable")


      method purge () = self#check_args 1 "purge" self#internal_purge

      (* clear a variable *)
      method private internal_purge () =
         let gen_el = stack#pop () in
         match gen_el with
         |RpcVariable s ->
            Hashtbl.remove variables s
         |_ ->
            stack#push gen_el;
            raise (Invalid_argument "only variables can be purged")



      (* greatest common divisor
       * This is an interruptible computation, and should be
       * called multiple times until it returns true.
       * If computation is aborted, the interface should call
       * abort_computation() to clean up. *)
      method gcd () =
         match interr_args with
         |Gcd_args (a, b, el1, el2) ->
            if eq_big_int b zero_big_int then begin
               stack#push (RpcInt a);
               interr_args <- NoArgs;
               true
            end else begin
               let a_mod_b = mod_big_int a b in
               interr_args <- Gcd_args (b, a_mod_b, el1, el2);
               false
            end
         |NoArgs ->
            if stack#length > 1 then begin
               self#backup ();
               self#evaln 2;
               let gen_el2 = stack#pop () in
               let gen_el1 = stack#pop () in
               begin match gen_el1 with
               |RpcInt a ->
                  begin match gen_el2 with
                  |RpcInt b ->
                     let abs_a = abs_big_int a
                     and abs_b = abs_big_int b in
                     interr_args <- Gcd_args (abs_a, abs_b, gen_el1, gen_el2);
                     false
                  |_ ->
                     stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "gcd requires integer arguments")
                  end
               |_ ->
                  stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "gcd requires integer arguments")
               end
            end else
               raise (Invalid_argument "insufficient arguments for gcd")
         |_ ->
            (* shouldn't hit this point if interface is well-behaved *)
            self#abort_computation ();
            false
            

      (* least common multiple
       * This is an interruptible computation, and should be
       * called multiple times until it returns true.
       * If computation is aborted, the interface should call
       * abort_computation() to clean up. *)
      method lcm () =
         match interr_args with
         |Lcm_args (coeff, a, b, el1, el2) ->
            if eq_big_int b zero_big_int then begin
               let result = div_big_int coeff a in
               stack#push (RpcInt result);
               interr_args <- NoArgs;
               true
            end else begin
               let a_mod_b = mod_big_int a b in
               interr_args <- Lcm_args (coeff, b, a_mod_b, el1, el2);
               false
            end
         |NoArgs ->
            if stack#length > 1 then begin
               self#backup ();
               self#evaln 2;
               let gen_el2 = stack#pop () in
               let gen_el1 = stack#pop () in
               begin match gen_el1 with
               |RpcInt a ->
                  begin match gen_el2 with
                  |RpcInt b ->
                     let coeff = mult_big_int a b
                     and abs_a = abs_big_int a
                     and abs_b = abs_big_int b in
                     interr_args <- Lcm_args (coeff, abs_a, abs_b, gen_el1, gen_el2);
                     false
                  |_ ->
                     stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "lcm requires integer arguments")
                  end
               |_ ->
                  stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "lcm requires integer arguments")
               end
            end else
               raise (Invalid_argument "insufficient arguments for lcm")
         |_ ->
            (* shouldn't hit this point if interface is well-behaved *)
            self#abort_computation ();
            false


      (* binomial coefficient
       * For a float argument, this is computed using lngamma in order to avoid
       * overflow.  For an integer argument, jump to an interruptible
       * exact arithmetic value. *)
      method binom () =
         match interr_args with
         |Binom_args (n, k, num, denom, el1, el2) ->
            if eq_big_int k zero_big_int then begin
               let result = div_big_int num denom in
               stack#push (RpcInt result);
               interr_args <- NoArgs;
               true
            end else begin
               let nmk = sub_big_int n k in
               let new_num = mult_big_int num (succ_big_int nmk) in
               let new_denom = mult_big_int denom k in
               interr_args <- Binom_args (n, (pred_big_int k), new_num,
               new_denom, el1, el2);
               false
            end
         |NoArgs ->
            if stack#length > 1 then begin
               self#backup ();
               self#evaln 2;
               let gen_el2 = stack#pop () in
               let gen_el1 = stack#pop () in
               begin match gen_el1 with
               |RpcInt el1 ->
                  begin match gen_el2 with
                  |RpcInt el2 ->
                     if sign_big_int el1 >= 0 && sign_big_int el2 >= 0 then
                        if ge_big_int el1 el2 then
                           (* save a little computation via a binomial identity *)
                           let nmk = sub_big_int el1 el2 in
                           if lt_big_int nmk el2 then begin
                              interr_args <- Binom_args (el1, nmk, unit_big_int,
                              unit_big_int, gen_el1, gen_el2);
                              false
                           end else begin
                              interr_args <- Binom_args (el1, el2, unit_big_int,
                              unit_big_int, gen_el1, gen_el2);
                              false
                           end
                        else
                           (stack#push gen_el1;
                           stack#push gen_el2;
                           raise (Invalid_argument "first argument to binom must be >= second argument"))
                     else
                        (stack#push gen_el1;
                        stack#push gen_el2;
                        raise (Invalid_argument "integer binom requires nonnegative arguments"))
                  |_ ->
                     (stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "binom requires either two integer or two real arguments"))
                  end
               |RpcFloat el1 ->
                  begin match gen_el2 with
                  |RpcFloat el2 ->
                     begin try
                        let log_coeff = (Gsl_sf.lngamma (el1 +. 1.0)) -.
                        (Gsl_sf.lngamma (el2 +. 1.0)) -. 
                        (Gsl_sf.lngamma (el1 -. el2 +. 1.0)) in
                        stack#push (RpcFloat (exp log_coeff));
                        true
                     with
                        Gsl_error.Gsl_exn (err, errstr) ->
                           (stack#push gen_el1;
                            stack#push gen_el2;
                           raise (Invalid_argument errstr))
                     end
                  |_ ->
                     (stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "binom requires either two integer or two real arguments"))
                  end
               |RpcVariable s ->
                  stack#push gen_el1;
                  stack#push gen_el2;
                  let err_msg = 
                     Printf.sprintf "variable \"%s\" has not been evaluated" s 
                  in
                  raise (Invalid_argument err_msg)
               |_ ->
                  (stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "mod can only be applied to arguments of type integer"))
               end
            end else
               raise (Invalid_argument "insufficient arguments for binom")
         |_ ->
            (* shouldn't hit this point if interface is well-behaved *)
            self#abort_computation ();
            false


         method total () = self#check_args 1 "total" self#internal_total

         (* single-variable statistics: total *)
         method private internal_total () =
            self#evaln 1;
            let gen_el = stack#pop () in
            match gen_el with
            |RpcFloatMatrix mat ->
               (* multiply on the left by a row of ones *)
               let n, m = Gsl_matrix.dims mat in
               let ones_arr = Array.make n 1.0 in
               let ones = Gsl_matrix.of_array ones_arr 1 n in
               let result = Gsl_matrix.create 1 m in
               Gsl_blas.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans 1.0 ones mat
               0.0 result;
               stack#push (RpcFloatMatrix result)
            |_ ->
               stack#push gen_el;
               raise (Invalid_argument "total can only be applied to real matrices")


         method mean () = self#check_args 1 "mean" self#internal_mean

         (* single-variable statistics: sample mean *)
         method private internal_mean () =
            self#evaln 1;
            let gen_el = stack#pop () in
            match gen_el with
            |RpcFloatMatrix mat ->
               (* multiply on the left by a row of ones, divided by n *)
               let n, m = Gsl_matrix.dims mat in
               let ones_arr = Array.make n (1.0 /. (float_of_int n)) in
               let ones = Gsl_matrix.of_array ones_arr 1 n in
               let result = Gsl_matrix.create 1 m in
               Gsl_blas.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans 1.0 ones mat
               0.0 result;
               stack#push (RpcFloatMatrix result)
            |_ ->
               stack#push gen_el;
               raise (Invalid_argument "total can only be applied to real matrices")


         method sum_squares () = self#check_args 1 "sumsq"
         self#internal_sum_squares

         (* single-variable statistics: sum of squares *)
         method private internal_sum_squares () =
            self#evaln 1;
            let gen_el = stack#pop () in
            match gen_el with
            |RpcFloatMatrix mat ->
               let n, m = Gsl_matrix.dims mat in
               let result = Gsl_matrix.create 1 m in
               for col = 0 to pred m do
                  result.{0, col} <- 0.0;
                  for row = 0 to pred n do
                     let squared_el = mat.{row, col} *. mat.{row, col} in
                     result.{0, col} <- result.{0, col} +. squared_el
                  done
               done;
               stack#push (RpcFloatMatrix result)
            |_ ->
               stack#push gen_el;
               raise (Invalid_argument "sumsq can only be applied to real matrices")


         method variance_unbiased () = self#check_args 1 "var"
         self#internal_variance_unbiased

         (* single-variable statistics: bias-corrected sample variance *)
         method private internal_variance_unbiased () =
            let gen_el = stack#peek 1 in
            match gen_el with
            |RpcFloatMatrix mat ->
               let n, m = Gsl_matrix.dims mat in
               if n >= 2 then begin
                  self#internal_variance_biased ();
                  let n_over_nm1 = (float_of_int n) /. (float_of_int (pred n)) in
                  stack#push (RpcFloat n_over_nm1);
                  self#internal_mult ()
               end else
                  raise (Invalid_argument "insufficient matrix rows for unbiased sample variance")
            |_ ->
               raise (Invalid_argument "varbias can only be applied to real matrices")


         method variance_biased () = self#check_args 1 "varbias"
         self#internal_variance_biased

         (* single-variable statistics: sample variance (biased) *)
         method private internal_variance_biased () =
            self#evaln 1;
            let gen_el = stack#peek 1 in
            match gen_el with
            |RpcFloatMatrix mat ->
               let n, m = Gsl_matrix.dims mat in
               let float_n = float_of_int n in
               (* computes variance as E[X^2] - E[X]^2 *)
               self#internal_dup ();
               self#internal_sum_squares ();
               stack#push (RpcFloat float_n);
               self#internal_div ();
               self#internal_swap ();
               self#internal_mean ();
               self#internal_sum_squares ();
               self#internal_sub ()
            |_ ->
               raise (Invalid_argument "var can only be applied to real matrices")


         method standard_deviation_unbiased () = self#check_args 1 "stdev"
         self#internal_standard_deviation_unbiased

         (* single-variable statistics: unbiased sample standard deviation *)
         method private internal_standard_deviation_unbiased () =
            self#internal_variance_unbiased ();
            let gen_el = stack#pop () in
            match gen_el with
            |RpcFloatMatrix mat ->
               let n, m = Gsl_matrix.dims mat in
               let result = Gsl_matrix.create 1 m in
               for col = 0 to pred m do
                  result.{0, col} <- sqrt mat.{0, col}
               done;
               stack#push (RpcFloatMatrix result)
            |_ -> ()
               

         method standard_deviation_biased () = self#check_args 1 "stdevbias"
         self#internal_standard_deviation_biased

         (* single-variable statistics: unbiased sample standard deviation *)
         method private internal_standard_deviation_biased () =
            self#internal_variance_biased ();
            let gen_el = stack#pop () in
            match gen_el with
            |RpcFloatMatrix mat ->
               let n, m = Gsl_matrix.dims mat in
               let result = Gsl_matrix.create 1 m in
               for col = 0 to pred m do
                  result.{0, col} <- sqrt mat.{0, col}
               done;
               stack#push (RpcFloatMatrix result)
            |_ -> ()


         method minimum () = self#check_args 1 "min" self#internal_minimum

         (* single-variable statistics: minimum of set *)
         method private internal_minimum () = self#min_or_max true ()

         method maximum () = self#check_args 1 "max" self#internal_maximum

         (* single-variable statistics: maximum of set *)
         method private internal_maximum () = self#min_or_max false ()

         method private min_or_max operation_is_min () =
            self#evaln 1;
            let gen_el = stack#pop () in
            match gen_el with
            |RpcFloatMatrix mat ->
               let n, m = Gsl_matrix.dims mat in
               let result = Gsl_matrix.create 1 m in
               for col = 0 to pred m do
                  result.{0, col} <- mat.{0, col};
                  for row = 1 to pred n do
                     if operation_is_min then
                        if mat.{row, col} < result.{0, col} then
                           result.{0, col} <- mat.{row, col}
                        else
                           ()
                     else
                        if mat.{row, col} > result.{0, col} then
                           result.{0, col} <- mat.{row, col}
                        else
                           ()
                  done
               done;
               stack#push (RpcFloatMatrix result)
            |_ ->
               stack#push gen_el;
               raise (Invalid_argument "min can only be applied to real matrices")


         method upper_tail_prob_normal () = self#check_args 3 "utpn"
         self#internal_upper_tail_prob_normal

         method private internal_upper_tail_prob_normal () =
            self#evaln 3;
            let gen_el3 = stack#pop () in
            let gen_el2 = stack#pop () in
            let gen_el1 = stack#pop () in
            let get_float_args gen_el =
               match gen_el with
               |RpcInt i_el ->
                  float_of_big_int i_el
               |RpcFloat el ->
                  el
               |_ ->
                  stack#push gen_el1;
                  stack#push gen_el2;
                  stack#push gen_el3;
                  raise (Invalid_argument "utpn requires real scalar arguments")
            in
            let mean = get_float_args gen_el1
            and var = get_float_args gen_el2
            and cutoff = get_float_args gen_el3 in
            if var <= 0.0 then begin
               stack#push gen_el1;
               stack#push gen_el2;
               stack#push gen_el3;
               raise (Invalid_argument "variance argument to utpn must be positive")
            end else begin
               let arg = (cutoff -. mean) /. (sqrt (2.0 *. var)) in
               stack#push (RpcFloat arg);
               self#internal_erfc ();
               stack#push (RpcFloat 0.5);
               self#internal_mult ()
            end



(*      method print_stack () =
         let print_el line_num el = Printf.printf "%2d:  %s\n" line_num el in
         for i = stack#length downto 1 do
            print_el i (stack#get_display_line i modes)
         done
*)

   end;;








(* arch-tag: DO_NOT_CHANGE_548916d4-da42-49b4-8941-c0d42306f1b7 *)
