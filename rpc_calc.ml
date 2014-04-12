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

(* rpc_calc.ml
 * This file defines Orpie's underlying calculator object.  All calculator
 * functions and commands have a corresponding method in this object. *)

open Rpc_stack;;
open Gsl_assist;;
open Big_int;;

type interruptable_args_t =
   | Gcd_args of big_int * big_int * orpie_data_t * orpie_data_t
   | Lcm_args of big_int * big_int * big_int * orpie_data_t * orpie_data_t
   | Fact_args of big_int * big_int * orpie_data_t
   | Binom_args of big_int * big_int * big_int * 
                   big_int * orpie_data_t * orpie_data_t
   | Perm_args of big_int * big_int * big_int *
                  orpie_data_t * orpie_data_t
   | NoArgs;;

let pi = 3.14159265358979323846;;

let c_of_f ff = {
   Complex.re = ff;
   Complex.im = 0.0
}

class rpc_calc conserve_memory =
   object(self)
      val mutable stack = new rpc_stack conserve_memory
      val mutable backup_stack = new rpc_stack conserve_memory
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

      method get_state () =
         (modes, variables, stack#get_state ())

      method set_state (m, v, s_op) =
         begin match s_op with
         |Some st -> stack#set_state st
         |None    -> ()
         end;
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
         |Perm_args (n, term, partial, el1, el2) ->
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
         |RpcFloatUnit (el, uu) ->
            stack#push (RpcFloatUnit (~-. el, uu))
         |RpcComplexUnit (el, uu) ->
            stack#push (RpcComplexUnit (Complex.neg el, uu))
         |RpcFloatMatrixUnit (el, uu) ->
            let copy = Gsl_matrix.copy el in
            (Gsl_matrix.scale copy (-1.0);
            stack#push (RpcFloatMatrixUnit (copy, uu)))
         |RpcComplexMatrixUnit (el, uu) ->
            let copy = Gsl_matrix_complex.copy el in
            (Gsl_matrix_complex.scale copy {Complex.re=(-1.0); Complex.im=0.0};
            stack#push (RpcComplexMatrixUnit (copy, uu)))
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
         |RpcFloatUnit (el, uu) ->
            stack#push (RpcFloatUnit (el *. el, Units.pow uu 2.))
         |RpcComplexUnit (el, uu) ->
            stack#push (RpcComplexUnit (Complex.mul el el, Units.pow uu 2.))
         |RpcFloatMatrixUnit (el, uu) ->
            let n, m = (Gsl_matrix.dims el) in
            if n = m then
               let result = Gsl_matrix.create n m in
               (Gsl_blas.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans 1.0 el el 0.0 result;
               stack#push (RpcFloatMatrixUnit (result, Units.pow uu 2.)))
            else
               (stack#push gen_el;
               raise (Invalid_argument "matrix is non-square"))
         |RpcComplexMatrixUnit (el, uu) ->
            let n, m = (Gsl_matrix_complex.dims el) in
            if m = n then
               let result = Gsl_matrix_complex.create n m in
               Gsl_blas.Complex.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
               Complex.one el el Complex.zero result;
               stack#push (RpcComplexMatrixUnit (result, Units.pow uu 2.))
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
         |RpcFloatUnit (el, uu) ->
            if el < 0.0 then
               let cc = c_of_f el in
               stack#push (RpcComplexUnit (Complex.sqrt cc, Units.pow uu 0.5))
            else
               stack#push (RpcFloatUnit (sqrt el, Units.pow uu 0.5))
         |RpcComplexUnit (el, uu) ->
            stack#push (RpcComplexUnit (Complex.sqrt el, Units.pow uu 0.5))
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
         |RpcFloatUnit (el, uu) ->
            stack#push (RpcFloatUnit (abs_float el, uu))
         |RpcComplexUnit (el, uu) ->
            stack#push (RpcFloatUnit (Gsl_complex.abs el, uu))
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
         |RpcComplexUnit (el, uu) ->
            begin match modes.angle with
            |Rad ->
               let (f, u) = funit_of_float (Gsl_complex.arg el) in
               stack#push (RpcFloatUnit (f, u))
            |Deg ->
               let (f, u) = funit_of_float (180.0 /. pi *.  (Gsl_complex.arg el)) in
               stack#push (RpcFloatUnit (f, u))
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
            let (f, u) = funit_of_float (exp (float_of_big_int el)) in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot exponentiate dimensioned value"
            end else
               let (f, u) = funit_of_float (exp el) in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot exponentiate dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.exp el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float (log (float_of_big_int el)) in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute logarithm of dimensioned value"
            end else if el >= 0.0 then begin
               let (f, u) = funit_of_float (log el) in
               stack#push (RpcFloatUnit (f, u))
            end else
               let c_arg = c_of_f el in
               let (c, u) = cunit_of_cpx (Gsl_complex.log c_arg) in
               stack#push (RpcComplexUnit (c, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute logarithm of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.log el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float (10.0 ** (float_of_big_int el)) in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot exponentiate dimensioned value"
            end else
               let (f, u) = funit_of_float (10.0 ** el) in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot exponentiate dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Complex.pow (cmpx_of_float 10.0) el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float (log10 (float_of_big_int el)) in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute logarithm of dimensioned value"
            end else if el >= 0.0 then begin
               let (f, u) = funit_of_float (log10 el) in
               stack#push (RpcFloatUnit (f, u))
            end else
               let c_arg = c_of_f el in
               let (c, u) = cunit_of_cpx (Gsl_complex.log10 c_arg) in
               stack#push (RpcComplexUnit (c, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute logarithm of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.log10 el) in
               stack#push (RpcComplexUnit (c, u))
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
         |RpcFloatUnit (el, uu) ->
            stack#push (RpcFloatUnit (el, uu))
         |RpcComplexUnit (el, uu) ->
            stack#push (RpcComplexUnit (Gsl_complex.conjugate el, uu))
         |RpcFloatMatrixUnit (el, uu) ->
            stack#push (RpcFloatMatrixUnit (el, uu))
         |RpcComplexMatrixUnit (el, uu) ->
            (* element-by-element conjugation *)
            let rows, cols = Gsl_matrix_complex.dims el and
            arr = Gsl_matrix_complex.to_array el in
            let conj_arr = Array.map Gsl_complex.conjugate arr in
            let conj_mat = Gsl_matrix_complex.of_array conj_arr rows cols in
            stack#push (RpcComplexMatrixUnit (conj_mat, uu))
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
            let (f, u) = funit_of_float
               begin
                  match modes.angle with
                  |Rad -> sin (float_of_big_int el)
                  |Deg -> sin (pi /. 180.0 *. (float_of_big_int el))
               end
            in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute sine of dimensioned value"
            end else
               let (f, u) = funit_of_float
                  begin
                     match modes.angle with
                     |Rad -> sin el
                     |Deg -> sin (pi /. 180.0 *. el)
                  end
               in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute sine of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.sin el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float
               begin
                  match modes.angle with
                  |Rad -> cos (float_of_big_int el)
                  |Deg -> cos (pi /. 180.0 *. (float_of_big_int el))
               end
            in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute cosine of dimensioned value"
            end else
               let (f, u) = funit_of_float
                  begin
                     match modes.angle with
                     |Rad -> cos el
                     |Deg -> cos (pi /. 180.0 *. el)
                  end
               in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute cosine of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.cos el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float
               begin
                  match modes.angle with
                  |Rad -> tan (float_of_big_int el)
                  |Deg -> tan (pi /. 180.0 *. (float_of_big_int el))
               end
            in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute tangent of dimensioned value"
            end else
               let (f, u) = funit_of_float
                  begin
                     match modes.angle with
                     |Rad -> tan el
                     |Deg -> tan (pi /. 180.0 *. el)
                  end
               in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute tangent of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.tan el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float
               begin
                  match modes.angle with
                  |Rad -> asin (float_of_big_int el)
                  |Deg -> 180.0 /. pi *. asin (float_of_big_int el)
               end
            in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute arcsine of dimensioned value"
            end else
               let (f, u) = funit_of_float
                  begin
                     match modes.angle with
                     |Rad -> asin el
                     |Deg -> 180.0 /. pi *. asin el
                  end
               in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute arcsine of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.arcsin el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float
               begin
                  match modes.angle with
                  |Rad -> acos (float_of_big_int el)
                  |Deg -> 180.0 /. pi *. acos (float_of_big_int el)
               end
            in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute arccos of dimensioned value"
            end else
               let (f, u) = funit_of_float
                  begin
                     match modes.angle with
                     |Rad -> acos el
                     |Deg -> 180.0 /. pi *. acos el
                  end
               in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute arccos of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.arccos el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float
               begin
                  match modes.angle with
                  |Rad -> atan (float_of_big_int el)
                  |Deg -> 180.0 /. pi *. atan (float_of_big_int el)
               end
            in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute arctan of dimensioned value"
            end else
               let (f, u) = funit_of_float
                  begin
                     match modes.angle with
                     |Rad -> atan el
                     |Deg -> 180.0 /. pi *. atan el
                  end
               in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute arctan of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.arctan el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float (sinh (float_of_big_int el)) in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute sinh of dimensioned value"
            end else
               let (f, u) = funit_of_float (sinh el) in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute sinh of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.sinh el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float (cosh (float_of_big_int el)) in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute cosh of dimensioned value"
            end else
               let (f, u) = funit_of_float (cosh el) in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute cosh of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.cosh el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float (tanh (float_of_big_int el)) in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute tanh of dimensioned value"
            end else
               let (f, u) = funit_of_float (tanh el) in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute tanh of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.tanh el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float (Gsl_math.asinh (float_of_big_int el)) in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute asinh of dimensioned value"
            end else
               let (f, u) = funit_of_float (Gsl_math.asinh el) in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute asinh of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.arcsinh el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float (Gsl_math.acosh (float_of_big_int el)) in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute acosh of dimensioned value"
            end else
               let (f, u) = funit_of_float (Gsl_math.acosh el) in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute acosh of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.arccosh el) in
               stack#push (RpcComplexUnit (c, u))
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
            let (f, u) = funit_of_float (Gsl_math.atanh (float_of_big_int el)) in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute atanh of dimensioned value"
            end else
               let (f, u) = funit_of_float (Gsl_math.atanh el) in
               stack#push (RpcFloatUnit (f, u))
         |RpcComplexUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute atanh of dimensioned value"
            end else
               let (c, u) = cunit_of_cpx (Gsl_complex.arctanh el) in
               stack#push (RpcComplexUnit (c, u))
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
         |RpcFloatUnit (el, uu) ->
            stack#push gen_el
         |RpcComplexUnit (el, uu) ->
            stack#push (RpcFloatUnit (el.Complex.re, uu))
         |RpcFloatMatrixUnit (el, uu) ->
            stack#push gen_el
         |RpcComplexMatrixUnit (el, uu) ->
            let n, m = Gsl_matrix_complex.dims el
            and carr = Gsl_matrix_complex.to_array el in
            let farr = Array.make (n * m) 0.0 in
            for i = 0 to pred (n * m) do
               farr.(i) <- carr.(i).Complex.re
            done;
            stack#push (RpcFloatMatrixUnit (Gsl_matrix.of_array farr n m, uu))
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
         |RpcFloatUnit (el, uu) ->
            stack#push (RpcFloatUnit (0.0, uu))
         |RpcComplexUnit (el, uu) ->
            stack#push (RpcFloatUnit (el.Complex.im, uu))
         |RpcFloatMatrixUnit (el, uu) ->
            let n, m = Gsl_matrix.dims el in
            let farr = Array.make (n * m) 0.0 in
            stack#push (RpcFloatMatrixUnit (Gsl_matrix.of_array farr n m, uu))
         |RpcComplexMatrixUnit (el, uu) ->
            let n, m = Gsl_matrix_complex.dims el
            and carr = Gsl_matrix_complex.to_array el in
            let farr = Array.make (n * m) 0.0 in
            for i = 0 to pred (n * m) do
               farr.(i) <- carr.(i).Complex.im
            done;
            stack#push (RpcFloatMatrixUnit (Gsl_matrix.of_array farr n m, uu))
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
               let (f, u) = funit_of_float (Gsl_sf.gamma (float_of_big_int el)) in
               stack#push (RpcFloatUnit (f, u))
            with
               Gsl_error.Gsl_exn (err, errstr) ->
                  (stack#push gen_el;
                  raise (Invalid_argument errstr))
            end
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute gamma of dimensioned value"
            end else
               begin try
                  let (f, u) = funit_of_float (Gsl_sf.gamma el) in
                  stack#push (RpcFloatUnit (f, u))
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
               let (f, u) = funit_of_float (Gsl_sf.lngamma (float_of_big_int el)) in
               stack#push (RpcFloatUnit (f, u))
            with
               Gsl_error.Gsl_exn (err, errstr) ->
                  (stack#push gen_el;
                  raise (Invalid_argument errstr))
            end
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute lngamma of dimensioned value"
            end else
               begin try
                  let (f, u) = funit_of_float (Gsl_sf.lngamma el) in
                  stack#push (RpcFloatUnit (f, u))
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
               let (f, u) = funit_of_float (Gsl_sf.erf (float_of_big_int el)) in
               stack#push (RpcFloatUnit (f, u))
            with
               Gsl_error.Gsl_exn (err, errstr) ->
                  (stack#push gen_el;
                  raise (Invalid_argument errstr))
            end
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute error function of dimensioned value"
            end else
               begin try
                  let (f, u) = funit_of_float (Gsl_sf.erf el) in
                  stack#push (RpcFloatUnit (f, u))
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
               let (f, u) = funit_of_float (Gsl_sf.erfc (float_of_big_int el)) in
               stack#push (RpcFloatUnit (f, u))
            with
               Gsl_error.Gsl_exn (err, errstr) ->
                  (stack#push gen_el;
                  raise (Invalid_argument errstr))
            end
         |RpcFloatUnit (el, uu) ->
            if uu <> Units.empty_unit then begin
               stack#push gen_el;
               raise_invalid "cannot compute erfc of dimensioned value"
            end else
               begin try
                  let (f, u) = funit_of_float (Gsl_sf.erfc el) in
                  stack#push (RpcFloatUnit (f, u))
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
               |RpcFloatUnit (el, uu) ->
                  if uu <> Units.empty_unit then begin
                     stack#push gen_el;
                     raise_invalid "cannot compute factorial of dimensioned value"
                  end else
                     begin try
                        let (f, u) = funit_of_float (Gsl_sf.gamma (el +. 1.0)) in
                        stack#push (RpcFloatUnit (f, u));
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
         |RpcFloatMatrixUnit (el, uu) ->
            let n, m = (Gsl_matrix.dims el) in
            let trans_mat = Gsl_matrix.create m n in
            Gsl_matrix.transpose trans_mat el;
            stack#push (RpcFloatMatrixUnit (trans_mat, uu))
         |RpcComplexMatrixUnit (el, uu) ->
            let n, m = (Gsl_matrix_complex.dims el) in
            let trans_mat = Gsl_matrix_complex.create m n in
            Gsl_matrix_complex.transpose trans_mat el;
            stack#push (RpcComplexMatrixUnit (trans_mat, uu))
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
                if eq_big_int el2 zero_big_int then begin
                   stack#push gen_el1;
                   stack#push gen_el2;
                   raise (Invalid_argument "division by zero")
                end else
                   stack#push (RpcInt (mod_big_int el1 el2))
            |RpcFloatUnit (ff2, uu2) ->
               if uu2 <> Units.empty_unit then begin
                  stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "cannot compute mod of dimensioned values")
               end else if (abs_float ff2) < 1e9 then
                  let bi_el2 = big_int_of_int (int_of_float ff2) in
                  if eq_big_int bi_el2 zero_big_int then begin
                     stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "division by zero")
                  end else
                     stack#push (RpcInt (mod_big_int el1 bi_el2))
               else begin
                  stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "real argument is too large to convert to integer")
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
               raise (Invalid_argument "mod can only be applied to arguments of type integer or real"))
            end
         |RpcFloatUnit (ff1, uu1) ->
            if uu1 <> Units.empty_unit then begin
               stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "cannot compute mod of dimensioned values")
            end else if (abs_float ff1) < 1e9 then begin
               let bi_el1 = big_int_of_int (int_of_float ff1) in
               begin match gen_el2 with
               |RpcInt el2 ->
                  if eq_big_int el2 zero_big_int then begin
                     stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "division by zero")
                  end else
                     stack#push (RpcInt (mod_big_int bi_el1 el2))
               |RpcFloatUnit (ff2, uu2) ->
                  if uu2 <> Units.empty_unit then begin
                     stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "cannot compute mod of dimensioned values")
                  end else if (abs_float ff2) < 1e9 then
                     let bi_el2 = big_int_of_int (int_of_float ff2) in
                     if eq_big_int bi_el2 zero_big_int then begin
                        stack#push gen_el1;
                        stack#push gen_el2;
                        raise (Invalid_argument "division by zero")
                     end else
                        stack#push (RpcInt (mod_big_int bi_el1 bi_el2))
                  else begin
                     stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "real argument is too large to convert to integer")
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
                  raise (Invalid_argument "mod can only be applied to arguments of type integer or real"))
               end
            end else begin
               stack#push gen_el1;
               stack#push gen_el2;
               raise (Invalid_argument "real argument is too large to convert to integer")
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
            raise (Invalid_argument "mod can only be applied to arguments of type integer or real"))


      method floor () = self#check_args 1 "floor" self#internal_floor

      (* floor function *)
      method private internal_floor () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcFloatUnit (el, uu) -> 
            stack#push (RpcFloatUnit (floor el, uu))
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
         |RpcFloatUnit (el, uu) ->
            stack#push (RpcFloatUnit (ceil el, uu))
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
         |RpcFloatUnit (ff, uu) ->
            if (abs_float ff) < 1e9 then
               stack#push (RpcInt (big_int_of_int (int_of_float ff)))
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
            let (f, u) = funit_of_float (float_of_big_int el) in
            stack#push (RpcFloatUnit (f, u))
         |RpcFloatMatrixUnit (el, uu) ->
            let n, m = Gsl_matrix.dims el in
            if n = 1 && m = 1 then
               stack#push (RpcFloatUnit (el.{0, 0}, uu))
            else begin
               stack#push gen_el;
               raise_invalid "matrix argument of to_float must be 1x1"
            end
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
         self#backup ();
         let (f, u) = funit_of_float pi in
         stack#push (RpcFloatUnit (f, u))

      method get_display_line line_num =
         stack#get_display_string line_num modes

      method get_fullscreen_display line_num =
         stack#get_fullscreen_display_string line_num modes

      (* fill in the display string lookup table *)
      method launch_fill_in_thread () =
         stack#launch_fill_in_thread ()


      method drop () = self#check_args 1 "drop" self#internal_drop 

      method private internal_drop () =
         let _ = stack#pop () in 
         ()


      method swap () = self#check_args 2 "swap" self#internal_swap

      method private internal_swap () = stack#swap ()


      method clear () =
         self#backup ();
         for i = 1 to stack#length do
            let _ = stack#pop () in ()
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
         stack#push (RpcFloatUnit (f, Units.empty_unit))

      method enter_cmpx c =
         stack#push (RpcComplexUnit (c, Units.empty_unit))

      method enter_fmat fm uu =
         stack#push (RpcFloatMatrixUnit (fm, uu))

      method enter_cmat cm uu =
         stack#push (RpcComplexMatrixUnit (cm, uu))

      method enter_const cc uu =
         stack#push (RpcFloatUnit (cc, uu))

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
                  |RpcFloatUnit (ff2, uu2) ->
                     if uu2 <> Units.empty_unit then begin
                        stack#push gen_el1;
                        stack#push gen_el2;
                        raise (Invalid_argument "cannot compute gcd of dimensioned values")
                     end else if (abs_float ff2) < 1e9 then begin
                        let abs_a = abs_big_int a
                        and abs_b = abs_big_int (big_int_of_int (int_of_float ff2)) in
                        interr_args <- Gcd_args (abs_a, abs_b, gen_el1, gen_el2);
                        false
                     end else begin
                        stack#push gen_el1;
                        stack#push gen_el2;
                        raise (Invalid_argument "real argument is too large to convert to integer")
                     end
                  |RpcVariable s ->
                     stack#push gen_el1;
                     stack#push gen_el2;
                     let err_msg = 
                        Printf.sprintf "variable \"%s\" has not been evaluated" s 
                     in
                     raise (Invalid_argument err_msg)
                  |_ ->
                     stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "gcd requires integer or real arguments")
                  end
               |RpcFloatUnit (ff1, uu1) ->
                  if uu1 <> Units.empty_unit then begin
                     stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "cannot compute gcd of dimensioned values")
                  end else if (abs_float ff1) < 1e9 then begin
                     let abs_a = abs_big_int (big_int_of_int (int_of_float ff1)) in
                     begin match gen_el2 with
                     |RpcInt b ->
                        let abs_b = abs_big_int b in
                        interr_args <- Gcd_args (abs_a, abs_b, gen_el1, gen_el2);
                        false
                     |RpcFloatUnit (ff2, uu2) ->
                        if uu2 <> Units.empty_unit then begin
                           stack#push gen_el1;
                           stack#push gen_el2;
                           raise (Invalid_argument "cannot compute gcd of dimensioned values")
                        end else if (abs_float ff2) < 1e9 then begin
                           let abs_b = abs_big_int (big_int_of_int (int_of_float ff2)) in
                           interr_args <- Gcd_args (abs_a, abs_b, gen_el1, gen_el2);
                           false
                        end else begin
                           stack#push gen_el1;
                           stack#push gen_el2;
                           raise (Invalid_argument "real argument is too large to convert to integer")
                        end
                     |RpcVariable s ->
                        stack#push gen_el1;
                        stack#push gen_el2;
                        let err_msg = 
                           Printf.sprintf "variable \"%s\" has not been evaluated" s 
                        in
                        raise (Invalid_argument err_msg)
                     |_ ->
                        stack#push gen_el1;
                        stack#push gen_el2;
                        raise (Invalid_argument "gcd requires integer or real arguments")
                     end
                  end else begin
                     stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "real argument is too large to convert to integer")
                  end
               |RpcVariable s ->
                  stack#push gen_el1;
                  stack#push gen_el2;
                  let err_msg = 
                     Printf.sprintf "variable \"%s\" has not been evaluated" s 
                  in
                  raise (Invalid_argument err_msg)
               |_ ->
                  stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "gcd requires integer or real arguments")
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
                  |RpcFloatUnit (ff2, uu2) ->
                     if uu2 <> Units.empty_unit then begin
                        stack#push gen_el1;
                        stack#push gen_el2;
                        raise (Invalid_argument "cannot compute lcm of dimensioned values")
                     end else if (abs_float ff2) < 1e9 then begin
                        let bi_b  = big_int_of_int (int_of_float ff2) in
                        let abs_a = abs_big_int a
                        and abs_b = abs_big_int bi_b in
                        let coeff = mult_big_int a bi_b in
                        interr_args <- Lcm_args (coeff, abs_a, abs_b, gen_el1, gen_el2);
                        false
                     end else begin
                        stack#push gen_el1;
                        stack#push gen_el2;
                        raise (Invalid_argument "real argument is too large to convert to integer")
                     end
                  |RpcVariable s ->
                     stack#push gen_el1;
                     stack#push gen_el2;
                     let err_msg = 
                        Printf.sprintf "variable \"%s\" has not been evaluated" s 
                     in
                     raise (Invalid_argument err_msg)
                  |_ ->
                     stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "lcm requires integer or real arguments")
                  end
               |RpcFloatUnit (ff1, uu1) ->
                  if uu1 <> Units.empty_unit then begin
                     stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "cannot compute lcm of dimensioned values")
                  end else if (abs_float ff1) < 1e9 then begin
                     let bi_a  = big_int_of_int (int_of_float ff1) in
                     let abs_a = abs_big_int bi_a in
                     begin match gen_el2 with
                     |RpcInt b ->
                        let coeff = mult_big_int bi_a b
                        and abs_b = abs_big_int b in
                        interr_args <- Lcm_args (coeff, abs_a, abs_b, gen_el1, gen_el2);
                        false
                     |RpcFloatUnit (ff2, uu2) ->
                        if uu2 <> Units.empty_unit then begin
                           stack#push gen_el1;
                           stack#push gen_el2;
                           raise (Invalid_argument "cannot compute lcm of dimensioned values")
                        end else if (abs_float ff2) < 1e9 then begin
                           let bi_b = big_int_of_int (int_of_float ff2) in
                           let abs_b = abs_big_int bi_b in
                           let coeff = mult_big_int bi_a bi_b in
                           interr_args <- Lcm_args (coeff, abs_a, abs_b, gen_el1, gen_el2);
                           false
                        end else begin
                           stack#push gen_el1;
                           stack#push gen_el2;
                           raise (Invalid_argument "real argument is too large to convert to integer")
                        end
                     |RpcVariable s ->
                        stack#push gen_el1;
                        stack#push gen_el2;
                        let err_msg = 
                           Printf.sprintf "variable \"%s\" has not been evaluated" s 
                        in
                        raise (Invalid_argument err_msg)
                     |_ ->
                        stack#push gen_el1;
                        stack#push gen_el2;
                        raise (Invalid_argument "lcm requires integer or real arguments")
                     end
                  end else begin
                     stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "real argument is too large to convert to integer")
                  end
               |RpcVariable s ->
                  stack#push gen_el1;
                  stack#push gen_el2;
                  let err_msg = 
                     Printf.sprintf "variable \"%s\" has not been evaluated" s 
                  in
                  raise (Invalid_argument err_msg)
               |_ ->
                  stack#push gen_el1;
                  stack#push gen_el2;
                  raise (Invalid_argument "lcm requires integer or real arguments")
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
               |RpcFloatUnit (el1, uu1) ->
                  begin match gen_el2 with
                  |RpcFloatUnit (el2, uu2) ->
                     if uu1 <> Units.empty_unit || uu2 <> Units.empty_unit then begin
                        stack#push gen_el1;
                        stack#push gen_el2;
                        raise_invalid "cannot compute binom of dimensioned values"
                     end else
                        begin try
                           let log_coeff = (Gsl_sf.lngamma (el1 +. 1.0)) -.
                           (Gsl_sf.lngamma (el2 +. 1.0)) -. 
                           (Gsl_sf.lngamma (el1 -. el2 +. 1.0)) in
                           let (f, u) = funit_of_float (exp log_coeff) in
                           stack#push (RpcFloatUnit (f, u));
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
                  raise (Invalid_argument "binom can only be applied to real or integer arguments"))
               end
            end else
               raise (Invalid_argument "insufficient arguments for binom")
         |_ ->
            (* shouldn't hit this point if interface is well-behaved *)
            self#abort_computation ();
            false


      (* # of permutations of subsets of a population
       * For a float argument, this is computed using lngamma in order to avoid
       * overflow.  For an integer argument, jump to an interruptible
       * exact arithmetic value. *)
      method permutations () =
         match interr_args with
         |Perm_args (n, term, partial, el1, el2) ->
            if eq_big_int n term then begin
               stack#push (RpcInt partial);
               interr_args <- NoArgs;
               true
            end else begin
               let new_partial = mult_big_int n partial in
               interr_args <- Perm_args ((pred_big_int n), term, new_partial,
               el1, el2);
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
                           let nmk = sub_big_int el1 el2 in
                           interr_args <- Perm_args (el1, nmk, unit_big_int,
                           gen_el1, gen_el2);
                           false
                        else
                           (stack#push gen_el1;
                           stack#push gen_el2;
                           raise (Invalid_argument "first argument to perm must be >= second argument"))
                     else
                        (stack#push gen_el1;
                        stack#push gen_el2;
                        raise (Invalid_argument "integer perm requires nonnegative arguments"))
                  |_ ->
                     (stack#push gen_el1;
                     stack#push gen_el2;
                     raise (Invalid_argument "perm requires either two integer or two real arguments"))
                  end
               |RpcFloatUnit (el1, uu1) ->
                  begin match gen_el2 with
                  |RpcFloatUnit (el2, uu2) ->
                     if uu1 <> Units.empty_unit || uu2 <> Units.empty_unit then begin
                        stack#push gen_el1;
                        stack#push gen_el2;
                        raise_invalid "cannot compute permutations of dimensioned values"
                     end else
                        begin try
                           let log_perm = (Gsl_sf.lngamma (el1 +. 1.0)) -.
                           (Gsl_sf.lngamma (el1 -. el2 +. 1.0)) in
                           let (f, u) = funit_of_float (exp log_perm) in
                           stack#push (RpcFloatUnit (f, u));
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
                     raise (Invalid_argument "perm requires either two integer or two real arguments"))
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
                  raise (Invalid_argument "perm can only be applied to real or integer arguments"))
               end
            end else
               raise (Invalid_argument "insufficient arguments for perm")
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
         |RpcFloatMatrixUnit (mat, uu) ->
            (* multiply on the left by a row of ones *)
            let n, m = Gsl_matrix.dims mat in
            let ones_arr = Array.make n 1.0 in
            let ones = Gsl_matrix.of_array ones_arr 1 n in
            let result = Gsl_matrix.create 1 m in
            Gsl_blas.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans 1.0 ones mat
            0.0 result;
            stack#push (RpcFloatMatrixUnit (result, uu))
         |_ ->
            stack#push gen_el;
            raise (Invalid_argument "total can only be applied to real matrices")


      method mean () = self#check_args 1 "mean" self#internal_mean

      (* single-variable statistics: sample mean *)
      method private internal_mean () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcFloatMatrixUnit (mat, uu) ->
            (* multiply on the left by a row of ones, divided by n *)
            let n, m = Gsl_matrix.dims mat in
            let ones_arr = Array.make n (1.0 /. (float_of_int n)) in
            let ones = Gsl_matrix.of_array ones_arr 1 n in
            let result = Gsl_matrix.create 1 m in
            Gsl_blas.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans 1.0 ones mat
            0.0 result;
            stack#push (RpcFloatMatrixUnit (result, uu))
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
         |RpcFloatMatrixUnit (mat, uu) ->
            let n, m = Gsl_matrix.dims mat in
            let result = Gsl_matrix.create 1 m in
            for col = 0 to pred m do
               result.{0, col} <- 0.0;
               for row = 0 to pred n do
                  let squared_el = mat.{row, col} *. mat.{row, col} in
                  result.{0, col} <- result.{0, col} +. squared_el
               done
            done;
            stack#push (RpcFloatMatrixUnit (result, Units.mult uu uu))
         |_ ->
            stack#push gen_el;
            raise (Invalid_argument "sumsq can only be applied to real matrices")


      method variance_unbiased () = self#check_args 1 "var"
      self#internal_variance_unbiased

      (* single-variable statistics: bias-corrected sample variance *)
      method private internal_variance_unbiased () =
         self#evaln 1;
         let gen_el = stack#peek 1 in
         match gen_el with
         |RpcFloatMatrixUnit (mat, uu) ->
            let n, m = Gsl_matrix.dims mat in
            if n >= 2 then begin
               self#internal_variance_biased ();
               let n_over_nm1 = (float_of_int n) /. (float_of_int (pred n)) in
               let (f, u) = funit_of_float n_over_nm1 in
               stack#push (RpcFloatUnit (f, u));
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
         |RpcFloatMatrixUnit (mat, uu) ->
            let n, m = Gsl_matrix.dims mat in
            let float_n = float_of_int n in
            (* computes variance as E[X^2] - E[X]^2 *)
            self#internal_dup ();
            self#internal_sum_squares ();
            let (f, u) = funit_of_float float_n in
            stack#push (RpcFloatUnit (f, u));
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
         |RpcFloatMatrixUnit (mat, uu) ->
            let n, m = Gsl_matrix.dims mat in
            let result = Gsl_matrix.create 1 m in
            for col = 0 to pred m do
               result.{0, col} <- sqrt mat.{0, col}
            done;
            stack#push (RpcFloatMatrixUnit (result, Units.pow uu 0.5))
         |_ -> ()
            

      method standard_deviation_biased () = self#check_args 1 "stdevbias"
      self#internal_standard_deviation_biased

      (* single-variable statistics: unbiased sample standard deviation *)
      method private internal_standard_deviation_biased () =
         self#internal_variance_biased ();
         let gen_el = stack#pop () in
         match gen_el with
         |RpcFloatMatrixUnit (mat, uu) ->
            let n, m = Gsl_matrix.dims mat in
            let result = Gsl_matrix.create 1 m in
            for col = 0 to pred m do
               result.{0, col} <- sqrt mat.{0, col}
            done;
            stack#push (RpcFloatMatrixUnit (result, Units.pow uu 0.5))
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
         |RpcFloatMatrixUnit (mat, uu) ->
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
            stack#push (RpcFloatMatrixUnit (result, uu))
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
               funit_of_float (float_of_big_int i_el)
            |RpcFloatUnit (el, uu) ->
               (el, uu)
            |_ ->
               stack#push gen_el1;
               stack#push gen_el2;
               stack#push gen_el3;
               raise (Invalid_argument "utpn requires real scalar arguments")
         in
         let (mean_orig, mean_units) = get_float_args gen_el1
         and (var_orig, var_units)   = get_float_args gen_el2
         and (cutoff, cutoff_units)  = get_float_args gen_el3 in
         try
            (* check that units are consistent *)
            let mean = mean_orig *. 
               (Units.conversion_factor mean_units cutoff_units
               !Rcfile.unit_table)
            in
            let var = var_orig *.
               (Units.conversion_factor var_units cutoff_units
               !Rcfile.unit_table)
            in
            if var <= 0.0 then begin
               stack#push gen_el1;
               stack#push gen_el2;
               stack#push gen_el3;
               raise (Invalid_argument "variance argument to utpn must be positive")
            end else begin
               let arg = (cutoff -. mean) /. (sqrt (2.0 *. var)) in
               let (f, u) = funit_of_float arg in
               stack#push (RpcFloatUnit (f, u));
               self#internal_erfc ();
               stack#push (RpcFloatUnit (0.5, cutoff_units));
               self#internal_mult ()
            end
         with Units.Units_error s ->
            stack#push gen_el1;
            stack#push gen_el2;
            stack#push gen_el3;
            raise_invalid s



      (* random float between 0 and 1 *)
      method rand () =
         self#backup ();           
         let (f, u) = funit_of_float (Random.float 1.0) in
         stack#push (RpcFloatUnit (f, u))


      (* standardize units *)
      method standardize_units () = self#check_args 1 "ustand"
      self#internal_standardize_units

      method private internal_standardize_units () =
         self#evaln 1;
         let gen_el = stack#pop () in
         let get_std u = Units.standardize_units u !Rcfile.unit_table in
         match gen_el with
         |RpcFloatUnit (el, uu) ->
            let std = get_std uu in
            stack#push (RpcFloatUnit (el *. std.Units.coeff, std.Units.comp_units))
         |RpcComplexUnit (el, uu) ->
            let std = get_std uu in
            let c_coeff = c_of_f std.Units.coeff in
            stack#push (RpcComplexUnit 
               (Complex.mul el c_coeff, std.Units.comp_units))
         |RpcFloatMatrixUnit (el, uu) ->
            let std = get_std uu in
            let result = Gsl_matrix.copy el in
            Gsl_matrix.scale result std.Units.coeff;
            stack#push (RpcFloatMatrixUnit 
            (result, std.Units.comp_units))
         |RpcComplexMatrixUnit (el, uu) ->
            let std = get_std uu in
            let c_coeff = c_of_f std.Units.coeff in
            let result = Gsl_matrix_complex.copy el in
            Gsl_matrix_complex.scale result c_coeff;
            stack#push (RpcComplexMatrixUnit 
            (result, std.Units.comp_units))
         |_ ->
            stack#push gen_el


      (* obtain the magnitude of a dimensioned value *)
      method unit_value () = self#check_args 1 "uvalue"
      self#internal_unit_value

      method private internal_unit_value () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcFloatUnit (el, uu) ->
            stack#push (RpcFloatUnit (el, Units.empty_unit))
         |RpcComplexUnit (el, uu) ->
            stack#push (RpcComplexUnit (el, Units.empty_unit))
         |RpcFloatMatrixUnit (el, uu) ->
            stack#push (RpcFloatMatrixUnit (el, Units.empty_unit))
         |RpcComplexMatrixUnit (el, uu) ->
            stack#push (RpcComplexMatrixUnit (el, Units.empty_unit))
         |_ ->
            stack#push gen_el


      (* obtain the magnitude of a dimensioned value *)
      method convert_units () = self#check_args 2 "uconvert"
      self#internal_convert_units

      method private internal_convert_units () =
         self#evaln 1;
         let gen_el2 = stack#pop () in
         let gen_el1 = stack#pop () in
         match gen_el2 with
         |RpcFloatUnit (el2, uu2) ->
            begin match gen_el1 with
            |RpcFloatUnit (el1, uu1) ->
               begin try
                  let conv = Units.conversion_factor uu1 uu2 !Rcfile.unit_table in
                  stack#push (RpcFloatUnit (el1 *. conv, uu2))
               with Units.Units_error s ->
                  stack#push gen_el1;
                  stack#push gen_el2;
                  raise_invalid s
               end
            |RpcComplexUnit (el1, uu1) ->
               begin try
                  let c_conv = {
                     Complex.re = Units.conversion_factor uu1 uu2 !Rcfile.unit_table;
                     Complex.im = 0.0
                  } in
                  stack#push (RpcComplexUnit (Complex.mul el1 c_conv, uu2))
               with Units.Units_error s ->
                  stack#push gen_el1;
                  stack#push gen_el2;
                  raise_invalid s
               end
            |RpcFloatMatrixUnit (el1, uu1) ->
               begin try
                  let conv = Units.conversion_factor uu1 uu2 !Rcfile.unit_table in
                  let result = Gsl_matrix.copy el1 in
                  Gsl_matrix.scale result conv;
                  stack#push (RpcFloatMatrixUnit (result, uu2))
               with Units.Units_error s ->
                  stack#push gen_el1;
                  stack#push gen_el2;
                  raise_invalid s
               end
            |RpcComplexMatrixUnit (el1, uu1) ->
               begin try
                  let conv = {
                     Complex.re = Units.conversion_factor uu1 uu2 !Rcfile.unit_table;
                     Complex.im = 0.0
                  } in
                  let result = Gsl_matrix_complex.copy el1 in
                  Gsl_matrix_complex.scale result conv;
                  stack#push (RpcComplexMatrixUnit (result, uu2))
               with Units.Units_error s ->
                  stack#push gen_el1;
                  stack#push gen_el2;
                  raise_invalid s
               end
            |_ ->
               stack#push gen_el1;
               stack#push gen_el2;
               raise_invalid "cannot convert units for this data type"
            end
         |_ ->
            stack#push gen_el1;
            stack#push gen_el2;
            raise_invalid "unit conversion target must be real-valued"


      (* trace of a matrix *)
      method trace () = self#check_args 1 "trace"
      self#internal_trace

      method private internal_trace () =
         self#evaln 1;
         let gen_el = stack#pop () in
         match gen_el with
         |RpcFloatMatrixUnit (el, uu) ->
            let n, m = Gsl_matrix.dims el in
            if n = m then begin
               let result = ref 0.0 in
               for i = 0 to pred n do
                  result := !result +. el.{i, i}
               done;
               stack#push (RpcFloatUnit (!result, uu))
            end else begin
               stack#push gen_el;
               raise_invalid "argument of trace must be a square matrix"
            end
         |RpcComplexMatrixUnit (el, uu) ->
            let n, m = Gsl_matrix_complex.dims el in
            if n = m then begin
               let result = ref Complex.zero in
               for i = 0 to pred n do
                  result := Complex.add !result el.{i, i}
               done;
               stack#push (RpcComplexUnit (!result, uu))
            end else begin
               stack#push gen_el;
               raise_invalid "argument of trace must be a square matrix"
            end
         |_ ->
            stack#push gen_el;
            raise_invalid "argument of trace must be a square matrix"




(*      method print_stack () =
         let print_el line_num el = Printf.printf "%2d:  %s\n" line_num el in
         for i = stack#length downto 1 do
            print_el i (stack#get_display_line i modes)
         done
*)

   end;;








(* arch-tag: DO_NOT_CHANGE_548916d4-da42-49b4-8941-c0d42306f1b7 *)
