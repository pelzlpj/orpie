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

(* calc_test.ml
 * a testing framework for the rpc_calc object *)

open Rpc_calc;;
open Rpc_stack;;

let calc = new Rpc_calc.rpc_calc in

(* load data into the calculator using its string representation *)
let load_data (data_str : string) =
   let lexbuf = Lexing.from_string data_str in
   let data = 
      (* need to call completely different parsers when using degrees
       * or when using radians *)
      begin match (calc#get_modes ()).angle with
      |Rad ->
         Txtin_parser.decode_data_rad Txtin_lexer.token lexbuf
      |Deg ->
         Txtin_parser.decode_data_deg Txtin_lexer.token lexbuf
      end
   in
   List.iter calc#push data
in
let get_result () = calc#get_display_line 1 in
let underscore = Str.regexp "_" in
(* grab only the numeric portion of a value with units *)
let num_part (num_and_units : string) =
   List.hd (Str.split underscore num_and_units)
in
(* grab only the units portion of a value with units *)
let units_part (num_and_units : string) =
   List.hd (List.tl (Str.split underscore num_and_units))
in
let print_indent s = print_endline ("     " ^ s) in
(* test whether the calculator result exactly matches the given
 * string, or raise an exception *)
let test_result_exact (expected : string) (test_stage : string) =
   print_indent test_stage;
   let result = get_result () in
   if result <> expected then begin
      print_endline ("calculator result: \"" ^ result ^ "\"");
      print_endline ("         expected: \"" ^ expected ^ "\"");
      failwith test_stage
   end else ()
in
let test_result_float (expected : float) (tol : float)
(test_stage : string) (normalized : bool) =
   print_indent test_stage;
   let result = get_result () in
   let ff = float_of_string (num_part result) in
   let test = 
      if normalized then (abs_float ((ff -. expected) /. expected)) > tol
      else               (abs_float  (ff -. expected)) > tol
   in
   if test then begin
      print_endline ("calculator result: " ^ (num_part result));
      print_endline ("         expected: " ^ (string_of_float expected));
      failwith test_stage
   end else ()
in
(* test whether the calculator result is a floating-point value
 * which is within the specified tolerance (normalized) *)
let test_result_float_tolnorm (expected : float) (tol : float)
(test_stage : string) =
   test_result_float expected tol test_stage true
in
(* test whether the calculator result is a floating-point value
 * which is within the specified tolerance (normalized) *)
let test_result_float_tol (expected : float) (tol : float)
(test_stage : string) =
   test_result_float expected tol test_stage false
in
(* machine precision tolerance *)
let mprec = 1e-15 in
(* unit conversion tolerance *)
let uprec = 1e-6 in
(* ad-hoc matrix norm *)
let mat_norm () =
   calc#dup ();
   calc#transpose ();
   calc#conj ();
   calc#mult ();
   calc#trace ();
   calc#abs ();
   calc#sqrt ()
in
(* get a normlized error metric for a matrix result. *)
(* Assumes the last two stack elements are result matrix
 * and expected result matrix. *)
let mat_error () =
   calc#dup ();
   calc#rolldown 3;
   calc#sub ();
   mat_norm ();
   calc#swap ();
   mat_norm ();
   calc#div ()
in

(************************************************)
(* ADDITION                                     *)
(************************************************)
print_endline "testing add()...";

load_data "#10`d #20`d";
calc#add ();
test_result_exact "# 30`d" "add-int-int-1";

load_data "#10`o #20`h";
calc#add ();
test_result_exact "# 40`d" "add-int-int-2";

load_data "#10`d 10.0";
calc#add ();
test_result_float_tolnorm 20.0 mprec "add-int-float-1";

load_data "(20.0, 20.0) #10`d (10.0, 20.0)";
calc#add ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 mprec "add-int-complex-1";

load_data "10.0 #10`d";
calc#add ();
test_result_float_tolnorm 20.0 mprec "add-float-int-1";

load_data "10.0 20.0";
calc#add ();
test_result_float_tolnorm 30.0 mprec "add-float-float-1";

load_data "10.0_kg*m/s 20.0_ft*lb/min";
calc#add ();
test_result_float_tolnorm 4359.80831073 uprec "add-float-float-2";

load_data "(20.0, 20.0) 10.0 (10.0, 20.0)";
calc#add ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 mprec "add-float-complex-1";

load_data "(76.66666666667, 20.0)_yd^2/min 10.0_ft^2/s (10.0, 20.0)_yd^2/min";
calc#add ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 uprec "add-float-complex-2";

load_data "(20.0, 20.0) (10.0, 20.0) #10`d";
calc#add ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 mprec "add-complex-int-1";

load_data "(20.0, 20.0) (10.0, 20.0) 10.0";
calc#add ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 mprec "add-complex-float-1";

load_data "(40.0, 60.0) (10.0, 20.0) (30.0, 40.0)";
calc#add ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 mprec "add-complex-complex-1";

load_data "(10.254, 20.508)_m (10.0, 20.0)_in (10.0, 20.0)_m";
calc#add ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 uprec "add-complex-complex-2";

load_data "[[6, 8][10, 12]] [[1, 2][3, 4]] [[5, 6][7, 8]]";
calc#add ();
mat_error ();
test_result_float_tol 0.0 mprec "add-fmat-fmat-1";

load_data "[[55.1676416, 106.3352832][157.5029248, 208.6705664]]_m^2/min
[[1, 2][3, 4]]_yd^2/s [[5, 6][7, 8]]_m^2/min";
calc#add ();
mat_error ();
test_result_float_tol 0.0 uprec "add-fmat-fmat-2";

load_data "[[(6, 6), (9, 8)][(12, 10), (15, 12)]]
           [[1, 2][3, 4]] [[(5, 6), (7, 8)][(9, 10), (11, 12)]]";
calc#add ();
mat_error ();
test_result_float_tol 0.0 mprec "add-fmat-cmat-1";

load_data "[[(55.1676416, 10.0), (106.3352832, 20.0)]
            [(157.5029248, 30.0), (208.6705664, 40.0)]]_m^2/min
           [[1, 2][3, 4]]_yd^2/s
           [[(5, 10.0), (6, 20.0)][(7, 30.0), (8, 40.0)]]_m^2/min";
calc#add ();
mat_error ();
test_result_float_tol 0.0 uprec "add_fmat_cmat_2";

load_data "[[(6, 6), (9, 8)][(12, 10), (15, 12)]]
           [[(5, 6), (7, 8)][(9, 10), (11, 12)]] [[1, 2][3, 4]]";
calc#add ();
mat_error ();
test_result_float_tol 0.0 mprec "add-cmat-fmat-1";

load_data "[[(55.1676416, 10.0), (106.3352832, 20.0)]
            [(157.5029248, 30.0), (208.6705664, 40.0)]]_m^2/min
           [[(5, 10.0), (6, 20.0)][(7, 30.0), (8, 40.0)]]_m^2/min
           [[1, 2][3, 4]]_yd^2/s";
calc#add ();
load_data "1_m^2/min";
calc#convert_units ();
mat_error ();
test_result_float_tol 0.0 uprec "add-cmat-fmat-2";

load_data "[[(-4, 12), (17, 24)][(105, 9), (-5, -7)]]
           [[(1, 2), (3, 4)][(5, 6), (7, 8)]]
           [[(-5, 10), (14, 20)][(100, 3), (-12, -15)]]";
calc#add ();
mat_error ();
test_result_float_tol 0.0 mprec "add-cmat-cmat-1";

load_data "[[(12.0231131092, 20.0462262185), (9.22773573109, 144.092452437)]
            [(65.4323583529, 76.1386786555), (8.63698097479, 58.184904874)]]_lb
           [[(5, 10), (6, 20)][(7, 30), (8, 40)]]_kg
           [[(1, -2), (-4, 100)][(50, 10), (-9, -30)]]_lb";
calc#add ();
mat_error ();
test_result_float_tol 0.0 uprec "add-cmat-cmat-2";

calc#clear ();
(************************************************)
(* SUBTRACTION                                  *)
(************************************************)
print_endline "testing sub()...";

load_data "#10`d #20`d";
calc#sub ();
test_result_exact "# -10`d" "sub-int-int-1";

load_data "#60`o #20`h";
calc#sub ();
test_result_exact "# 16`d" "sub-int-int-2";

load_data "#50`d 10.0";
calc#sub ();
test_result_float_tolnorm 40.0 mprec "sub-int-float-1";

load_data "(20.0, -20.0) #30`d (10.0, 20.0)";
calc#sub ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 mprec "sub-int-complex-1";

load_data "30.0 #10`d";
calc#sub ();
test_result_float_tolnorm 20.0 mprec "sub-float-int-1";

load_data "50.0 20.0";
calc#sub ();
test_result_float_tolnorm 30.0 mprec "sub-float-float-1";

load_data "10.0_kg*m/s 4359.80831073_ft*lb/min";
calc#sub ();
test_result_float_tolnorm (-20.0000000041) uprec "sub-float-float-2";

load_data "(20.0, -20.0) 30.0 (10.0, 20.0)";
calc#sub ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 mprec "sub-float-complex-1";

load_data "(36.6666666667, -20)_yd^2/min 10.0_ft^2/s (30, 20)_yd^2/min";
calc#sub ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 uprec "sub-float-complex-2";

load_data "(20.0, 20.0) (30.0, 20.0) #10`d";
calc#sub ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 mprec "sub-complex-int-1";

load_data "(20.0, 20.0) (30.0, 20.0) 10.0";
calc#sub ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 mprec "sub-complex-float-1";

load_data "(-20.0, -30.0) (10.0, 20.0) (30.0, 50.0)";
calc#sub ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 mprec "sub-complex-complex-1";

load_data "(-10.0, -20.0)_m (10.0, 20.0)_in (10.254, 20.508)_m";
calc#sub ();
calc#sub ();
calc#abs ();
test_result_float_tol 0.0 uprec "sub-complex-complex-2";

load_data "[[-4, -1][2, 6]] [[1, 5][9, 14]] [[5, 6][7, 8]]";
calc#sub ();
mat_error ();
test_result_float_tol 0.0 mprec "sub-fmat-fmat-1";

load_data "[[-5, -6][-7, -8]]_m^2/min [[1, 2][3, 4]]_yd^2/s
           [[55.1676416, 106.3352832][157.5029248, 208.6705664]]_m^2/min";
calc#sub ();
mat_error ();
test_result_float_tol 0.0 uprec "sub-fmat-fmat-2";

load_data "[[(-5, -6), (-7, -8)][(-9, -10), (-11, -12)]]
           [[1, 2][3, 4]] 
           [[(6, 6), (9, 8)][(12, 10), (15, 12)]]";
calc#sub ();
mat_error ();
test_result_float_tol 0.0 mprec "sub-fmat-cmat-1";

load_data "[[(-5, -10.0), (-6, -20.0)][(-7, -30.0), (-8, -40.0)]]_m^2/min
           [[1, 2][3, 4]]_yd^2/s
           [[(55.1676416, 10.0), (106.3352832, 20.0)]
            [(157.5029248, 30.0), (208.6705664, 40.0)]]_m^2/min";
calc#sub ();
mat_error ();
test_result_float_tol 0.0 uprec "sub_fmat_cmat_2";

load_data "[[(4, 6), (5, 8)][(6, 10), (7, 12)]]
           [[(5, 6), (7, 8)][(9, 10), (11, 12)]] [[1, 2][3, 4]]";
calc#sub ();
mat_error ();
test_result_float_tol 0.0 mprec "sub-cmat-fmat-1";

load_data "[[(5, 10), (6, 20)][(7, 30), (8, 40)]]_m^2/min
           [[(55.1676416, 10), (106.3352832, 20)]
            [(157.5029248, 30), (208.6705664, 40)]]_m^2/min
           [[1, 2][3, 4]]_yd^2/s";
calc#sub ();
load_data "1_m^2/min";
calc#convert_units ();
mat_error ();
test_result_float_tol 0.0 uprec "sub-cmat-fmat-2";

load_data "[[(6, -8), (-11, -16)][(-95, 3), (19, 23)]]
           [[(1, 2), (3, 4)][(5, 6), (7, 8)]]
           [[(-5, 10), (14, 20)][(100, 3), (-12, -15)]]";
calc#sub ();
mat_error ();
test_result_float_tol 0.0 mprec "sub-cmat-cmat-1";

load_data "[[(-1, 2), (4, -100)][(-50, -10), (9, 30)]]_lb
           [[(5, 10), (6, 20)][(7, 30), (8, 40)]]_kg
           [[(12.0231131092, 20.0462262185), (9.22773573109, 144.092452437)]
            [(65.4323583529, 76.1386786555), (8.63698097479, 58.184904874)]]_lb";
calc#sub ();
mat_error ();
test_result_float_tol 0.0 uprec "sub-cmat-cmat-2";





print_endline "rpc_calc tested OK!";;



(* arch-tag: DO_NOT_CHANGE_f6a7a71b-838a-4128-8858-14708a0c2f69 *)
