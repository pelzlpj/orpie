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
(* test whether the calculator result is a floating-point value
 * which is within the specified tolerance (normalized) *)
let test_result_float_tol (expected : float) (tol : float)
(test_stage : string) =
   print_indent test_stage;
   let result = get_result () in
   let ff = float_of_string (num_part result) in
   if abs_float (ff -. expected) /. expected > tol then begin
      print_endline ("calculator result: \"" ^ (num_part result) ^ "\"");
      print_endline ("         expected: \"" ^ (string_of_float expected) ^ "\"");
      failwith test_stage
   end else ()
in


print_endline "testing add()...";

load_data "#10`d #20`d";
calc#add ();
test_result_exact "# 30`d" "add-int-int-1";

load_data "#10`o #20`h";
calc#add ();
test_result_exact "# 40`d" "add-int-int-2";

load_data "10.0 #10`d";
calc#add ();
test_result_float_tol 20.0 1e-15 "add-float-int-1";

load_data "#10`d 10.0";
calc#add ();
test_result_float_tol 20.0 1e-15 "add-int-float-1";





print_endline "rpc_calc tested OK!";;



(* arch-tag: DO_NOT_CHANGE_f6a7a71b-838a-4128-8858-14708a0c2f69 *)
