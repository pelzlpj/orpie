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


(* simplifying functions for use with ocamlgsl bindings *)

let cmpx_of_int i   = {Complex.re=Big_int.float_of_big_int i; Complex.im=0.0}
let cmpx_of_float f = {Complex.re=f; Complex.im=0.0}
let cmat_of_fmat fm =
   let rows, cols = Gsl_matrix.dims fm and
   f_array = Gsl_matrix.to_array fm in
   let c_array = Array.map cmpx_of_float f_array in
   Gsl_matrix_complex.of_array c_array rows cols



(* 1-norm of matrix *)
let one_norm mat =
   let n, m = Gsl_matrix.dims mat in
   let maxval = ref (-1.0) in
   let sum = ref 0.0 in
   for j = 0 to pred m do
      sum := 0.0;
      for i = 0 to pred n do
         sum := !sum +. (abs_float mat.{i, j})
      done;
      if !sum > !maxval then
         maxval := !sum
      else
         ()
   done;
   !maxval






(* arch-tag: DO_NOT_CHANGE_a19e0df2-6d6b-4925-87eb-be2a2926ffbb *)
