%{
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

(* txtinr_parser.mly
 *
 * Orpie can handle input of data from a textfile created by an external editor.
 * editor_parser.mly generates a parser that works in conjunction with
 * editor_lexer.mll to enter this data on the stack.
 *)

type f_or_c = | F of float 
              | C of Complex.t

(* decode a matrix of type CF and decide
 * whether it has float elements or complex elements,
 * then create the appropriate orpie_data type. *)
let decode_float_complex_matrix mat =
   let num_rows = Array.length mat
   and num_cols = Array.length mat.(0) in
   let flt_array = Array.make_matrix num_rows num_cols 0.0
   and cpx_array = Array.make_matrix num_rows num_cols Complex.zero in
   let has_complex = ref false in
   Printf.fprintf stderr "matrix:\n";
   for i = 0 to pred num_rows do
      if Array.length mat.(i) != num_cols then
         raise (Utility.Txtin_error "inconsistent number of columns in input matrix")
      else
         begin
            for j = 0 to pred num_cols do
               begin match mat.(i).(j) with
               |F el ->
                  Printf.fprintf stderr "F: %8.5g " el;
                  flush stderr;
                  flt_array.(i).(j) <- el;
                  cpx_array.(i).(j) <- {Complex.re = el; Complex.im = 0.0}
               |C el -> 
                  Printf.fprintf stderr "C: (%8.5g, %8.5g) " el.Complex.re
                  el.Complex.im;
                  flush stderr;
                  has_complex := true;
                  cpx_array.(i).(j) <- el
               end
            done;
            Printf.fprintf stderr "\n"
         end
   done;
   if !has_complex then
      Rpc_stack.RpcComplexMatrix (Gsl_matrix_complex.of_arrays cpx_array)
   else
      Rpc_stack.RpcFloatMatrix (Gsl_matrix.of_arrays flt_array)


let rect_of_polar r theta = 
   let real = r *. (cos theta)
   and imag = r *. (sin theta) in
   {Complex.re = real; Complex.im = imag}


%}

/* declarations */
%token <string> INTEGER
%token <string> FLOAT
%token BEGINCOMPLEX
%token ENDCOMPLEX
%token SEPARATOR
%token ANGLE
%token BEGINMATRIX
%token ENDMATRIX
%token EOF

%start data_list
%type <Rpc_stack.orpie_data list> data_list

%%
/* rules */

data_list:
   tokenlist EOF 
      { List.rev $1 }
;


tokenlist:
   tokenlist tokengroup 
      { $2 :: $1 }
   |  /* empty */
      { [] }
;


tokengroup:
   token
      { $1 }
;


token:
   INTEGER 
      { Printf.fprintf stderr "integer: %s\n" $1;
        flush stderr;
        let int_str = $1 in
        let str_len = String.length int_str in
        let digits  = Str.string_before int_str (str_len - 2) in
        let int_val =
           let base_char = int_str.[pred str_len] in
           if base_char = 'b' then 
              Big_int_str.big_int_of_string_base digits 2
           else if base_char = 'o' then
              Big_int_str.big_int_of_string_base digits 8
           else if base_char = 'd' then
              Big_int_str.big_int_of_string_base digits 10
           else if base_char = 'h' then
              Big_int_str.big_int_of_string_base digits 16
           else 
              let base_string = String.make 1 base_char in
              raise (Utility.Txtin_error ("illegal base character " ^ base_string))
        in
        Rpc_stack.RpcInt int_val}

   | FLOAT
      { Printf.fprintf stderr "FLOAT float string: '%s'\n" $1;
        flush stderr;
        Printf.fprintf stderr "float: %g\n" (float_of_string $1);
        flush stderr;
        Rpc_stack.RpcFloat (float_of_string $1)} 

   | BEGINCOMPLEX FLOAT SEPARATOR FLOAT ENDCOMPLEX
      {
         Printf.fprintf stderr "COMPLEX1 float string: '%s'\n" $2;
         flush stderr;
         Printf.fprintf stderr "COMPLEX2 float string: '%s'\n" $4;
         flush stderr;
         let f1 = float_of_string $2
         and f2 = float_of_string $4 in
         Printf.fprintf stderr "complex: (%g, %g)\n" f1 f2;
         flush stderr;
         Rpc_stack.RpcComplex {Complex.re = f1; Complex.im = f2}
      }

   | BEGINCOMPLEX FLOAT ANGLE FLOAT ENDCOMPLEX
      {
         Printf.fprintf stderr "COMPLEX1 float string: '%s'\n" $2;
         flush stderr;
         Printf.fprintf stderr "COMPLEX2 float string: '%s'\n" $4;
         flush stderr;
         let mag = float_of_string $2
         and ang = float_of_string $4 in
         Printf.fprintf stderr "complex: (%g <%g)\n" mag ang;
         flush stderr;
         Rpc_stack.RpcComplex (rect_of_polar mag ang)
      }

   | BEGINMATRIX matrix_rows ENDMATRIX
      { 
         (* matrix_rows is a list of rows, each of which
          * is a list of elements; create a 2d array 
          * from these lists, and generate the appropriate
          * orpie_data from the 2d array. *)
         Printf.fprintf stderr "matched matrix toplevel\n";
         flush stderr;
         let num_rows = List.length $2 in
         let num_cols = List.length (List.hd $2) in
         let mat = Array.make_matrix num_rows num_cols (F 0.0) in
         let temp_arr = Array.of_list (List.rev $2) in
         for i = 0 to pred num_rows do
            mat.(i) <- Array.of_list (List.rev temp_arr.(i))
         done;
         (* create a float array or a complex array, depending
          * on whether or not any complex types are present
          * in this array. *)
         decode_float_complex_matrix mat
      }
;


matrix_rows:
   matrix_rows BEGINMATRIX matrix_row_elements ENDMATRIX
      {$3 :: $1}
   | /* empty */
      {[]}
;


matrix_row_elements:
   matrix_row_elements SEPARATOR FLOAT
      { (F (float_of_string $3)) :: $1 }
   | matrix_row_elements SEPARATOR BEGINCOMPLEX FLOAT SEPARATOR FLOAT ENDCOMPLEX
      { 
         let f1 = float_of_string $4
         and f2 = float_of_string $6 in
         (C {Complex.re = f1; Complex.im = f2}) :: $1
      }
   | matrix_row_elements SEPARATOR BEGINCOMPLEX FLOAT ANGLE FLOAT ENDCOMPLEX
      { 
         let r  = float_of_string $4
         and th = float_of_string $6 in
         (C (rect_of_polar r th)) :: $1
      }
   | FLOAT
      { (F (float_of_string $1)) :: [] }
   | BEGINCOMPLEX FLOAT SEPARATOR FLOAT ENDCOMPLEX
      {
         let f1 = float_of_string $2
         and f2 = float_of_string $4 in
         (C {Complex.re = f1; Complex.im = f2}) :: []
      }
   | BEGINCOMPLEX FLOAT ANGLE FLOAT ENDCOMPLEX
      {
         let r  = float_of_string $2
         and th = float_of_string $4 in
         (C (rect_of_polar r th)) :: []
      }
;

/* arch-tag: DO_NOT_CHANGE_c65a2550-f00d-40f1-b51f-f5d654257785 */
