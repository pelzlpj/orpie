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


let pi = 3.14159265358979323846;;

type f_or_c = | F of float 
              | C of Complex.t

(* decode a matrix of type CF and decide
 * whether it has float elements or complex elements,
 * then create the appropriate orpie_data_t type. *)
let decode_float_complex_matrix mat =
   let num_rows = Array.length mat
   and num_cols = Array.length mat.(0) in
   let flt_array = Array.make_matrix num_rows num_cols 0.0
   and cpx_array = Array.make_matrix num_rows num_cols Complex.zero in
   let has_complex = ref false in
   for i = 0 to pred num_rows do
      if Array.length mat.(i) != num_cols then
         raise (Utility.Txtin_error "inconsistent number of columns in input matrix")
      else
         begin
            for j = 0 to pred num_cols do
               begin match mat.(i).(j) with
               |F el ->
                  flt_array.(i).(j) <- el;
                  cpx_array.(i).(j) <- {Complex.re = el; Complex.im = 0.0}
               |C el -> 
                  has_complex := true;
                  cpx_array.(i).(j) <- el
               end
            done;
         end
   done;
   if !has_complex then
      Rpc_stack.RpcComplexMatrix (Gsl_matrix_complex.of_arrays cpx_array)
   else
      Rpc_stack.RpcFloatMatrix (Gsl_matrix.of_arrays flt_array)


let rect_of_polar_rad r theta = 
   let real = r *. (cos theta)
   and imag = r *. (sin theta) in
   {Complex.re = real; Complex.im = imag}

let rect_of_polar_deg r theta = 
   let rad_theta = theta /. 180.0 *. pi in
   rect_of_polar_rad r rad_theta


(* convert an integer string to an RpcInt *)
let decode_integer i_str =
   let int_str = i_str in
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
   Rpc_stack.RpcInt int_val


(* convert a floating point string to an RpcFloat *)
let decode_float f_str =
   Rpc_stack.RpcFloat (float_of_string f_str)


(* convert a cartesian complex number string to an RpcComplex *)
let decode_complex_rect re_str im_str =
   let f1 = float_of_string re_str
   and f2 = float_of_string im_str in
   Rpc_stack.RpcComplex {Complex.re = f1; Complex.im = f2}


(* convert a polar representation complex number string to an
 * RpcComplex.  The rect_of_polar argument should take care of
 * any necessary degrees/radians conversion. *)
let decode_complex_polar rect_of_polar mag_str ang_str = 
   let mag = float_of_string mag_str
   and ang = float_of_string ang_str in
   Rpc_stack.RpcComplex (rect_of_polar mag ang)


(* convert a polar representation complex number string to an
 * RpcComplex.  Assumes radian representation of the angle. *)
let decode_complex_polar_rad mag_str ang_str =
   decode_complex_polar rect_of_polar_rad mag_str ang_str

(* convert a polar representation complex number string to an
 * RpcComplex.  Assumes degree representation of the angle. *)
let decode_complex_polar_deg mag_str ang_str =
   decode_complex_polar rect_of_polar_deg mag_str ang_str


(* convert a matrix to an RpcFloatMatrix or an RpcComplexMatrix. *)
let decode_matrix mat_rows =
   (* matrix_rows is a list of rows, each of which
    * is a list of elements; create a 2d array 
    * from these lists, and generate the appropriate
    * orpie_data_t from the 2d array. *)
   let num_rows = List.length mat_rows in
   let num_cols = List.length (List.hd mat_rows) in
   let mat = Array.make_matrix num_rows num_cols (F 0.0) in
   let temp_arr = Array.of_list (List.rev mat_rows) in
   for i = 0 to pred num_rows do
      mat.(i) <- Array.of_list (List.rev temp_arr.(i))
   done;
   (* create a float array or a complex array, depending
    * on whether or not any complex types are present
    * in this array. *)
   decode_float_complex_matrix mat

(* convert a floating point string to an RpcFloat *)
let decode_variable v_str =
   Rpc_stack.RpcVariable v_str
%}


/* declarations */
%token <string> INTEGER
%token <string> FLOAT
%token <string> VARIABLE
%token BEGINCOMPLEX
%token ENDCOMPLEX
%token SEPARATOR
%token ANGLE
%token BEGINMATRIX
%token ENDMATRIX
%token EOF

/* parse the input under the assumption that angles
 * are provided using radian measure */
%start decode_data_rad
%type <Rpc_stack.orpie_data_t list> decode_data_rad

/* parse the input under the assumption that angles
 * are provided using degree measure */
%start decode_data_deg
%type <Rpc_stack.orpie_data_t list> decode_data_deg

%%
/* rules */

/****************************************
 * ANGLES PROVIDED USING RADIAN MEASURE *
 ****************************************/
decode_data_rad:
   datalist_rad EOF 
      { List.rev $1 }
;


datalist_rad:
   datalist_rad datagroup_rad
      { $2 :: $1 }
   |  /* empty */
      { [] }
;


datagroup_rad:
   data_rad
      { $1 }
;


data_rad:
   INTEGER 
      { decode_integer $1 }

   | VARIABLE
      { decode_variable $1}

   | FLOAT
      { decode_float $1 } 

   | BEGINCOMPLEX FLOAT SEPARATOR FLOAT ENDCOMPLEX
      { decode_complex_rect $2 $4 }

   | BEGINCOMPLEX FLOAT ANGLE FLOAT ENDCOMPLEX
      { decode_complex_polar_rad $2 $4 }

   | BEGINMATRIX matrix_rows_rad ENDMATRIX
      { decode_matrix $2 }
;


matrix_rows_rad:
   matrix_rows_rad BEGINMATRIX matrix_row_elements_rad ENDMATRIX
      {$3 :: $1}
   | /* empty */
      {[]}
;


matrix_row_elements_rad:
   matrix_row_elements_rad SEPARATOR FLOAT
      { (F (float_of_string $3)) :: $1 }
   | matrix_row_elements_rad SEPARATOR BEGINCOMPLEX FLOAT SEPARATOR FLOAT ENDCOMPLEX
      { 
         let f1 = float_of_string $4
         and f2 = float_of_string $6 in
         (C {Complex.re = f1; Complex.im = f2}) :: $1
      }
   | matrix_row_elements_rad SEPARATOR BEGINCOMPLEX FLOAT ANGLE FLOAT ENDCOMPLEX
      { 
         let r  = float_of_string $4
         and th = float_of_string $6 in
         (C (rect_of_polar_rad r th)) :: $1
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
         (C (rect_of_polar_rad r th)) :: []
      }
;



/****************************************
 * ANGLES PROVIDED USING DEGREE MEASURE *
 ****************************************/
decode_data_deg:
   datalist_deg EOF 
      { List.rev $1 }
;


datalist_deg:
   datalist_deg datagroup_deg
      { $2 :: $1 }
   |  /* empty */
      { [] }
;


datagroup_deg:
   data_deg
      { $1 }
;


data_deg:
   INTEGER 
      { decode_integer $1 }

   | VARIABLE
      { decode_variable $1}

   | FLOAT
      { decode_float $1 } 

   | BEGINCOMPLEX FLOAT SEPARATOR FLOAT ENDCOMPLEX
      { decode_complex_rect $2 $4 }

   | BEGINCOMPLEX FLOAT ANGLE FLOAT ENDCOMPLEX
      { decode_complex_polar_deg $2 $4 }

   | BEGINMATRIX matrix_rows_deg ENDMATRIX
      { decode_matrix $2 }
;


matrix_rows_deg:
   matrix_rows_deg BEGINMATRIX matrix_row_elements_deg ENDMATRIX
      {$3 :: $1}
   | /* empty */
      {[]}
;


matrix_row_elements_deg:
   matrix_row_elements_deg SEPARATOR FLOAT
      { (F (float_of_string $3)) :: $1 }
   | matrix_row_elements_deg SEPARATOR BEGINCOMPLEX FLOAT SEPARATOR FLOAT ENDCOMPLEX
      { 
         let f1 = float_of_string $4
         and f2 = float_of_string $6 in
         (C {Complex.re = f1; Complex.im = f2}) :: $1
      }
   | matrix_row_elements_deg SEPARATOR BEGINCOMPLEX FLOAT ANGLE FLOAT ENDCOMPLEX
      { 
         let r  = float_of_string $4
         and th = float_of_string $6 in
         (C (rect_of_polar_deg r th)) :: $1
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
         (C (rect_of_polar_deg r th)) :: []
      }
;


/* arch-tag: DO_NOT_CHANGE_c65a2550-f00d-40f1-b51f-f5d654257785 */
