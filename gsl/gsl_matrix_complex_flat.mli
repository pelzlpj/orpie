(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002-2005 - Olivier Andrieu                *)
(* distributed under the terms of the GPL version 2         *)

(** Matrices of complex number simplemented with [float array] *)

type complex_mat_flat = private {
  data : float array;
  off : int;
  dim1 : int;
  dim2 : int;
  tda : int;
} 
type matrix = complex_mat_flat

open Gsl_complex

val create : ?init:complex -> int -> int -> matrix
val dims : matrix -> int * int

val of_arrays : complex array array -> matrix
val of_array  : complex array -> int -> int -> matrix
val to_arrays : matrix -> complex array array
val to_array  : matrix -> complex array

val of_complex_array : float array -> int -> int -> matrix
val to_complex_array : matrix -> complex_array

val get : matrix -> int -> int -> complex
val set : matrix -> int -> int -> complex -> unit

val set_all : matrix -> complex -> unit
val set_zero : matrix -> unit
val set_id : matrix -> unit

val memcpy : src:matrix -> dst:matrix -> unit
val copy : matrix -> matrix

external add : matrix -> matrix -> unit = "ml_gsl_matrix_complex_add"
external sub : matrix -> matrix -> unit = "ml_gsl_matrix_complex_sub"
external mul_elements : matrix -> matrix -> unit = "ml_gsl_matrix_complex_mul"
external div_elements : matrix -> matrix -> unit = "ml_gsl_matrix_complex_div"
external scale : matrix -> float -> unit = "ml_gsl_matrix_complex_scale"
external add_constant : matrix -> float -> unit
  = "ml_gsl_matrix_complex_add_constant"
external add_diagonal : matrix -> complex -> unit = "ml_gsl_matrix_complex_add_diagonal"
external is_null : matrix -> bool = "ml_gsl_matrix_complex_isnull"

external swap_rows : matrix -> int -> int -> unit
  = "ml_gsl_matrix_complex_swap_rows"
external swap_columns : matrix -> int -> int -> unit
  = "ml_gsl_matrix_complex_swap_columns"
external swap_rowcol : matrix -> int -> int -> unit
  = "ml_gsl_matrix_complex_swap_rowcol"
external transpose : matrix -> matrix -> unit
  = "ml_gsl_matrix_complex_transpose_memcpy"
external transpose_in_place : matrix -> unit
  = "ml_gsl_matrix_complex_transpose"

open Gsl_vector_complex_flat

val submatrix :
  matrix ->
  k1:int -> k2:int -> n1:int -> n2:int -> matrix

val row    : matrix -> int -> vector
val column : matrix -> int -> vector
val diagonal      : matrix -> vector
val subdiagonal   : matrix -> int -> vector
val superdiagonal : matrix -> int -> vector

val view_complex_array :
  complex_array -> ?off:int -> int -> ?tda:int -> int -> matrix
val view_vector : vector -> ?off:int -> 
  int -> ?tda:int -> int -> matrix
