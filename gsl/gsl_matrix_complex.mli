(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002 - Olivier Andrieu                     *)
(* distributed under the terms of the GPL version 2         *)

open Bigarray
open Gsl_complex

type complex_mat_bigarr = 
    (Complex.t, complex64_elt, c_layout) Array2.t

type matrix = complex_mat_bigarr

val create : ?init:complex -> int -> int -> matrix

val dims : matrix -> int * int

val of_array  : complex array -> int -> int -> matrix
val of_arrays : complex array array -> matrix
val to_array  : matrix -> complex array
val to_arrays : matrix -> complex array array

val of_complex_array : complex_array -> int -> int -> matrix
val to_complex_array : matrix -> complex_array

val get : matrix -> int -> int -> complex
val set : matrix -> int -> int -> complex -> unit
val set_all  : matrix -> complex -> unit
val set_zero : matrix -> unit
val set_id   : matrix -> unit

val memcpy : src:matrix -> dst:matrix -> unit
val copy   : matrix -> matrix

val row : matrix -> int -> Gsl_vector_complex.vector

external add : matrix -> matrix -> unit = "ml_gsl_matrix_complex_add"
external sub : matrix -> matrix -> unit = "ml_gsl_matrix_complex_sub"
external mul_elements : matrix -> matrix -> unit = "ml_gsl_matrix_complex_mul"
external div_elements : matrix -> matrix -> unit = "ml_gsl_matrix_complex_div"
external scale : matrix -> complex -> unit = "ml_gsl_matrix_complex_scale"
external add_constant : matrix -> complex -> unit = "ml_gsl_matrix_complex_add_constant"
external add_diagonal : matrix -> complex -> unit = "ml_gsl_matrix_complex_add_diagonal"
external is_null : matrix -> bool = "ml_gsl_matrix_complex_isnull"

external swap_rows : matrix -> int -> int -> unit = "ml_gsl_matrix_complex_swap_rows"
external swap_columns : matrix -> int -> int -> unit = "ml_gsl_matrix_complex_swap_columns"
external swap_rowcol : matrix -> int -> int -> unit = "ml_gsl_matrix_complex_swap_rowcol"
external transpose : matrix -> matrix -> unit = "ml_gsl_matrix_complex_transpose_memcpy"
external transpose_in_place : matrix -> unit = "ml_gsl_matrix_complex_transpose"


module Single : sig

  type complex_float_mat_bigarr = 
      (Complex.t, complex32_elt, c_layout) Array2.t

  type matrix = complex_float_mat_bigarr

  val create : ?init:complex -> int -> int -> matrix

  val dims : matrix -> int * int

  val of_array  : complex array -> int -> int -> matrix
  val of_arrays : complex array array -> matrix
  val to_array  : matrix -> complex array
  val to_arrays : matrix -> complex array array

  val of_complex_array : complex_array -> int -> int -> matrix
  val to_complex_array : matrix -> complex_array

  val get : matrix -> int -> int -> complex
  val set : matrix -> int -> int -> complex -> unit
  val set_all  : matrix -> complex -> unit
  val set_zero : matrix -> unit
  val set_id   : matrix -> unit

  val memcpy : src:matrix -> dst:matrix -> unit
  val copy   : matrix -> matrix

  val row : matrix -> int -> Gsl_vector_complex.Single.vector

  external add : matrix -> matrix -> unit = "ml_gsl_matrix_complex_float_add"
  external sub : matrix -> matrix -> unit = "ml_gsl_matrix_complex_float_sub"
  external mul_elements : matrix -> matrix -> unit = "ml_gsl_matrix_complex_float_mul"
  external div_elements : matrix -> matrix -> unit = "ml_gsl_matrix_complex_float_div"
  external scale : matrix -> complex -> unit = "ml_gsl_matrix_complex_float_scale"
  external add_constant : matrix -> complex -> unit = "ml_gsl_matrix_complex_float_add_constant"
  external add_diagonal : matrix -> complex -> unit = "ml_gsl_matrix_complex_float_add_diagonal"
  external is_null : matrix -> bool = "ml_gsl_matrix_complex_float_isnull"
      
  external swap_rows : matrix -> int -> int -> unit = "ml_gsl_matrix_complex_float_swap_rows"
  external swap_columns : matrix -> int -> int -> unit = "ml_gsl_matrix_complex_float_swap_columns"
  external swap_rowcol : matrix -> int -> int -> unit = "ml_gsl_matrix_complex_float_swap_rowcol"
  external transpose : matrix -> matrix -> unit = "ml_gsl_matrix_complex_float_transpose_memcpy"
  external transpose_in_place : matrix -> unit = "ml_gsl_matrix_complex_float_transpose"
end
