(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002-2005 - Olivier Andrieu                *)
(* distributed under the terms of the GPL version 2         *)

(** Matrices of floats implemented with [Bigarray] *)

type double_mat_bigarr = 
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

type matrix = double_mat_bigarr

val create : ?init:float -> int -> int -> matrix

val dims : matrix -> int * int

val of_array  : float array -> int -> int -> matrix
val of_arrays : float array array -> matrix
val to_array  : matrix -> float array
val to_arrays : matrix -> float array array

val get : matrix -> int -> int -> float
val set : matrix -> int -> int -> float -> unit
val set_all  : matrix -> float -> unit
val set_zero : matrix -> unit
val set_id   : matrix -> unit

val memcpy : src:matrix -> dst:matrix -> unit
val copy   : matrix -> matrix

val row : matrix -> int -> Gsl_vector.vector

external add : matrix -> matrix -> unit = "ml_gsl_matrix_add"
external sub : matrix -> matrix -> unit = "ml_gsl_matrix_sub"
external mul_elements : matrix -> matrix -> unit = "ml_gsl_matrix_mul"
external div_elements : matrix -> matrix -> unit = "ml_gsl_matrix_div"
external scale : matrix -> float -> unit = "ml_gsl_matrix_scale"
external add_constant : matrix -> float -> unit = "ml_gsl_matrix_add_constant"
external add_diagonal : matrix -> float -> unit = "ml_gsl_matrix_add_diagonal"
external is_null : matrix -> bool = "ml_gsl_matrix_isnull"

external swap_rows : matrix -> int -> int -> unit = "ml_gsl_matrix_swap_rows"
external swap_columns : matrix -> int -> int -> unit = "ml_gsl_matrix_swap_columns"
external swap_rowcol : matrix -> int -> int -> unit = "ml_gsl_matrix_swap_rowcol"
external transpose : matrix -> matrix -> unit = "ml_gsl_matrix_transpose_memcpy"
external transpose_in_place : matrix -> unit = "ml_gsl_matrix_transpose"


module Single : sig

  type float_mat_bigarr = 
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t

  type matrix = float_mat_bigarr

  val create : ?init:float -> int -> int -> matrix

  val dims : matrix -> int * int

  val of_array  : float array -> int -> int -> matrix
  val of_arrays : float array array -> matrix
  val to_array  : matrix -> float array
  val to_arrays : matrix -> float array array

  val get : matrix -> int -> int -> float
  val set : matrix -> int -> int -> float -> unit
  val set_all  : matrix -> float -> unit
  val set_zero : matrix -> unit
  val set_id   : matrix -> unit

  val memcpy : src:matrix -> dst:matrix -> unit
  val copy   : matrix -> matrix

  val row : matrix -> int -> Gsl_vector.Single.vector

  external add : matrix -> matrix -> unit = "ml_gsl_matrix_float_add"
  external sub : matrix -> matrix -> unit = "ml_gsl_matrix_float_sub"
  external mul_elements : matrix -> matrix -> unit = "ml_gsl_matrix_float_mul"
  external div_elements : matrix -> matrix -> unit = "ml_gsl_matrix_float_div"
  external scale : matrix -> float -> unit = "ml_gsl_matrix_float_scale"
  external add_constant : matrix -> float -> unit = "ml_gsl_matrix_float_add_constant"
  external add_diagonal : matrix -> float -> unit = "ml_gsl_matrix_float_add_diagonal"
  external is_null : matrix -> bool = "ml_gsl_matrix_float_isnull"
      
  external swap_rows : matrix -> int -> int -> unit = "ml_gsl_matrix_float_swap_rows"
  external swap_columns : matrix -> int -> int -> unit = "ml_gsl_matrix_float_swap_columns"
  external swap_rowcol : matrix -> int -> int -> unit = "ml_gsl_matrix_float_swap_rowcol"
  external transpose : matrix -> matrix -> unit = "ml_gsl_matrix_float_transpose_memcpy"
  external transpose_in_place : matrix -> unit = "ml_gsl_matrix_float_transpose"
end
