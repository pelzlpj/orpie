(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002 - Olivier Andrieu                     *)
(* distributed under the terms of the GPL version 2         *)


type double_mat_flat = { 
    data : float array ;
    off  : int ;
    dim1 : int ;
    dim2 : int ; 
    tda  : int ; }

type matrix = double_mat_flat

val create : ?init:float -> int -> int -> matrix

val dims : matrix -> int * int

val of_array  : float array -> int -> int -> matrix
val of_arrays : float array array -> matrix
val to_array  : matrix -> float array
val to_arrays : matrix -> float array array

val to_array : matrix -> float array

val get : matrix -> int -> int -> float
val set : matrix -> int -> int -> float -> unit
val set_all  : matrix -> float -> unit
val set_zero : matrix -> unit
val set_id   : matrix -> unit

val memcpy : src:matrix -> dst:matrix -> unit
val copy   : matrix -> matrix

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

open Gsl_vector_flat

val submatrix : matrix -> k1:int -> k2:int -> 
  n1:int -> n2:int -> matrix

val row    : matrix -> int -> vector
val column : matrix -> int -> vector
val diagonal      : matrix -> vector
val subdiagonal   : matrix -> int -> vector
val superdiagonal : matrix -> int -> vector

val view_array : float array -> ?off:int -> 
  int -> ?tda:int -> int -> matrix
val view_vector : vector -> ?off:int -> 
  int -> ?tda:int -> int -> matrix
