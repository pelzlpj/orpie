(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002 - Olivier Andrieu                     *)
(* distributed under the terms of the GPL version 2         *)

(** Generic variant types for vectors and matrices *)

(** {3 Real values} *)

type vec = [
  | `V of Gsl_vector.vector
  | `VF of Gsl_vector_flat.vector ]

val vec_convert :
  ?protect:bool ->
  [< `A of float array 
   | `VF of Gsl_vector_flat.vector 
   | `V of Gsl_vector.vector] -> [> vec]
       
type mat = [
  | `M of Gsl_matrix.matrix
  | `MF of Gsl_matrix_flat.matrix ]

val mat_convert :
  ?protect:bool ->
  [< `M of Gsl_matrix.matrix
   | `MF of Gsl_matrix_flat.matrix
   | `A of float array * int * int
   | `AA of float array array] -> [> mat]

val mat_flat :
  ?protect:bool ->
  [< `M of Gsl_matrix.matrix
   | `MF of Gsl_matrix_flat.matrix
   | `A of float array * int * int
   | `AA of float array array] -> Gsl_matrix_flat.matrix
    
(** {3 Complex values} *)

type cvec = [
  | `CV  of Gsl_vector_complex.vector
  | `CVF of Gsl_vector_complex_flat.vector ]

type cmat = [
  | `CM  of Gsl_matrix_complex.matrix
  | `CMF of Gsl_matrix_complex_flat.matrix ]

val cmat_convert :
  ?protect:bool ->
  [< `CM of Gsl_matrix_complex.matrix
   | `CMF of Gsl_matrix_complex_flat.matrix
   | `CA of Gsl_complex.complex_array * int * int ] -> 
  [> cmat]


(** {3 Generic vector operations} *)

val length    : [< vec| cvec] -> int
val to_array  : [< vec] -> float array
val v_copy    : [< vec] -> [> vec]
val subvector : [< vec] -> off:int -> len:int -> [> vec]

external v_memcpy : [< vec] -> [< vec] -> unit 
    = "ml_gsl_vector_memcpy"

external v_add : [< vec] -> [< vec] -> unit 
    = "ml_gsl_vector_add"
external v_sub : [< vec] -> [< vec] -> unit 
    = "ml_gsl_vector_sub"
external v_mul : [< vec] -> [< vec] -> unit 
    = "ml_gsl_vector_mul"
external v_div : [< vec] -> [< vec] -> unit 
    = "ml_gsl_vector_div"

external v_max : [< vec] -> float
    = "ml_gsl_vector_max"
external v_min : [< vec] -> float
    = "ml_gsl_vector_min"
external v_minmax : [< vec] -> float * float
    = "ml_gsl_vector_minmax"
external v_max_index : [< vec] -> int
    = "ml_gsl_vector_maxindex"
external v_min_index : [< vec] -> int
    = "ml_gsl_vector_minindex"
external v_minmax_index : [< vec] -> int * int
    = "ml_gsl_vector_minmaxindex"

(** {3 Generic matrix operations} *)

val dims      : [< mat| cmat] -> int * int
val tmp       : [< mat] -> [> `M of Gsl_matrix.matrix]
val to_arrays : [< mat] -> float array array
val m_copy    : [< mat] -> [> mat]

external m_memcpy : [< mat] -> [< mat] -> unit 
    = "ml_gsl_matrix_memcpy"

external m_add : [< mat] -> [< mat] -> unit 
    = "ml_gsl_matrix_add"
external m_sub : [< mat] -> [< mat] -> unit 
    = "ml_gsl_matrix_sub"
external m_mul : [< mat] -> [< mat] -> unit 
    = "ml_gsl_matrix_mul"
external m_div : [< mat] -> [< mat] -> unit 
    = "ml_gsl_matrix_div"
external m_add_diagonal : [< mat] -> float -> unit
    = "ml_gsl_matrix_add_diagonal"

external swap_rows : [< mat] -> int -> int -> unit 
    = "ml_gsl_matrix_swap_rows"
external swap_columns : [< mat] -> int -> int -> unit
    = "ml_gsl_matrix_swap_columns"
external swap_rowcol : [< mat] -> int -> int -> unit
    = "ml_gsl_matrix_swap_rowcol"
external transpose : [< mat] -> [< mat] -> unit
    = "ml_gsl_matrix_transpose_memcpy"
external transpose_in_place : [< mat] -> unit
    = "ml_gsl_matrix_transpose"

(** {3 Other generic operations} *)

val is_null      : [< vec | mat] -> bool
val scale        : [< vec | mat] -> float -> unit
val add_constant : [< vec | mat] -> float -> unit
