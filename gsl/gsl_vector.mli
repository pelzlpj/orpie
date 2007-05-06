(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002-2005 - Olivier Andrieu                *)
(* distributed under the terms of the GPL version 2         *)

(** Vector of floats implemented with [Bigarray] *)

(** {3 Double precision} *)

type double_vector_bigarr =
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

type vector = double_vector_bigarr

(** {4 Operations} *)

val create   : ?init:float -> int -> vector
val of_array : float array -> vector
val to_array : vector -> float array

val length : vector -> int
val get : vector -> int -> float
val set : vector -> int -> float -> unit

val set_all   : vector -> float -> unit
val set_zero  : vector -> unit
val set_basis : vector -> int -> unit

val memcpy : src:vector -> dst:vector -> unit
val copy   : vector -> vector

val swap_element : vector -> int -> int -> unit
val reverse : vector -> unit

external add : vector -> vector -> unit 
    = "ml_gsl_vector_add"
external sub : vector -> vector -> unit 
    = "ml_gsl_vector_sub"
external mul : vector -> vector -> unit 
    = "ml_gsl_vector_mul"
external div : vector -> vector -> unit 
    = "ml_gsl_vector_div"
external scale : vector -> float -> unit 
    = "ml_gsl_vector_scale"
external add_constant : vector -> float -> unit
    = "ml_gsl_vector_add_constant"
external is_null : vector -> bool
    = "ml_gsl_vector_isnull"

external max : vector -> float
    = "ml_gsl_vector_max"
external min : vector -> float
    = "ml_gsl_vector_min"
external minmax : vector -> float * float
    = "ml_gsl_vector_minmax"
external max_index : vector -> int
    = "ml_gsl_vector_maxindex"
external min_index : vector -> int
    = "ml_gsl_vector_minindex"
external minmax_index : vector -> int * int
    = "ml_gsl_vector_minmaxindex"

(** {4 No-copy operations} *)

val subvector : vector -> off:int -> len:int -> vector

(** {3 Single precision} *)

module Single : sig
  type float_vector_bigarr =
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

  type vector = float_vector_bigarr

  val create   : ?init:float -> int -> vector
  val of_array : float array -> vector
  val to_array : vector -> float array

  val length : vector -> int
  val get : vector -> int -> float
  val set : vector -> int -> float -> unit

  val set_all   : vector -> float -> unit
  val set_zero  : vector -> unit
  val set_basis : vector -> int -> unit

  val memcpy : src:vector -> dst:vector -> unit
  val copy   : vector -> vector

  val swap_element : vector -> int -> int -> unit
  val reverse : vector -> unit

  external add : vector -> vector -> unit 
      = "ml_gsl_vector_float_add"
  external sub : vector -> vector -> unit 
      = "ml_gsl_vector_float_sub"
  external mul : vector -> vector -> unit 
      = "ml_gsl_vector_float_mul"
  external div : vector -> vector -> unit 
      = "ml_gsl_vector_float_div"
  external scale : vector -> float -> unit 
      = "ml_gsl_vector_float_scale"
  external add_constant : vector -> float -> unit
      = "ml_gsl_vector_float_add_constant"
  external is_null : vector -> bool
      = "ml_gsl_vector_float_isnull"
      
  external max : vector -> float
      = "ml_gsl_vector_float_max"
  external min : vector -> float
      = "ml_gsl_vector_float_min"
  external minmax : vector -> float * float
      = "ml_gsl_vector_float_minmax"
  external max_index : vector -> int
      = "ml_gsl_vector_float_maxindex"
  external min_index : vector -> int
      = "ml_gsl_vector_float_minindex"
  external minmax_index : vector -> int * int
      = "ml_gsl_vector_float_minmaxindex"

 (** {4 No-copy operations} *)
      
  val subvector : vector -> off:int -> len:int -> vector
end
