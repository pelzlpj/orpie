(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002-2005 - Olivier Andrieu                *)
(* distributed under the terms of the GPL version 2         *)

(** Vector of complex numbers implemented with a [Bigarray] *)

open Bigarray
open Gsl_complex

type complex_double_vector_bigarr = 
    (Complex.t, complex64_elt, c_layout) Array1.t

type vector = complex_double_vector_bigarr

val create   : ?init:complex -> int -> vector
val of_array : complex array -> vector
val to_array : vector -> complex array

val of_complex_array : complex_array -> vector
val to_complex_array : vector -> complex_array

val length : vector -> int
val get : vector -> int -> complex
val set : vector -> int -> complex -> unit

val set_all   : vector -> complex -> unit
val set_zero  : vector -> unit
val set_basis : vector -> int -> unit

val memcpy : src:vector -> dst:vector -> unit
val copy   : vector -> vector

val swap_element : vector -> int -> int -> unit
val reverse : vector -> unit

val subvector : vector -> off:int -> len:int -> vector


module Single :
  sig
    type complex_float_vector_bigarr = 
	(Complex.t, complex32_elt, c_layout) Array1.t
	  
    type vector = complex_float_vector_bigarr
	  
    val create   : ?init:complex -> int -> vector
    val of_array : complex array -> vector
    val to_array : vector -> complex array

    val of_complex_array : complex_array -> vector
    val to_complex_array : vector -> complex_array

    val length : vector -> int
    val get : vector -> int -> complex
    val set : vector -> int -> complex -> unit

    val set_all   : vector -> complex -> unit
    val set_zero  : vector -> unit
    val set_basis : vector -> int -> unit

    val memcpy : src:vector -> dst:vector -> unit
    val copy   : vector -> vector

    val swap_element : vector -> int -> int -> unit
    val reverse : vector -> unit

    val subvector : vector -> off:int -> len:int -> vector
  end
