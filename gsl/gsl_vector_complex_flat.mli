(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002 - Olivier Andrieu                     *)
(* distributed under the terms of the GPL version 2         *)

(** Vector of complex numbers implemented with a [float array] *)

type complex_vector_flat = 
    { data   : float array ;
      off    : int ;
      len    : int ;
      stride : int ; }

type vector = complex_vector_flat

(** {3 Operations} *)

open Gsl_complex

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

val memcpy : vector -> vector -> unit
val copy   : vector -> vector

val swap_element : vector -> int -> int -> unit
val reverse : vector -> unit

(** {3 No-copy operations} *)

val subvector : ?stride:int -> vector -> off:int -> len:int -> vector
val view_complex_array : ?stride:int -> ?off:int -> ?len:int -> complex_array -> vector

val real : vector -> Gsl_vector_flat.vector
val imag : vector -> Gsl_vector_flat.vector

