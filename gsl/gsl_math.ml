(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002-2005 - Olivier Andrieu                *)
(* distributed under the terms of the GPL version 2         *)

let e          = 2.71828182845904523536028747135 (* e *)

let log2e      = 1.44269504088896340735992468100 (* log_2 (e) *)

let log10e     = 0.43429448190325182765112891892 (* log_10 (e) *)

let sqrt2      = 1.41421356237309504880168872421 (* sqrt(2) *)

let sqrt1_2    = 0.70710678118654752440084436210 (* sqrt(1/2) *)

let sqrt3      = 1.73205080756887729352744634151 (* sqrt(3) *)

let pi         = 3.14159265358979323846264338328 (* pi *)

let pi_2       = 1.57079632679489661923132169164 (* pi/2 *)

let pi_4       = 0.78539816339744830966156608458 (* pi/4 *)

let sqrtpi     = 1.77245385090551602729816748334 (* sqrt(pi) *)

let i_2_sqrtpi = 1.12837916709551257389615890312 (* 2/sqrt(pi) *)

let i_1_pi     = 0.31830988618379067153776752675 (* 1/pi *)

let i_2_pi     = 0.63661977236758134307553505349 (* 2/pi *)

let ln10       = 2.30258509299404568401799145468 (* ln(10) *)

let ln2        = 0.69314718055994530941723212146 (* ln(2) *)

let lnpi       = 1.14472988584940017414342735135 (* ln(pi) *)

let euler      = 0.57721566490153286060651209008 (* Euler constant *)


let rec unsafe_pow_int x = function
  | 1 -> x
  | n when n mod 2 = 0 ->
      unsafe_pow_int (x *. x) (n/2)
  | n ->
      x *. (unsafe_pow_int x (pred n))

let pow_int x = function
  | 0 -> 1.
  | n when n > 0 -> unsafe_pow_int x n 
  | _ -> invalid_arg "pow_int"

external log1p : float -> float
    = "ml_gsl_log1p" "gsl_log1p" "float"

external expm1 : float -> float
    = "ml_gsl_expm1" "gsl_expm1" "float"

external hypot : float -> float -> float
    = "ml_gsl_hypot" "gsl_hypot" "float"

external acosh : float -> float
    = "ml_gsl_acosh" "gsl_acosh" "float"

external asinh : float -> float
    = "ml_gsl_asinh" "gsl_asinh" "float"

external atanh : float -> float
    = "ml_gsl_atanh" "gsl_atanh" "float"

external fcmp : float -> float -> epsilon:float -> int
    = "ml_gsl_fcmp"
