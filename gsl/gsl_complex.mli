(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002, 2003 - Olivier Andrieu, Paul Pelzl   *)
(* distributed under the terms of the GPL version 2         *)

(** Complex arithmetic and simple functions *)

type complex = Complex.t =
    { re : float ; im : float }

val complex : re:float -> im:float -> complex

type complex_array = float array

val set : complex_array -> int -> complex -> unit
val get : complex_array -> int -> complex

val unpack : complex_array -> complex array
val pack   : complex array -> complex_array

val mult : complex_array -> complex_array -> unit

(* added by Paul Pelzl 2003/12/25 *)
val rect  : float -> float -> complex
val polar : float -> float -> complex

(** {4 Properties of complex numbers} *)

val arg    : complex -> float
val abs    : complex -> float
val abs2   : complex -> float
external logabs : complex -> float
    = "ml_gsl_complex_logabs"

(** {4 Complex arithmetic operators} *)

val add : complex -> complex -> complex
val sub : complex -> complex -> complex
val mul : complex -> complex -> complex
val div : complex -> complex -> complex

val add_real : complex -> float -> complex
val sub_real : complex -> float -> complex
val mul_real : complex -> float -> complex
val div_real : complex -> float -> complex

val add_imag : complex -> float -> complex
val sub_imag : complex -> float -> complex
val mul_imag : complex -> float -> complex
val div_imag : complex -> float -> complex

val conjugate : complex -> complex
val inverse   : complex -> complex
val negative  : complex -> complex

(** {4 Elementary complex functions} *)

external sqrt : complex -> complex
    = "ml_gsl_complex_sqrt"

external sqrt_real : float -> complex
    = "ml_gsl_complex_sqrt_real"

external pow : complex -> complex -> complex
    = "ml_gsl_complex_pow"

external pow_real : complex -> float -> complex
    = "ml_gsl_complex_pow_real"

external exp : complex -> complex
    = "ml_gsl_complex_exp"

external log : complex -> complex
    = "ml_gsl_complex_log"

external log10 : complex -> complex
    = "ml_gsl_complex_log10"

external log_b : complex -> complex -> complex
    = "ml_gsl_complex_log_b"

(** {4 Complex trigonometric functions} *)

external sin : complex -> complex
    = "ml_gsl_complex_sin"

external cos : complex -> complex
    = "ml_gsl_complex_cos"

external tan : complex -> complex
    = "ml_gsl_complex_tan"

external sec : complex -> complex
    = "ml_gsl_complex_sec"

external csc : complex -> complex
    = "ml_gsl_complex_csc"

external cot : complex -> complex
    = "ml_gsl_complex_cot"

(** {4 Inverse complex trigonometric functions} *)

external arcsin : complex -> complex
    = "ml_gsl_complex_arcsin"

external arcsin_real : float -> complex
    = "ml_gsl_complex_arcsin_real"

external arccos : complex -> complex
    = "ml_gsl_complex_arccos"

external arccos_real : float -> complex
    = "ml_gsl_complex_arccos_real"

external arctan : complex -> complex
    = "ml_gsl_complex_arctan"

external arcsec : complex -> complex
    = "ml_gsl_complex_arcsec"

external arcsec_real : float -> complex
    = "ml_gsl_complex_arcsec_real"

external arccsc : complex -> complex
    = "ml_gsl_complex_arccsc"

external arccsc_real : float -> complex
    = "ml_gsl_complex_arccsc_real"

external arccot : complex -> complex
    = "ml_gsl_complex_arccot"

(** {4 Complex hyperbolic functions} *)

external sinh : complex -> complex
    = "ml_gsl_complex_sinh"

external cosh : complex -> complex
    = "ml_gsl_complex_cosh"

external tanh : complex -> complex
    = "ml_gsl_complex_tanh"

external sech : complex -> complex
    = "ml_gsl_complex_sech"

external csch : complex -> complex
    = "ml_gsl_complex_csch"

external coth : complex -> complex
    = "ml_gsl_complex_coth"

(** {4 Inverse complex hyperbolic functions} *)

external arcsinh : complex -> complex
    = "ml_gsl_complex_arcsinh"

external arccosh : complex -> complex
    = "ml_gsl_complex_arccosh"

external arccosh_real : float -> complex
    = "ml_gsl_complex_arccosh_real"

external arctanh : complex -> complex
    = "ml_gsl_complex_arctanh"

external arctanh_real : float -> complex
    = "ml_gsl_complex_arctanh_real"

external arcsech : complex -> complex
    = "ml_gsl_complex_arcsech"

external arccsch : complex -> complex
    = "ml_gsl_complex_arccsch"

external arccoth : complex -> complex
    = "ml_gsl_complex_arccoth"

