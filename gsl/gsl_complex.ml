(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002-2005, 2003 - Olivier Andrieu, Paul Pelzl   *)
(* distributed under the terms of the GPL version 2         *)


type complex = Complex.t =
    { re : float ; im : float }

let complex ~re ~im =
  { re = re ; im = im }

type complex_array = float array

let set a i c = 
  a.(2*i) <- c.re ;
  a.(2*i + 1) <- c.im

let get a i =
  { re = a.(2*i); im = a.(2*i+1) }

let unpack ca =
  let len = Array.length ca in
  if len mod 2 <> 0
  then invalid_arg "unpack_complex_array" ;
  Array.init (len / 2) (get ca)

let pack a = 
  let len = Array.length a in
  let ca  = Array.make (2 * len) 0. in
  for i=0 to pred len do
    ca.(2*i)   <- a.(i).re ;
    ca.(2*i+1) <- a.(i).im
  done ;
  ca

let mult a b = 
  if Array.length a mod 2 <> 0
  then invalid_arg "mult: not a complex array" ;
  let len = (Array.length a) / 2 in
  for i = 0 to pred len do
    let re = a.(2*i) *. b.(2*i) -. a.(2*i+1) *. b.(2*i+1) in
    let im = a.(2*i) *. b.(2*i+1) +. a.(2*i+1) *. b.(2*i) in
    a.(2*i) <- re ;
    a.(2*i+1) <- im
  done


(* added by Paul Pelzl, 2003/12/25 *)
let rect x y = {re = x; im = y}
let polar = Complex.polar
  
let arg = Complex.arg
let abs = Complex.norm
let abs2 = Complex.norm2
external logabs : complex -> float
    = "ml_gsl_complex_logabs"

let add = Complex.add
let sub = Complex.sub
let mul = Complex.mul
let div = Complex.div

let add_real a x = {re = a.re +. x; im = a.im}
let sub_real a x = {re = a.re -. x; im = a.im}
let mul_real a x = {re = a.re *. x; im = a.im *. x}
let div_real a x = {re = a.re /. x; im = a.im /. x}

let add_imag a y = {re = a.re; im = a.im +. y}
let sub_imag a y = {re = a.re; im = a.im -. y}
let mul_imag a y = {re = a.im *. (~-. y); im = a.re *. y}
let div_imag a y = {re = a.im /. y; im = a.re /. (~-. y)}

let conjugate = Complex.conj
let inverse   = Complex.inv
let negative  = Complex.neg

(* elementary complex functions *)
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

(* complex trigonometric functions *)
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

(* inverse complex trigonometric functions *)
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

(* complex hyperbolic functions *)
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

(* inverse complex hyperbolic functions *)
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

