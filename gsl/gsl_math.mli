(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002 - Olivier Andrieu                     *)
(* distributed under the terms of the GPL version 2         *)

(** Mathematical constants and some simple functions *)

(** {3 Constants} *)

val e          : float (** e *)             
val log2e      : float (** log_2 (e) *)     
val log10e     : float (** log_10 (e) *)    
val sqrt2      : float (** sqrt(2) *)       
val sqrt1_2    : float (** sqrt(1/2) *)     
val sqrt3      : float (** sqrt(3) *)       
val pi         : float (** pi *)            
val pi_2       : float (** pi/2 *)          
val pi_4       : float (** pi/4 *)          
val sqrtpi     : float (** sqrt(pi) *)      
val i_2_sqrtpi : float (** 2/sqrt(pi) *)    
val i_1_pi     : float (** 1/pi *)          
val i_2_pi     : float (** 2/pi *)          
val ln10       : float (** ln(10) *)        
val ln2        : float (** ln(2) *)         
val lnpi       : float (** ln(pi) *)        
val euler      : float (** Euler constant *)

(** {3 Simple Functions} *)

val pow_int    : float -> int -> float

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
