(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002-2005 - Olivier Andrieu                *)
(* distributed under the terms of the GPL version 2         *)

(** Error reporting *)

(** version of GSL library *)
val version : string

type errno =
  | CONTINUE (** iteration has not converged *)
  | FAILURE  
  | EDOM     (** input domain error, e.g sqrt(-1) *)
  | ERANGE   (** output range error, e.g. exp(1e100) *)
  | EFAULT   (** invalid pointer *)
  | EINVAL   (** invalid argument supplied by user *)
  | EFAILED  (** generic failure *)
  | EFACTOR  (** factorization failed *)
  | ESANITY  (** sanity check failed - shouldn't happen *)
  | ENOMEM   (** malloc failed *)
  | EBADFUNC (** problem with user-supplied function *)
  | ERUNAWAY (** iterative process is out of control *)
  | EMAXITER (** exceeded max number of iterations *)
  | EZERODIV (** tried to divide by zero *)
  | EBADTOL  (** user specified an invalid tolerance *)
  | ETOL     (** failed to reach the specified tolerance *)
  | EUNDRFLW (** underflow *)
  | EOVRFLW  (** overflow  *)
  | ELOSS    (** loss of accuracy *)
  | EROUND   (** failed because of roundoff error *)
  | EBADLEN  (** matrix, vector lengths are not conformant *)
  | ENOTSQR  (** matrix not square *)
  | ESING    (** apparent singularity detected *)
  | EDIVERGE (** integral or series is divergent *)
  | EUNSUP   (** requested feature is not supported by the hardware *)
  | EUNIMPL  (** requested feature not (yet) implemented *)
  | ECACHE   (** cache limit exceeded *)
  | ETABLE   (** table limit exceeded *)
  | ENOPROG  (** iteration is not making progress towards solution *)
  | ENOPROGJ (** jacobian evaluations are not improving the solution *)
  | ETOLF    (** cannot reach the specified tolerance in F *)
  | ETOLX    (** cannot reach the specified tolerance in X *)
  | ETOLG    (** cannot reach the specified tolerance in gradient *)
  | EOF      (** end of file *)

exception Gsl_exn of (errno * string)

val init   : unit -> unit
val uninit : unit -> unit

external strerror : errno -> string = "ml_gsl_strerror"
val string_of_errno : errno -> string

val pprint_exn : exn -> string

val handle_exn : ('a -> 'b) -> 'a -> 'b
