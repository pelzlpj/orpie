(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002 - Olivier Andrieu                     *)
(* distributed under the terms of the GPL version 2         *)

external gsl_version : unit -> string
    = "ml_gsl_version"

let version = gsl_version ()

type errno =
  | CONTINUE (* iteration has not converged *)
  | FAILURE  
  | EDOM     (* input domain error, e.g sqrt(-1) *)
  | ERANGE   (* output range error, e.g. exp(1e100) *)
  | EFAULT   (* invalid pointer *)
  | EINVAL   (* invalid argument supplied by user *)
  | EFAILED  (* generic failure *)
  | EFACTOR  (* factorization failed *)
  | ESANITY  (* sanity check failed - shouldn't happen *)
  | ENOMEM   (* malloc failed *)
  | EBADFUNC (* problem with user-supplied function *)
  | ERUNAWAY (* iterative process is out of control *)
  | EMAXITER (* exceeded max number of iterations *)
  | EZERODIV (* tried to divide by zero *)
  | EBADTOL  (* user specified an invalid tolerance *)
  | ETOL     (* failed to reach the specified tolerance *)
  | EUNDRFLW (* underflow *)
  | EOVRFLW  (* overflow  *)
  | ELOSS    (* loss of accuracy *)
  | EROUND   (* failed because of roundoff error *)
  | EBADLEN  (* matrix, vector lengths are not conformant *)
  | ENOTSQR  (* matrix not square *)
  | ESING    (* apparent singularity detected *)
  | EDIVERGE (* integral or series is divergent *)
  | EUNSUP   (* requested feature is not supported by the hardware *)
  | EUNIMPL  (* requested feature not (yet) implemented *)
  | ECACHE   (* cache limit exceeded *)
  | ETABLE   (* table limit exceeded *)
  | ENOPROG  (* iteration is not making progress towards solution *)
  | ENOPROGJ (* jacobian evaluations are not improving the solution *)
  | ETOLF    (* cannot reach the specified tolerance in F *)
  | ETOLX    (* cannot reach the specified tolerance in X *)
  | ETOLG    (* cannot reach the specified tolerance in gradient *)
  | EOF      (* end of file *)

exception Gsl_exn of (errno * string)

external raise_caml_exn : bool -> unit = "ml_gsl_error_init"

let init () = 
  Callback.register_exception "mlgsl_exn" (Gsl_exn (CONTINUE, "")) ;
  raise_caml_exn true

let uninit () = 
  raise_caml_exn false

external strerror : errno -> string = "ml_gsl_strerror"

let string_of_errno = function
  | CONTINUE -> "CONTINUE"
  | FAILURE  -> "FAILURE"
  | EDOM     -> "EDOM"
  | ERANGE   -> "ERANGE"
  | EFAULT   -> "EFAULT"
  | EINVAL   -> "EINVAL"
  | EFAILED  -> "EFAILED"
  | EFACTOR  -> "EFACTOR"
  | ESANITY  -> "ESANITY"
  | ENOMEM   -> "ENOMEM"
  | EBADFUNC -> "EBADFUNC"
  | ERUNAWAY -> "ERUNAWAY"
  | EMAXITER -> "EMAXITER"
  | EZERODIV -> "EZERODIV"
  | EBADTOL  -> "EBADTOL"
  | ETOL     -> "ETOL"
  | EUNDRFLW -> "EUNDRFLW"
  | EOVRFLW  -> "EOVRFLW"
  | ELOSS    -> "ELOSS"
  | EROUND   -> "EROUND"
  | EBADLEN  -> "EBADLEN"
  | ENOTSQR  -> "ENOTSQR"
  | ESING    -> "ESING"
  | EDIVERGE -> "EDIVERGE"
  | EUNSUP   -> "EUNSUP"
  | EUNIMPL  -> "EUNIMPL"
  | ECACHE   -> "ECACHE"
  | ETABLE   -> "ETABLE"
  | ENOPROG  -> "ENOPROG"
  | ENOPROGJ -> "ENOPROGJ"
  | ETOLF    -> "ETOLF"
  | ETOLX    -> "ETOLX"
  | ETOLG    -> "ETOLG"
  | EOF      -> "EOF"

let pprint_exn = function
  | Gsl_exn (errno, msg) ->
      Printf.sprintf "GSL error: %s, %s\n  %s" 
	(string_of_errno errno)
	(strerror errno) msg
  | e -> 
      Printexc.to_string e

let handle_exn f x =
  try f x 
  with exn -> prerr_endline (pprint_exn exn) ; raise exn
