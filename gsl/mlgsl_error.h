/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002 - Olivier Andrieu                     */
/* distributed under the terms of the GPL version 2         */

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>

extern const char *ml_gsl_exn_msg;
extern int         ml_gsl_exn_raise;

void ml_gsl_raise_exn(int gsl_errno);

#define MLGSLerror(x) do{ \
  int status=(x); \
  if(status){ \
    ml_gsl_exn_raise = 1; \
    CAMLreturn((ml_gsl_raise_exn(status), Val_unit)); } } while(0)

#define MLGSLexn_r(msg, n) do{ \
  ml_gsl_exn_msg = msg; \
  CAMLreturn(ml_gsl_raise_exn(n)); } while(0) 

#define MLGSLexn(msg, n) do{ \
  ml_gsl_exn_msg = msg; \
  ml_gsl_raise_exn(n); } while(0)

#define ml_gsl_error_off() do{ ml_gsl_exn_raise = 0; } while(0)
#define ml_gsl_error_on()  do{ ml_gsl_exn_raise = 1; } while(0)
