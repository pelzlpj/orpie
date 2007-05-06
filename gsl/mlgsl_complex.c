/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2003 - Paul Pelzl                          */
/* distributed under the terms of the GPL version 2         */

#include <caml/alloc.h>
#include <gsl/gsl_complex_math.h>
#include "mlgsl_complex.h"


#define _COMPLEX_HANDLER(funct) \
  CAMLprim value ml_gsl_complex_##funct(value Z) { \
    _DECLARE_COMPLEX2(Z,temp); \
    _CONVERT_COMPLEX(Z); \
    z_temp = gsl_complex_##funct(z_Z); \
    return copy_complex(&z_temp); \
  }


#define _COMPLEX_HANDLER2(funct) \
  CAMLprim value ml_gsl_complex_##funct(value Z, value A) { \
    _DECLARE_COMPLEX3(Z, A, temp); \
    _CONVERT_COMPLEX2(Z, A); \
    z_temp = gsl_complex_##funct(z_Z, z_A); \
    return copy_complex(&z_temp); \
  }


#define _COMPLEX_HANDLER_DOUBLE(funct) \
  CAMLprim value ml_gsl_complex_##funct(value X) { \
    gsl_complex temp; \
    temp = gsl_complex_##funct(Double_val(X)); \
    return copy_complex(&temp); \
  }


/* properties of complex numbers */
CAMLprim value ml_gsl_complex_logabs(value Z)
{
  _DECLARE_COMPLEX(Z);
  _CONVERT_COMPLEX(Z);
  return copy_double(gsl_complex_logabs(z_Z));
}


_COMPLEX_HANDLER(sqrt)
_COMPLEX_HANDLER_DOUBLE(sqrt_real)
_COMPLEX_HANDLER2(pow)


CAMLprim value ml_gsl_complex_pow_real(value Z, value X)
{
  _DECLARE_COMPLEX2(Z, temp);
  _CONVERT_COMPLEX(Z);
  z_temp = gsl_complex_pow_real(z_Z, Double_val(X));
  return copy_complex(&z_temp);
}

  
_COMPLEX_HANDLER(exp)
_COMPLEX_HANDLER(log)
_COMPLEX_HANDLER(log10)
_COMPLEX_HANDLER2(log_b)

_COMPLEX_HANDLER(sin)
_COMPLEX_HANDLER(cos)
_COMPLEX_HANDLER(tan)
_COMPLEX_HANDLER(sec)
_COMPLEX_HANDLER(csc)
_COMPLEX_HANDLER(cot)

_COMPLEX_HANDLER(arcsin)
_COMPLEX_HANDLER_DOUBLE(arcsin_real)
_COMPLEX_HANDLER(arccos)
_COMPLEX_HANDLER_DOUBLE(arccos_real)
_COMPLEX_HANDLER(arctan)
_COMPLEX_HANDLER(arcsec)
_COMPLEX_HANDLER_DOUBLE(arcsec_real)
_COMPLEX_HANDLER(arccsc)
_COMPLEX_HANDLER_DOUBLE(arccsc_real)
_COMPLEX_HANDLER(arccot)

_COMPLEX_HANDLER(sinh)
_COMPLEX_HANDLER(cosh)
_COMPLEX_HANDLER(tanh)
_COMPLEX_HANDLER(sech)
_COMPLEX_HANDLER(csch)
_COMPLEX_HANDLER(coth)

_COMPLEX_HANDLER(arcsinh)
_COMPLEX_HANDLER(arccosh)
_COMPLEX_HANDLER_DOUBLE(arccosh_real)
_COMPLEX_HANDLER(arctanh)
_COMPLEX_HANDLER_DOUBLE(arctanh_real)
_COMPLEX_HANDLER(arcsech)
_COMPLEX_HANDLER(arccsch)
_COMPLEX_HANDLER(arccoth)
