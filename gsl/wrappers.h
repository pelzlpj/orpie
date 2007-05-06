/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002-2005 - Olivier Andrieu                */
/* distributed under the terms of the GPL version 2         */

#ifndef _MLGSL_WRAPPERS_
#define _MLGSL_WRAPPERS_

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#ifdef ARCH_ALIGN_DOUBLE
#error "Architectures with double-word alignment for doubles are not supported"
#endif

#define IS_CUSTOM(v) (Tag_val(v) == Custom_tag)

#define Unoption(v) (Field((v), 0))
#define Opt_arg(v, conv, def) (Is_block(v) ? conv(Field((v),0)) : (def))
#define Val_none Val_int(0)

#define Val_negbool(x) Val_not(Val_bool(x))

#define Array_length(v)        (Wosize_val(v))
#define Double_array_length(v) (Wosize_val(v) / Double_wosize)
#define Double_array_val(v) ((double *)v)

#define Unit(v) ((v), Val_unit)

static inline value copy_two_double(double a, double b)
{
  CAMLparam0();
  CAMLlocal3(r, va, vb);
  va = copy_double(a);
  vb = copy_double(b);
  r = alloc_small(2, 0);
  Field(r, 0) = va;
  Field(r, 1) = vb;
  CAMLreturn(r);
}

static inline value copy_two_double_arr(double a, double b)
{
  value r;
  r=alloc_small(2 * Double_wosize, Double_array_tag);
  Store_double_field(r, 0, a);
  Store_double_field(r, 1, b);
  return r;
}

#define Abstract_ptr(v, p) \
  ( v=alloc_small(1, Abstract_tag), Field(v, 0)=Val_bp(p) )

#define ML1(name, conv1, convr) \
  CAMLprim value ml_##name(value arg1) \
  { CAMLparam1(arg1); \
    CAMLreturn(convr(name(conv1(arg1)))) ; }
#define ML1_alloc(name, conv1, convr) \
  CAMLprim value ml_##name(value arg1) \
  { CAMLparam1(arg1); CAMLlocal1(res); \
    convr(res, name(conv1(arg1))); \
    CAMLreturn(res); }
#define ML2(name, conv1, conv2, convr) \
  CAMLprim value ml_##name(value arg1, value arg2) \
  { CAMLparam2(arg1, arg2); \
    CAMLreturn(convr(name(conv1(arg1), conv2(arg2)))) ; }
#define ML3(name, conv1, conv2, conv3, convr) \
  CAMLprim value ml_##name(value arg1, value arg2, value arg3) \
  { CAMLparam3(arg1, arg2, arg3); \
    CAMLreturn(convr(name(conv1(arg1), conv2(arg2), conv3(arg3)))) ; }
#define ML4(name, conv1, conv2, conv3, conv4, convr) \
  CAMLprim value ml_##name(value arg1, value arg2, value arg3, value arg4) \
  { CAMLparam4(arg1, arg2, arg3, arg4); \
    CAMLreturn(convr(name(conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4)))) ; }
#define ML5(name, conv1, conv2, conv3, conv4, conv5, convr) \
  CAMLprim value ml_##name(value arg1, value arg2, value arg3, value arg4, value arg5) \
  { CAMLparam5(arg1, arg2, arg3, arg4, arg5);				\
    CAMLreturn(convr(name(conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), conv5(arg5)))) ; }

#define CONCAT2x(a,b) a ## _ ## b
#define CONCAT2(a,b) CONCAT2x(a,b)
#define CONCAT3x(a,b,c) a ## _ ## b ## _ ## c
#define CONCAT3(a,b,c) CONCAT3x(a,b,c)

#if defined (__GNUC__) || defined (DONT_USE_ALLOCA)
#define LOCALARRAY(type, x, len)  type x [(len)]
#else
#include <malloc.h>
#define LOCALARRAY(type, x, len)  type * x = ( type *) alloca(sizeof( type ) * (len))
#endif 

#endif /* _MLGSL_WRAPPERS_ */
