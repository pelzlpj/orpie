/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002 - Olivier Andrieu                     */
/* distributed under the terms of the GPL version 2         */

#include <caml/alloc.h>

#ifndef FUNCTION
#error pb with include files
#endif

value FUNCTION(ml_gsl_vector,memcpy)(value a, value b)
{
  _DECLARE_VECTOR2(a,b);
  _CONVERT_VECTOR2(a,b);
  FUNCTION(gsl_vector,memcpy)(&v_b, &v_a);
  return Val_unit;
}

value FUNCTION(ml_gsl_vector,add)(value a, value b)
{
  _DECLARE_VECTOR2(a,b);
  _CONVERT_VECTOR2(a,b);
  FUNCTION(gsl_vector,add)(&v_a, &v_b);
  return Val_unit;
}

value FUNCTION(ml_gsl_vector,sub)(value a, value b)
{
  _DECLARE_VECTOR2(a,b);
  _CONVERT_VECTOR2(a,b);
  FUNCTION(gsl_vector,sub)(&v_a, &v_b);
  return Val_unit;
}

value FUNCTION(ml_gsl_vector,mul)(value a, value b)
{
  _DECLARE_VECTOR2(a,b);
  _CONVERT_VECTOR2(a,b);
  FUNCTION(gsl_vector,mul)(&v_a, &v_b);
  return Val_unit;
}

value FUNCTION(ml_gsl_vector,div)(value a, value b)
{
  _DECLARE_VECTOR2(a,b);
  _CONVERT_VECTOR2(a,b);
  FUNCTION(gsl_vector,div)(&v_a, &v_b);
  return Val_unit;
}

value FUNCTION(ml_gsl_vector,scale)(value a, value x)
{
  _DECLARE_VECTOR(a);
  _CONVERT_VECTOR(a);
  FUNCTION(gsl_vector,scale)(&v_a, Double_val(x));
  return Val_unit;
}

value FUNCTION(ml_gsl_vector,add_constant)(value a, value x)
{
  _DECLARE_VECTOR(a);
  _CONVERT_VECTOR(a);
  FUNCTION(gsl_vector,add_constant)(&v_a, Double_val(x));
  return Val_unit;
}

value FUNCTION(ml_gsl_vector,isnull)(value a)
{
  int r;
  _DECLARE_VECTOR(a);
  _CONVERT_VECTOR(a);
  r = FUNCTION(gsl_vector,isnull)(&v_a);
  return Val_bool(r);
}

value FUNCTION(ml_gsl_vector,max)(value a)
{
  _DECLARE_VECTOR(a);
  _CONVERT_VECTOR(a);
  return copy_double(FUNCTION(gsl_vector,max)(&v_a));
}

value FUNCTION(ml_gsl_vector,min)(value a)
{
  _DECLARE_VECTOR(a);
  _CONVERT_VECTOR(a);
  return copy_double(FUNCTION(gsl_vector,min)(&v_a));
}

value FUNCTION(ml_gsl_vector,minmax)(value a)
{
  BASE_TYPE x,y;
  _DECLARE_VECTOR(a);
  _CONVERT_VECTOR(a);
  FUNCTION(gsl_vector,minmax)(&v_a, &x, &y);
  return copy_two_double(x, y);
}

value FUNCTION(ml_gsl_vector,maxindex)(value a)
{
  _DECLARE_VECTOR(a);
  _CONVERT_VECTOR(a);
  return Val_int(FUNCTION(gsl_vector,max_index)(&v_a));
}

value FUNCTION(ml_gsl_vector,minindex)(value a)
{
  _DECLARE_VECTOR(a);
  _CONVERT_VECTOR(a);
  return Val_int(FUNCTION(gsl_vector,min_index)(&v_a));
}

value FUNCTION(ml_gsl_vector,minmaxindex)(value a)
{
  size_t x,y;
  value v;
  _DECLARE_VECTOR(a);
  _CONVERT_VECTOR(a);
  FUNCTION(gsl_vector,minmax_index)(&v_a, &x, &y);
  v=alloc_small(2, 0);
  Field(v, 0) = Val_int(x);
  Field(v, 1) = Val_int(y);
  return v;
}
