/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002 - Olivier Andrieu                     */
/* distributed under the terms of the GPL version 2         */

#include <string.h>

#include <gsl/gsl_math.h>

#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/bigarray.h>

#include "wrappers.h"
#include "mlgsl_fun.h"


/* CALLBACKS */
double gslfun_callback(double x, void *params)
{
  struct callback_params *p=params;
  value res;
  value v_x = copy_double(x);
  res=callback_exn(p->closure, v_x);
  if(Is_exception_result(res))
    return GSL_NAN;
  return Double_val(res);
}

double gslfun_callback_indir(double x, void *params)
{
  value res;
  value v_x = copy_double(x);
  value *closure = params;
  res=callback_exn(*closure, v_x);
  if(Is_exception_result(res))
    return GSL_NAN;
  return Double_val(res);
}
 
/* FDF CALLBACKS */
double gslfun_callback_f(double x, void *params)
{
  struct callback_params *p=params;
  value res;
  value v_x=copy_double(x);
  res=callback_exn(Field(p->closure, 0), v_x);
  if(Is_exception_result(res))
    return GSL_NAN;
  return Double_val(res);
}

double gslfun_callback_df(double x, void *params)
{
  struct callback_params *p=params;
  value res;
  value v_x=copy_double(x);
  res=callback_exn(Field(p->closure, 1), v_x);
  if(Is_exception_result(res))
    return GSL_NAN;
  return Double_val(res);
}

void gslfun_callback_fdf(double x, void *params, 
			 double *f, double *df)
{
  struct callback_params *p=params;
  value res;
  value v_x=copy_double(x);
  res=callback_exn(Field(p->closure, 2), v_x);
  if(Is_exception_result(res)){
    *f=GSL_NAN; *df=GSL_NAN;
    return;
  }
  *f =Double_val(Field(res, 0));
  *df=Double_val(Field(res, 1));
}


/* MONTE CALLBACKS */
double gsl_monte_callback(double *x_arr, size_t dim, void *params)
{
  struct callback_params *p=params;
  value res;

  memcpy(Double_array_val(p->dbl), x_arr, dim*sizeof(double));
  res=callback_exn(p->closure, p->dbl);
  if(Is_exception_result(res))
    return GSL_NAN;
  return Double_val(res);
}

double gsl_monte_callback_fast(double *x_arr, size_t dim, void *params)
{
  struct callback_params *p=params;
  value res;

  res=callback_exn(p->closure, (value)x_arr);
  if(Is_exception_result(res))
    return GSL_NAN;
  return Double_val(res);
}



/* MULTIROOT CALLBACKS */
int gsl_multiroot_callback(const gsl_vector *x, void *params, gsl_vector *F)
{
  int barr_flags = BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT | BIGARRAY_EXTERNAL;
  struct callback_params *p=params;
  value x_barr, f_barr;
  int len = x->size;
  LOCALARRAY(double, x_arr, len); 
  LOCALARRAY(double, f_arr, len); 
  gsl_vector_view x_v, f_v;
  value res;

  x_v = gsl_vector_view_array(x_arr, len);
  f_v = gsl_vector_view_array(f_arr, len);
  gsl_vector_memcpy(&x_v.vector, x);
  x_barr = alloc_bigarray_dims(barr_flags, 1, x_arr, len);
  f_barr = alloc_bigarray_dims(barr_flags, 1, f_arr, len);
  res=callback2_exn(p->closure, x_barr, f_barr);
  if(Is_exception_result(res))
    return GSL_EFAILED;
  gsl_vector_memcpy(F, &f_v.vector);
  return GSL_SUCCESS;
}

int gsl_multiroot_callback_f(const gsl_vector *x, void *params, gsl_vector *F)
{
  int barr_flags = BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT | BIGARRAY_EXTERNAL;
  struct callback_params *p=params;
  value x_barr, f_barr;
  int len = x->size;
  LOCALARRAY(double, x_arr, len); 
  LOCALARRAY(double, f_arr, len); 
  gsl_vector_view x_v, f_v;
  value res;

  x_v = gsl_vector_view_array(x_arr, len);
  f_v = gsl_vector_view_array(f_arr, len);
  gsl_vector_memcpy(&x_v.vector, x);
  x_barr = alloc_bigarray_dims(barr_flags, 1, x_arr, len);
  f_barr = alloc_bigarray_dims(barr_flags, 1, f_arr, len);
  res=callback2_exn(Field(p->closure, 0), x_barr, f_barr);
  if(Is_exception_result(res))
    return GSL_EFAILED;
  gsl_vector_memcpy(F, &f_v.vector);
  return GSL_SUCCESS;
}

int gsl_multiroot_callback_df(const gsl_vector *x, void *params, gsl_matrix *J)
{
  int barr_flags = BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT | BIGARRAY_EXTERNAL;
  struct callback_params *p=params;
  value x_barr, j_barr;
  int len = x->size;
  LOCALARRAY(double, x_arr, len); 
  LOCALARRAY(double, j_arr, len*len); 
  gsl_vector_view x_v;
  gsl_matrix_view j_v;
  value res;

  x_v = gsl_vector_view_array(x_arr, len);
  j_v = gsl_matrix_view_array(j_arr, len, len);
  gsl_vector_memcpy(&x_v.vector, x);
  x_barr = alloc_bigarray_dims(barr_flags, 1, x_arr, len);
  j_barr = alloc_bigarray_dims(barr_flags, 2, j_arr, len, len);
  res=callback2_exn(Field(p->closure, 1), x_barr, j_barr);
  if(Is_exception_result(res))
    return GSL_EFAILED;
  gsl_matrix_memcpy(J, &j_v.matrix);
  return GSL_SUCCESS;
}

int gsl_multiroot_callback_fdf(const gsl_vector *x, void *params, 
			   gsl_vector *F, gsl_matrix *J)
{
  int barr_flags = BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT | BIGARRAY_EXTERNAL;
  struct callback_params *p=params;
  value x_barr, f_barr, j_barr;
  int len = x->size;
  LOCALARRAY(double, x_arr, len); 
  LOCALARRAY(double, f_arr, len); 
  LOCALARRAY(double, j_arr, len*len); 
  gsl_vector_view x_v, f_v;
  gsl_matrix_view j_v;
  value res;
  
  x_v = gsl_vector_view_array(x_arr, len);
  f_v = gsl_vector_view_array(f_arr, len);
  j_v = gsl_matrix_view_array(j_arr, len, len);
  gsl_vector_memcpy(&x_v.vector, x);
  x_barr = alloc_bigarray_dims(barr_flags, 1, x_arr, len);
  f_barr = alloc_bigarray_dims(barr_flags, 1, f_arr, len);
  j_barr = alloc_bigarray_dims(barr_flags, 2, j_arr, len, len);
  res=callback3_exn(Field(p->closure, 2), x_barr, f_barr, j_barr);
  if(Is_exception_result(res))
    return GSL_EFAILED;
  gsl_vector_memcpy(F, &f_v.vector);
  gsl_matrix_memcpy(J, &j_v.matrix);
  return GSL_SUCCESS;
}



/* MULTIMIN CALLBACKS */
double gsl_multimin_callback(const gsl_vector *x, void *params)
{
  int barr_flags = BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT | BIGARRAY_EXTERNAL;
  struct callback_params *p=params;
  value x_barr;
  int len = x->size;
  LOCALARRAY(double, x_arr, len); 
  gsl_vector_view x_v;
  value res;

  x_v = gsl_vector_view_array(x_arr, len);
  gsl_vector_memcpy(&x_v.vector, x);
  x_barr = alloc_bigarray_dims(barr_flags, 1, x_arr, len);
  res=callback_exn(p->closure, x_barr);
  if(Is_exception_result(res))
    return GSL_NAN;
  return Double_val(res);
}

double gsl_multimin_callback_f(const gsl_vector *x, void *params)
{
  int barr_flags = BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT | BIGARRAY_EXTERNAL;
  struct callback_params *p=params;
  value x_barr;
  int len = x->size;
  LOCALARRAY(double, x_arr, len); 
  gsl_vector_view x_v;
  value res;

  x_v = gsl_vector_view_array(x_arr, len);
  gsl_vector_memcpy(&x_v.vector, x);
  x_barr = alloc_bigarray_dims(barr_flags, 1, x_arr, len);
  res=callback_exn(Field(p->closure, 0), x_barr);
  if(Is_exception_result(res))
    return GSL_NAN;
  return Double_val(res);
}

void gsl_multimin_callback_df(const gsl_vector *x, void *params, gsl_vector *G)
{
  int barr_flags = BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT | BIGARRAY_EXTERNAL;
  struct callback_params *p=params;
  value x_barr, g_barr;
  int len = x->size;
  LOCALARRAY(double, x_arr, len); 
  LOCALARRAY(double, g_arr, len); 
  gsl_vector_view x_v, g_v;
  value res;

  x_v = gsl_vector_view_array(x_arr, len);
  g_v = gsl_vector_view_array(g_arr, len);
  gsl_vector_memcpy(&x_v.vector, x);
  x_barr = alloc_bigarray_dims(barr_flags, 1, x_arr, len);
  g_barr = alloc_bigarray_dims(barr_flags, 1, g_arr, len);
  res=callback2_exn(Field(p->closure, 1), x_barr, g_barr);
  if(Is_exception_result(res)){
    fprintf(stderr, "MLGSL: OCaml callback raised an exception\n");
    fflush(stderr);
    abort();
  }
  gsl_vector_memcpy(G, &g_v.vector);
}

void gsl_multimin_callback_fdf(const gsl_vector *x, void *params, 
			       double *f, gsl_vector *G)
{
  int barr_flags = BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT | BIGARRAY_EXTERNAL;
  struct callback_params *p=params;
  value x_barr, g_barr;
  int len = x->size;
  LOCALARRAY(double, x_arr, len);
  LOCALARRAY(double, g_arr, len); 
  gsl_vector_view x_v, g_v;
  value res;
  
  x_v = gsl_vector_view_array(x_arr, len);
  g_v = gsl_vector_view_array(g_arr, len);
  gsl_vector_memcpy(&x_v.vector, x);
  x_barr = alloc_bigarray_dims(barr_flags, 1, x_arr, len);
  g_barr = alloc_bigarray_dims(barr_flags, 1, g_arr, len);
  res=callback2_exn(Field(p->closure, 2), x_barr, g_barr);
  if(Is_exception_result(res)){
    *f=GSL_NAN;
    return;
  }
  gsl_vector_memcpy(G, &g_v.vector);
  *f=Double_val(res);
}



/* MULTIFIT CALLBACKS */
int gsl_multifit_callback_f(const gsl_vector *X, void *params, gsl_vector *F)
{
  int barr_flags = BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT | BIGARRAY_EXTERNAL;
  struct callback_params *parms=params;
  value x_barr, f_barr;
  size_t p = X->size;
  size_t n = F->size;
  LOCALARRAY(double, x_arr, p); 
  LOCALARRAY(double, f_arr, n); 
  gsl_vector_view x_v, f_v;
  value res;

  x_v = gsl_vector_view_array(x_arr, p);
  f_v = gsl_vector_view_array(f_arr, n);
  gsl_vector_memcpy(&x_v.vector, X);
  x_barr = alloc_bigarray_dims(barr_flags, 1, x_arr, p);
  f_barr = alloc_bigarray_dims(barr_flags, 1, f_arr, n);
  res=callback2_exn(Field(parms->closure, 0), x_barr, f_barr);
  if(Is_exception_result(res))
    return GSL_EFAILED;
  gsl_vector_memcpy(F, &f_v.vector);
  return GSL_SUCCESS;
}

int gsl_multifit_callback_df(const gsl_vector *X, void *params, gsl_matrix *J)
{
  int barr_flags = BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT | BIGARRAY_EXTERNAL;
  struct callback_params *parms=params;
  value x_barr, j_barr;
  size_t p = X->size;
  size_t n = J->size1;
  LOCALARRAY(double, x_arr, p); 
  LOCALARRAY(double, j_arr, n*p); 
  gsl_vector_view x_v;
  gsl_matrix_view j_v;
  value res;

  x_v = gsl_vector_view_array(x_arr, p);
  j_v = gsl_matrix_view_array(j_arr, n, p);
  gsl_vector_memcpy(&x_v.vector, X);
  x_barr = alloc_bigarray_dims(barr_flags, 1, x_arr, p);
  j_barr = alloc_bigarray_dims(barr_flags, 2, j_arr, n, p);
  res=callback2_exn(Field(parms->closure, 1), x_barr, j_barr);
  if(Is_exception_result(res))
    return GSL_EFAILED;
  gsl_matrix_memcpy(J, &j_v.matrix);
  return GSL_SUCCESS;
}

int gsl_multifit_callback_fdf(const gsl_vector *X, void *params, 
			      gsl_vector *F, gsl_matrix *J)
{
  int barr_flags = BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT | BIGARRAY_EXTERNAL;
  struct callback_params *parms=params;
  value x_barr, f_barr, j_barr;
  size_t p = X->size;
  size_t n = F->size;
  LOCALARRAY(double, x_arr, p); 
  LOCALARRAY(double, f_arr, n); 
  LOCALARRAY(double, j_arr, n*p); 
  gsl_vector_view x_v, f_v;
  gsl_matrix_view j_v;
  value res;
  
  x_v = gsl_vector_view_array(x_arr, p);
  f_v = gsl_vector_view_array(f_arr, n);
  j_v = gsl_matrix_view_array(j_arr, n, p);
  gsl_vector_memcpy(&x_v.vector, X);
  x_barr = alloc_bigarray_dims(barr_flags, 1, x_arr, p);
  f_barr = alloc_bigarray_dims(barr_flags, 1, f_arr, n);
  j_barr = alloc_bigarray_dims(barr_flags, 2, j_arr, n, p);
  res=callback3_exn(Field(parms->closure, 2), x_barr, f_barr, j_barr);
  if(Is_exception_result(res))
    return GSL_EFAILED;
  gsl_vector_memcpy(F, &f_v.vector);
  gsl_matrix_memcpy(J, &j_v.matrix);
  return GSL_SUCCESS;
}
