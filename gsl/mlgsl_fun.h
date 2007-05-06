/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002-2005 - Olivier Andrieu                */
/* distributed under the terms of the GPL version 2         */

#include <gsl/gsl_math.h>
#include <gsl/gsl_monte.h>
#include <gsl/gsl_multiroots.h>
#include <gsl/gsl_multimin.h>
#include <gsl/gsl_multifit_nlin.h>

#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>

struct callback_params {
  value closure ;  /* the closure(s) for the caml callback */
  value dbl;       /* a preallocated caml float array for monte callbacks */
  union {
    gsl_function gf;
    gsl_function_fdf gfdf;
    gsl_monte_function mf;
    gsl_multiroot_function mrf;
    gsl_multiroot_function_fdf mrfdf;
    gsl_multimin_function mmf;
    gsl_multimin_function_fdf mmfdf;
    gsl_multifit_function_fdf mffdf;
  } gslfun ;
};


extern double gslfun_callback(double, void *);
extern double gslfun_callback_indir(double, void *);

extern double gslfun_callback_f(double, void *);
extern double gslfun_callback_df(double, void *);
extern void   gslfun_callback_fdf(double, void *, double *, double*);

extern double gsl_monte_callback(double *, size_t , void *);
extern double gsl_monte_callback_fast(double *, size_t , void *);

extern int gsl_multiroot_callback(const gsl_vector *, void *, gsl_vector *);
extern int gsl_multiroot_callback_f(const gsl_vector *, void *, gsl_vector *);
extern int gsl_multiroot_callback_df(const gsl_vector *, void *, gsl_matrix *);
extern int gsl_multiroot_callback_fdf(const gsl_vector *, void *, 
				      gsl_vector *, gsl_matrix *);

extern double gsl_multimin_callback(const gsl_vector *, void *);
extern double gsl_multimin_callback_f(const gsl_vector *, void *);
extern void gsl_multimin_callback_df(const gsl_vector *, void *, gsl_vector *);
extern void gsl_multimin_callback_fdf(const gsl_vector *, void *, 
				      double *, gsl_vector *);

extern int gsl_multifit_callback_f(const gsl_vector *, void *, gsl_vector *);
extern int gsl_multifit_callback_df(const gsl_vector *, void *, gsl_matrix *);
extern int gsl_multifit_callback_fdf(const gsl_vector *, void *, 
				     gsl_vector *, gsl_matrix *);

#define GSLFUN_CLOSURE(gf,v) \
  gsl_function gf = { \
  /*.function =*/ &gslfun_callback_indir, \
  /*.params   =*/ &v }
