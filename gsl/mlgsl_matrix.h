/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002 - Olivier Andrieu                     */
/* distributed under the terms of the GPL version 2         */


#include <gsl/gsl_matrix.h>
#include <caml/bigarray.h>

#include "wrappers.h"

#ifndef TYPE
#error pb with include files
#endif

static inline void TYPE(mlgsl_mat_of_bigarray)(TYPE(gsl_matrix) *cmat, 
					       value vmat){
  struct caml_bigarray *bigarr = Bigarray_val(vmat);
  cmat->block = NULL;
  cmat->owner = 0;
  cmat->size1 = bigarr->dim[0];
  cmat->size2 = bigarr->dim[1];
  cmat->tda   = bigarr->dim[1];
  cmat->data  = bigarr->data; 
}

#ifdef CONV_FLAT
static inline void TYPE(mlgsl_mat_of_floatarray)(TYPE(gsl_matrix) *cmat, 
						 value vmat){
  cmat->block = NULL;
  cmat->owner = 0;
  cmat->size1 = Int_val(Field(vmat, 2)); 
  cmat->size2 = Int_val(Field(vmat, 3)); 
  cmat->tda   = Int_val(Field(vmat, 4)); 
  cmat->data  = (double *) Field(vmat, 0) + Int_val(Field(vmat, 1));
}
#endif

static inline void TYPE(mlgsl_mat_of_value)(TYPE(gsl_matrix) *cmat, 
					    value vmat){
  if(Tag_val(vmat) == 0 && Wosize_val(vmat) == 2)
    /* value is a polymorphic variant */
    vmat = Field(vmat, 1);
  if(Tag_val(vmat) == Custom_tag)
    /* value is a bigarray */
    TYPE(mlgsl_mat_of_bigarray)(cmat, vmat);
#ifdef CONV_FLAT
  else 
    /* value is a record wrapping a float array */
    TYPE(mlgsl_mat_of_floatarray)(cmat, vmat);
#endif
}

#define _DECLARE_MATRIX(a) TYPE(gsl_matrix) m_##a
#define _DECLARE_MATRIX2(a,b) _DECLARE_MATRIX(a); _DECLARE_MATRIX(b)
#define _DECLARE_MATRIX3(a,b,c) _DECLARE_MATRIX2(a,b); _DECLARE_MATRIX(c)

#define _CONVERT_MATRIX(a) TYPE(mlgsl_mat_of_value)(&m_##a, a)
#define _CONVERT_MATRIX2(a,b) _CONVERT_MATRIX(a); _CONVERT_MATRIX(b)
#define _CONVERT_MATRIX3(a,b,c) _CONVERT_MATRIX2(a,b); _CONVERT_MATRIX(c)

#if __GNUC__ >= 3
#define DECLARE_MATRIX(a) _DECLARE_MATRIX(a); _CONVERT_MATRIX(a)
#define DECLARE_MATRIX2(a,b) DECLARE_MATRIX(a); DECLARE_MATRIX(b)
#define DECLARE_MATRIX3(a,b,c) DECLARE_MATRIX2(a,b); DECLARE_MATRIX(c)
#endif /* __GNUC__ */
