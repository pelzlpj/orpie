/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002 - Olivier Andrieu                     */
/* distributed under the terms of the GPL version 2         */


#include <gsl/gsl_vector.h>
#include <caml/bigarray.h>

#include "wrappers.h"

#ifndef TYPE
#error pb with include files
#endif

static inline void TYPE(mlgsl_vec_of_bigarray)(TYPE(gsl_vector) *cvec, 
					       value vvec){
  struct caml_bigarray *bigarr = Bigarray_val(vvec);
  cvec->block = NULL;
  cvec->owner = 0;
  cvec->size   = bigarr->dim[0];
  cvec->stride = 1;
  cvec->data   = bigarr->data;
}

#ifdef CONV_FLAT
static inline void TYPE(mlgsl_vec_of_floatarray)(TYPE(gsl_vector) *cvec, 
						 value vvec){
  cvec->block = NULL;
  cvec->owner = 0;
  cvec->size   = Int_val(Field(vvec, 2));
  cvec->stride = Int_val(Field(vvec, 3));
  cvec->data   = (double *)Field(vvec, 0) + Int_val(Field(vvec, 1));
}
#endif

static inline void TYPE(mlgsl_vec_of_value)(TYPE(gsl_vector) *cvec, 
					    value vvec){
  if(Tag_val(vvec) == 0 && Wosize_val(vvec) == 2)
    /* value is a polymorphic variant */
    vvec = Field(vvec, 1);
  if(Tag_val(vvec) == Custom_tag)
    /* value is a bigarray */
    TYPE(mlgsl_vec_of_bigarray)(cvec, vvec);
#ifdef CONV_FLAT
  else 
    /* value is a record wrapping a float array */
    TYPE(mlgsl_vec_of_floatarray)(cvec, vvec);
#endif
}

#define _DECLARE_VECTOR(a) TYPE(gsl_vector) v_##a
#define _DECLARE_VECTOR2(a,b) _DECLARE_VECTOR(a); _DECLARE_VECTOR(b)
#define _DECLARE_VECTOR3(a,b,c) _DECLARE_VECTOR2(a,b); _DECLARE_VECTOR(c)
#define _DECLARE_VECTOR4(a,b,c,d) _DECLARE_VECTOR2(a,b); _DECLARE_VECTOR2(c,d)
#define _DECLARE_VECTOR5(a,b,c,d,e) _DECLARE_VECTOR4(a,b,c,d); _DECLARE_VECTOR(e)

#define _CONVERT_VECTOR(a) TYPE(mlgsl_vec_of_value)(&v_##a, a)
#define _CONVERT_VECTOR2(a,b) _CONVERT_VECTOR(a); _CONVERT_VECTOR(b)
#define _CONVERT_VECTOR3(a,b,c) _CONVERT_VECTOR2(a,b); _CONVERT_VECTOR(c)
#define _CONVERT_VECTOR4(a,b,c,d) _CONVERT_VECTOR2(a,b); _CONVERT_VECTOR2(c,d)
#define _CONVERT_VECTOR5(a,b,c,d,e) _CONVERT_VECTOR4(a,b,c,d); _CONVERT_VECTOR(e)


#if __GNUC__ >= 3
#define DECLARE_VECTOR(a) _DECLARE_VECTOR(a); _CONVERT_VECTOR(a)
#define DECLARE_VECTOR2(a,b) DECLARE_VECTOR(a); DECLARE_VECTOR(b)
#define DECLARE_VECTOR3(a,b,c) DECLARE_VECTOR2(a,b); DECLARE_VECTOR(c)
#define DECLARE_VECTOR4(a,b,c,d) DECLARE_VECTOR2(a,b); DECLARE_VECTOR2(c,d)
#define DECLARE_VECTOR5(a,b,c,d,e) DECLARE_VECTOR4(a,b,c,d); DECLARE_VECTOR(e)
#endif /* __GNUC__ */
