/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002 - Olivier Andrieu                     */
/* distributed under the terms of the GPL version 2         */


#include <gsl/gsl_permutation.h>
#include <gsl/gsl_permute.h>

#include "wrappers.h"
#include "mlgsl_error.h"
#include "mlgsl_permut.h"

value ml_gsl_permutation_init(value p)
{
  GSL_PERMUT_OF_BIGARRAY(p);
  gsl_permutation_init(&perm_p);
  return Val_unit;
}

value ml_gsl_permutation_valid(value p)
{
  int r;
  GSL_PERMUT_OF_BIGARRAY(p);
  r = gsl_permutation_valid(&perm_p);
  return Val_negbool(r);
}

value ml_gsl_permutation_reverse(value p)
{
  GSL_PERMUT_OF_BIGARRAY(p);
  gsl_permutation_reverse(&perm_p);
  return Val_unit;
}

value ml_gsl_permutation_inverse(value src, value dst)
{
  GSL_PERMUT_OF_BIGARRAY(src);
  GSL_PERMUT_OF_BIGARRAY(dst);
  gsl_permutation_inverse(&perm_dst, &perm_src);
  return Val_unit;
}

value ml_gsl_permutation_next(value p)
{
  GSL_PERMUT_OF_BIGARRAY(p);
  gsl_permutation_next(&perm_p);
  return Val_unit;
}

value ml_gsl_permutation_prev(value p)
{
  GSL_PERMUT_OF_BIGARRAY(p);
  gsl_permutation_prev(&perm_p);
  return Val_unit;
}

value ml_gsl_permute(value p, value arr)
{
  GSL_PERMUT_OF_BIGARRAY(p);
  if(Tag_val(arr) == Double_array_tag)
    gsl_permute(perm_p.data, Double_array_val(arr), 1,
		Double_array_length(arr));
  else
    gsl_permute_long(perm_p.data, (value *)arr, 1, Array_length(arr));
  return Val_unit;
}

value ml_gsl_permute_barr(value p, value arr)
{
  GSL_PERMUT_OF_BIGARRAY(p);
  struct caml_bigarray *barr = Bigarray_val(arr);
  enum caml_bigarray_kind kind = (barr->flags) & BIGARRAY_KIND_MASK ;
  switch(kind){
  case BIGARRAY_FLOAT32:
    gsl_permute_float(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_FLOAT64:
    gsl_permute(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_SINT8:
    gsl_permute_char(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_UINT8:
    gsl_permute_uchar(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_SINT16:
    gsl_permute_short(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_UINT16:
    gsl_permute_ushort(perm_p.data, barr->data, 1, barr->dim[0]); break;
#ifdef ARCH_SIXTYFOUR
  case BIGARRAY_INT64:
#else
  case BIGARRAY_INT32:
#endif
  case BIGARRAY_CAML_INT:
  case BIGARRAY_NATIVE_INT:
    gsl_permute_long(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_COMPLEX32:
    gsl_permute_complex_float(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_COMPLEX64:
    gsl_permute_complex(perm_p.data, barr->data, 1, barr->dim[0]); break;
  default: 
    MLGSLexn("data type not supported", GSL_EUNIMPL);
  }
  return Val_unit;
}

value ml_gsl_permute_complex(value p, value arr)
{
  GSL_PERMUT_OF_BIGARRAY(p);
  gsl_permute_complex(perm_p.data, Double_array_val(arr), 1, 
		      Double_array_length(arr)/2);
  return Val_unit;
}

value ml_gsl_permute_inverse(value p, value arr)
{
  GSL_PERMUT_OF_BIGARRAY(p);
  if(Tag_val(arr) == Double_array_tag)
    gsl_permute_inverse(perm_p.data, Double_array_val(arr), 1,
			Double_array_length(arr));
  else
    gsl_permute_long_inverse(perm_p.data, (value *)arr, 1, Array_length(arr));
  return Val_unit;
}

value ml_gsl_permute_inverse_barr(value p, value arr)
{
  GSL_PERMUT_OF_BIGARRAY(p);
  struct caml_bigarray *barr = Bigarray_val(arr);
  enum caml_bigarray_kind kind = (barr->flags) & BIGARRAY_KIND_MASK ;
  switch(kind){
  case BIGARRAY_FLOAT32:
    gsl_permute_float_inverse(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_FLOAT64:
    gsl_permute_inverse(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_SINT8:
    gsl_permute_char_inverse(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_UINT8:
    gsl_permute_uchar_inverse(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_SINT16:
    gsl_permute_short_inverse(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_UINT16:
    gsl_permute_ushort_inverse(perm_p.data, barr->data, 1, barr->dim[0]); break;
#ifdef ARCH_SIXTYFOUR
  case BIGARRAY_INT64:
#else
  case BIGARRAY_INT32:
#endif
  case BIGARRAY_CAML_INT:
  case BIGARRAY_NATIVE_INT:
    gsl_permute_long_inverse(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_COMPLEX32:
    gsl_permute_complex_float_inverse(perm_p.data, barr->data, 1, barr->dim[0]); break;
  case BIGARRAY_COMPLEX64:
    gsl_permute_complex_inverse(perm_p.data, barr->data, 1, barr->dim[0]); break;
  default:
    MLGSLexn("data type not supported", GSL_EUNIMPL);
  }
  return Val_unit;
}

value ml_gsl_permute_inverse_complex(value p, value arr)
{
  GSL_PERMUT_OF_BIGARRAY(p);
  gsl_permute_complex_inverse(perm_p.data, Double_array_val(arr), 1, 
			      Double_array_length(arr)/2);
  return Val_unit;
}
