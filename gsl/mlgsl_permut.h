/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002 - Olivier Andrieu                     */
/* distributed under the terms of the GPL version 2         */


#include <caml/bigarray.h>

#define GSL_PERMUT_OF_BIGARRAY(arr) \
  struct caml_bigarray *bigarr_##arr = Bigarray_val(arr); \
  gsl_permutation perm_##arr = { \
      /*.size =*/ bigarr_##arr->dim[0], \
      /*.data =*/ bigarr_##arr->data }

