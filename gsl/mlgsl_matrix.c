/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002-2005 - Olivier Andrieu                */
/* distributed under the terms of the GPL version 2         */


#ifndef FUNCTION
#error pb with include files
#endif

CAMLprim value FUNCTION(ml_gsl_matrix,memcpy)(value A, value B)
{
  _DECLARE_MATRIX2(A,B);
  _CONVERT_MATRIX2(A,B);
  FUNCTION(gsl_matrix,memcpy)(&m_B, &m_A);
  return Val_unit;
}

CAMLprim value FUNCTION(ml_gsl_matrix,add)(value A, value B)
{
  _DECLARE_MATRIX2(A,B);
  _CONVERT_MATRIX2(A,B);
  FUNCTION(gsl_matrix,add)(&m_A, &m_B);
  return Val_unit;
}

CAMLprim value FUNCTION(ml_gsl_matrix,sub)(value A, value B)
{
  _DECLARE_MATRIX2(A,B);
  _CONVERT_MATRIX2(A,B);
  FUNCTION(gsl_matrix,sub)(&m_A, &m_B);
  return Val_unit;
}

CAMLprim value FUNCTION(ml_gsl_matrix,mul)(value A, value B)
{
  _DECLARE_MATRIX2(A,B);
  _CONVERT_MATRIX2(A,B);
  FUNCTION(gsl_matrix,mul_elements)(&m_A, &m_B);
  return Val_unit;
}

CAMLprim value FUNCTION(ml_gsl_matrix,div)(value A, value B)
{
  _DECLARE_MATRIX2(A,B);
  _CONVERT_MATRIX2(A,B);
  FUNCTION(gsl_matrix,div_elements)(&m_A, &m_B);
  return Val_unit;
}

CAMLprim value FUNCTION(ml_gsl_matrix,scale)(value A, value X)
{
  _DECLARE_MATRIX(A);
  _DECLARE_BASE_TYPE(X);
  _CONVERT_MATRIX(A);
  _CONVERT_BASE_TYPE(X);
  FUNCTION(gsl_matrix,scale)(&m_A, conv_X);
  return Val_unit;
}

CAMLprim value FUNCTION(ml_gsl_matrix,add_constant)(value A, value X)
{
  _DECLARE_MATRIX(A);
  _DECLARE_BASE_TYPE(X);
  _CONVERT_MATRIX(A);
  _CONVERT_BASE_TYPE(X);
  FUNCTION(gsl_matrix,add_constant)(&m_A, conv_X);
  return Val_unit;
}

CAMLprim value FUNCTION(ml_gsl_matrix,add_diagonal)(value A, value X)
{
  _DECLARE_MATRIX(A);
  _DECLARE_BASE_TYPE(X);
  _CONVERT_MATRIX(A);
  _CONVERT_BASE_TYPE(X);
  FUNCTION(gsl_matrix,add_diagonal)(&m_A, conv_X);
  return Val_unit;
}

CAMLprim value FUNCTION(ml_gsl_matrix,isnull)(value A)
{
  int r;
  _DECLARE_MATRIX(A);
  _CONVERT_MATRIX(A);
  r = FUNCTION(gsl_matrix,isnull)(&m_A);
  return Val_bool(r);
}

CAMLprim value FUNCTION(ml_gsl_matrix,swap_rows)(value A, value i, value j)
{
  _DECLARE_MATRIX(A);
  _CONVERT_MATRIX(A);
  FUNCTION(gsl_matrix,swap_rows)(&m_A, Int_val(i), Int_val(j));
  return Val_unit;
}

CAMLprim value FUNCTION(ml_gsl_matrix,swap_columns)(value A, value i, value j)
{
  _DECLARE_MATRIX(A);
  _CONVERT_MATRIX(A);
  FUNCTION(gsl_matrix,swap_columns)(&m_A, Int_val(i), Int_val(j));
  return Val_unit;
}

CAMLprim value FUNCTION(ml_gsl_matrix,swap_rowcol)(value A, value i, value j)
{
  _DECLARE_MATRIX(A);
  _CONVERT_MATRIX(A);
  FUNCTION(gsl_matrix,swap_rowcol)(&m_A, Int_val(i), Int_val(j));
  return Val_unit;
}

CAMLprim value FUNCTION(ml_gsl_matrix,transpose_memcpy)(value A, value B)
{
  _DECLARE_MATRIX2(A, B);
  _CONVERT_MATRIX2(A, B);
  FUNCTION(gsl_matrix,transpose_memcpy)(&m_A, &m_B);
  return Val_unit;
}

CAMLprim value FUNCTION(ml_gsl_matrix,transpose)(value A)
{
  _DECLARE_MATRIX(A);
  _CONVERT_MATRIX(A);
  FUNCTION(gsl_matrix,transpose)(&m_A);
  return Val_unit;
}
