/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002 - Olivier Andrieu                     */
/* distributed under the terms of the GPL version 2         */


#include <gsl/gsl_linalg.h>

#include "mlgsl_matrix_complex.h"
#include "mlgsl_vector_complex.h"
#include "mlgsl_permut.h"
#include "mlgsl_complex.h"


/* Complex LU decomposition */

value ml_gsl_linalg_complex_LU_decomp(value A, value P)
{
  int sign;
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX(A);
  _CONVERT_MATRIX(A);
  gsl_linalg_complex_LU_decomp(&m_A, &perm_P, &sign);
  return Val_int(sign);
}

value ml_gsl_linalg_complex_LU_solve(value LU, value P, value B, value X)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX(LU);
  _DECLARE_VECTOR2(B,X);
  _CONVERT_MATRIX(LU);
  _CONVERT_VECTOR2(B,X);
  gsl_linalg_complex_LU_solve(&m_LU, &perm_P, &v_B, &v_X);
  return Val_unit;
}

value ml_gsl_linalg_complex_LU_svx(value LU, value P, value X)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX(LU);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(LU);
  _CONVERT_VECTOR(X);
  gsl_linalg_complex_LU_svx(&m_LU, &perm_P, &v_X);
  return Val_unit;
}

value ml_gsl_linalg_complex_LU_refine(value A, value LU, value P, 
				      value B, value X, value RES)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX2(A, LU);
  _DECLARE_VECTOR3(B, X, RES);
  _CONVERT_MATRIX2(A, LU);
  _CONVERT_VECTOR3(B, X, RES);
  gsl_linalg_complex_LU_refine(&m_A, &m_LU, &perm_P, &v_B, &v_X, &v_RES);
  return Val_unit;
}

value ml_gsl_linalg_complex_LU_refine_bc(value *argv, int argc)
{
  return ml_gsl_linalg_complex_LU_refine(argv[0], argv[1], argv[2],
					 argv[3], argv[4], argv[5]);
}

value ml_gsl_linalg_complex_LU_invert(value LU, value P, value INV)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX2(LU, INV);
  _CONVERT_MATRIX2(LU, INV);
  gsl_linalg_complex_LU_invert(&m_LU, &perm_P, &m_INV);
  return Val_unit;
}

value ml_gsl_linalg_complex_LU_det(value LU, value sig)
{
  gsl_complex z;
  _DECLARE_MATRIX(LU);
  _CONVERT_MATRIX(LU);
  z = gsl_linalg_complex_LU_det(&m_LU, Int_val(sig));
  return copy_complex(&z);
}

value ml_gsl_linalg_complex_LU_lndet(value LU)
{
  _DECLARE_MATRIX(LU);
  _CONVERT_MATRIX(LU);
  return copy_double(gsl_linalg_complex_LU_lndet(&m_LU));
}

value ml_gsl_linalg_complex_LU_sgndet(value LU, value sig)
{
  gsl_complex z;
  _DECLARE_MATRIX(LU);
  _CONVERT_MATRIX(LU);
  z = gsl_linalg_complex_LU_sgndet(&m_LU, Int_val(sig)) ;
  return copy_complex(&z);
}


/* Hermitian to symmetric tridiagonal decomposition */

/* Those are tricky 'coz they mix real & complex matrices ... */

#undef BASE_TYPE
#undef TYPE
#undef _DECLARE_BASE_TYPE
#undef _CONVERT_BASE_TYPE
#undef DECLARE_BASE_TYPE
#undef FUNCTION

#include "mlgsl_matrix_double.h"
#include "mlgsl_vector_double.h"

value ml_gsl_linalg_hermtd_decomp (value A, value tau)
{
  _DECLARE_COMPLEX_MATRIX(A);
  _DECLARE_COMPLEX_VECTOR(tau);
  _CONVERT_COMPLEX_MATRIX(A);
  _CONVERT_COMPLEX_VECTOR(tau);
  gsl_linalg_hermtd_decomp(&m_A, &v_tau);
  return Val_unit;
}

value ml_gsl_linalg_hermtd_unpack (value A, value tau, value Q, 
				   value diag, value subdiag)
{
  _DECLARE_COMPLEX_VECTOR(tau);
  _DECLARE_VECTOR2(diag,subdiag);
  _DECLARE_COMPLEX_MATRIX2(A,Q);
  _CONVERT_COMPLEX_VECTOR(tau);
  _CONVERT_VECTOR2(diag,subdiag);
  _CONVERT_COMPLEX_MATRIX2(A,Q);
  gsl_linalg_hermtd_unpack(&m_A, &v_tau, &m_Q, &v_diag, &v_subdiag);
  return Val_unit;
}

value ml_gsl_linalg_hermtd_unpack_T (value A, value diag, value subdiag)
{
  _DECLARE_COMPLEX_MATRIX(A);
  _DECLARE_VECTOR2(diag,subdiag);
  _CONVERT_COMPLEX_MATRIX(A);
  _CONVERT_VECTOR2(diag,subdiag);
  gsl_linalg_hermtd_unpack_T(&m_A, &v_diag, &v_subdiag);
  return Val_unit;
}
