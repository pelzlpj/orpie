/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002 - Olivier Andrieu                     */
/* distributed under the terms of the GPL version 2         */


#include <gsl/gsl_blas.h>

#include "mlgsl_vector_double.h"
#include "mlgsl_matrix_double.h"

#include "mlgsl_blas.h"


/* LEVEL1 double */

value ml_gsl_blas_ddot(value X, value Y)
{
  double r;
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_ddot(&v_X, &v_Y, &r);
  return copy_double(r);
}

value ml_gsl_blas_dnrm2(value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  return copy_double(gsl_blas_dnrm2(&v_X));
}

value ml_gsl_blas_dasum(value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  return copy_double(gsl_blas_dasum(&v_X));
}

value ml_gsl_blas_idamax(value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  return Val_int(gsl_blas_idamax(&v_X));
}

value ml_gsl_blas_dswap(value X, value Y)
{
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_dswap(&v_X, &v_Y);
  return Val_unit;
}

value ml_gsl_blas_dcopy(value X, value Y)
{
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_dcopy(&v_X, &v_Y);
  return Val_unit;
}

value ml_gsl_blas_daxpy(value alpha, value X, value Y)
{
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_daxpy(Double_val(alpha), &v_X, &v_Y);
  return Val_unit;
}

/* FIXME: drotg drotmg drotm */

value ml_gsl_blas_drot(value X, value Y, value c, value s)
{
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_drot(&v_X, &v_Y, Double_val(c), Double_val(s));
  return Val_unit;
}

value ml_gsl_blas_dscal(value alpha, value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  gsl_blas_dscal(Double_val(alpha), &v_X);
  return Val_unit;
}


/* LEVEL2 double */

value ml_gsl_blas_dgemv(value transa, value alpha, value A, 
			value X, value beta, value Y)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_dgemv(CBLAS_TRANS_val(transa), Double_val(alpha),
		 &m_A, &v_X, Double_val(beta), &v_Y);
  return Val_unit;
}

value ml_gsl_blas_dgemv_bc(value *argv, int argc)
{
  return ml_gsl_blas_dgemv(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}

value ml_gsl_blas_dtrmv(value uplo, value transa, value diag,
			value A, value X)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(X);
  gsl_blas_dtrmv(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(transa),
		 CBLAS_DIAG_val(diag), &m_A, &v_X);
  return Val_unit;
}

value ml_gsl_blas_dtrsv(value uplo, value transa, value diag,
			value A, value X)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(X);
  gsl_blas_dtrsv(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(transa),
		 CBLAS_DIAG_val(diag), &m_A, &v_X);
  return Val_unit;
}

value ml_gsl_blas_dsymv(value uplo, value alpha, value A, 
			value X, value beta, value Y)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_dsymv(CBLAS_UPLO_val(uplo), Double_val(alpha),
		 &m_A, &v_X, Double_val(beta), &v_Y);
  return Val_unit;
}

value ml_gsl_blas_dsymv_bc(value *argv, int argc)
{
  return ml_gsl_blas_dsymv(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}

value ml_gsl_blas_dger(value alpha, value X, value Y, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_dger(Double_val(alpha), &v_X, &v_Y, &m_A);
  return Val_unit;
}

value ml_gsl_blas_dsyr(value uplo ,value alpha, value X, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(X);
  gsl_blas_dsyr(CBLAS_UPLO_val(uplo), Double_val(alpha), 
		&v_X, &m_A);
  return Val_unit;
}

value ml_gsl_blas_dsyr2(value uplo ,value alpha, value X, value Y, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_dsyr2(CBLAS_UPLO_val(uplo), Double_val(alpha), 
		&v_X, &v_Y, &m_A);
  return Val_unit;
}



/* LEVEL3 double */

value ml_gsl_blas_dgemm(value transa, value transb, 
			value alpha, value A, value B, 
			value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _CONVERT_MATRIX3(A, B, C);
  gsl_blas_dgemm(CBLAS_TRANS_val(transa), CBLAS_TRANS_val(transb),
		 Double_val(alpha), &m_A, &m_B, Double_val(beta), &m_C);
  return Val_unit;
}

value ml_gsl_blas_dgemm_bc(value *argv, int argc)
{
  return ml_gsl_blas_dgemm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}


value ml_gsl_blas_dsymm(value side, value uplo,
			value alpha, value A, value B, 
			value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _CONVERT_MATRIX3(A, B, C);
  gsl_blas_dsymm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 Double_val(alpha), &m_A, &m_B, Double_val(beta), &m_C);
  return Val_unit;
}

value ml_gsl_blas_dsymm_bc(value *argv, int argc)
{
  return ml_gsl_blas_dsymm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

value ml_gsl_blas_dtrmm(value side, value uplo,
			value transa, value diag,
			value alpha, value A, value B)
{
  _DECLARE_MATRIX2(A, B);
  _CONVERT_MATRIX2(A, B);
  gsl_blas_dtrmm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 CBLAS_TRANS_val(transa), CBLAS_DIAG_val(diag),
		 Double_val(alpha), &m_A, &m_B);
  return Val_unit;
}

value ml_gsl_blas_dtrmm_bc(value *argv, int argc)
{
  return ml_gsl_blas_dtrmm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

value ml_gsl_blas_dtrsm(value side, value uplo,
			value transa, value diag,
			value alpha, value A, value B)
{
  _DECLARE_MATRIX2(A, B);
  _CONVERT_MATRIX2(A, B);
  gsl_blas_dtrsm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 CBLAS_TRANS_val(transa), CBLAS_DIAG_val(diag),
		 Double_val(alpha), &m_A, &m_B);
  return Val_unit;
}

value ml_gsl_blas_dtrsm_bc(value *argv, int argc)
{
  return ml_gsl_blas_dtrsm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

value ml_gsl_blas_dsyrk(value uplo, value trans, value alpha, 
			value A, value beta, value C)
{
  _DECLARE_MATRIX2(A, C);
  _CONVERT_MATRIX2(A, C);
  gsl_blas_dsyrk(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(trans), 
		 Double_val(alpha), &m_A,
		 Double_val(beta), &m_C);
  return Val_unit;
}

value ml_gsl_blas_dsyrk_bc(value *argv, int argc)
{
  return ml_gsl_blas_dsyrk(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}


value ml_gsl_blas_dsyr2k(value uplo, value trans, value alpha, 
			 value A, value B, value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _CONVERT_MATRIX3(A, B, C);
  gsl_blas_dsyr2k(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(trans), 
		  Double_val(alpha), &m_A, &m_B,
		  Double_val(beta), &m_C);
  return Val_unit;
}

value ml_gsl_blas_dsyr2k_bc(value *argv, int argc)
{
  return ml_gsl_blas_dsyr2k(argv[0], argv[1], argv[2],
			    argv[3], argv[4], argv[5], argv[6]);
}
