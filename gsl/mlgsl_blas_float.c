/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002-2005 - Olivier Andrieu                */
/* distributed under the terms of the GPL version 2         */


#include <gsl/gsl_blas.h>

#include "mlgsl_vector_float.h"
#include "mlgsl_matrix_float.h"

#include "mlgsl_blas.h"


/* LEVEL1 float */

CAMLprim value ml_gsl_blas_sdsdot(value alpha, value X, value Y)
{
  float r;
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_sdsdot(Double_val(alpha), &v_X, &v_Y, &r);
  return copy_double(r);
}

CAMLprim value ml_gsl_blas_dsdot(value X, value Y)
{
  double r;
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_dsdot(&v_X, &v_Y, &r);
  return copy_double(r);
}

CAMLprim value ml_gsl_blas_sdot(value X, value Y)
{
  float r;
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_sdot(&v_X, &v_Y, &r);
  return copy_double(r);
}

CAMLprim value ml_gsl_blas_snrm2(value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  return copy_double(gsl_blas_snrm2(&v_X));
}

CAMLprim value ml_gsl_blas_sasum(value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  return copy_double(gsl_blas_sasum(&v_X));
}

CAMLprim value ml_gsl_blas_isamax(value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  return Val_int(gsl_blas_isamax(&v_X));
}

CAMLprim value ml_gsl_blas_sswap(value X, value Y)
{
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_sswap(&v_X, &v_Y);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_scopy(value X, value Y)
{
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_scopy(&v_X, &v_Y);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_saxpy(value alpha, value X, value Y)
{
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_saxpy(Double_val(alpha), &v_X, &v_Y);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_srot(value X, value Y, value c, value s)
{
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_srot(&v_X, &v_Y, Double_val(c), Double_val(s));
  return Val_unit;
}

CAMLprim value ml_gsl_blas_sscal(value alpha, value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  gsl_blas_sscal(Double_val(alpha), &v_X);
  return Val_unit;
}



/* LEVEL2 float */

CAMLprim value ml_gsl_blas_sgemv(value transa, value alpha, value A, 
				 value X, value beta, value Y)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_sgemv(CBLAS_TRANS_val(transa), Double_val(alpha),
		 &m_A, &v_X, Double_val(beta), &v_Y);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_sgemv_bc(value *argv, int argc)
{
  return ml_gsl_blas_sgemv(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}

CAMLprim value ml_gsl_blas_strmv(value uplo, value transa, value diag,
				 value A, value X)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(X);
  gsl_blas_strmv(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(transa),
		 CBLAS_DIAG_val(diag), &m_A, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_strsv(value uplo, value transa, value diag,
				 value A, value X)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(X);
  gsl_blas_strsv(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(transa),
		 CBLAS_DIAG_val(diag), &m_A, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_ssymv(value uplo, value alpha, value A, 
				 value X, value beta, value Y)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_ssymv(CBLAS_UPLO_val(uplo), Double_val(alpha),
		 &m_A, &v_X, Double_val(beta), &v_Y);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_ssymv_bc(value *argv, int argc)
{
  return ml_gsl_blas_ssymv(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}

CAMLprim value ml_gsl_blas_sger(value alpha, value X, value Y, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_sger(Double_val(alpha), &v_X, &v_Y, &m_A);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_ssyr(value uplo ,value alpha, value X, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(X);
  gsl_blas_ssyr(CBLAS_UPLO_val(uplo), Double_val(alpha), 
		&v_X, &m_A);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_ssyr2(value uplo ,value alpha, value X, value Y, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_ssyr2(CBLAS_UPLO_val(uplo), Double_val(alpha), 
		&v_X, &v_Y, &m_A);
  return Val_unit;
}


/* LEVEL3 float */

CAMLprim value ml_gsl_blas_sgemm(value transa, value transb, 
				 value alpha, value A, value B, 
				 value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _CONVERT_MATRIX3(A, B, C);
  gsl_blas_sgemm(CBLAS_TRANS_val(transa), CBLAS_TRANS_val(transb),
		 Double_val(alpha), &m_A, &m_B, Double_val(beta), &m_C);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_sgemm_bc(value *argv, int argc)
{
  return ml_gsl_blas_sgemm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}


CAMLprim value ml_gsl_blas_ssymm(value side, value uplo,
				 value alpha, value A, value B, 
				 value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _CONVERT_MATRIX3(A, B, C);
  gsl_blas_ssymm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 Double_val(alpha), &m_A, &m_B, Double_val(beta), &m_C);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_ssymm_bc(value *argv, int argc)
{
  return ml_gsl_blas_ssymm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value ml_gsl_blas_ssyrk(value uplo, value trans, value alpha, 
				 value A, value beta, value C)
{
  _DECLARE_MATRIX2(A, C);
  _CONVERT_MATRIX2(A, C);
  gsl_blas_ssyrk(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(trans), 
		 Double_val(alpha), &m_A,
		 Double_val(beta), &m_C);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_ssyrk_bc(value *argv, int argc)
{
  return ml_gsl_blas_ssyrk(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}


CAMLprim value ml_gsl_blas_ssyr2k(value uplo, value trans, value alpha, 
				  value A, value B, value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _CONVERT_MATRIX3(A, B, C);
  gsl_blas_ssyr2k(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(trans), 
		  Double_val(alpha), &m_A, &m_B,
		  Double_val(beta), &m_C);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_ssyr2k_bc(value *argv, int argc)
{
  return ml_gsl_blas_ssyr2k(argv[0], argv[1], argv[2],
			    argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value ml_gsl_blas_strmm(value side, value uplo,
				 value transa, value diag,
				 value alpha, value A, value B)
{
  _DECLARE_MATRIX2(A, B);
  _CONVERT_MATRIX2(A, B);
  gsl_blas_strmm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 CBLAS_TRANS_val(transa), CBLAS_DIAG_val(diag),
		 Double_val(alpha), &m_A, &m_B);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_strmm_bc(value *argv, int argc)
{
  return ml_gsl_blas_strmm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value ml_gsl_blas_strsm(value side, value uplo,
				 value transa, value diag,
				 value alpha, value A, value B)
{
  _DECLARE_MATRIX2(A, B);
  _CONVERT_MATRIX2(A, B);
  gsl_blas_strsm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 CBLAS_TRANS_val(transa), CBLAS_DIAG_val(diag),
		 Double_val(alpha), &m_A, &m_B);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_strsm_bc(value *argv, int argc)
{
  return ml_gsl_blas_strsm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

