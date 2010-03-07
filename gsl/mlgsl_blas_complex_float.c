/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002-2005 - Olivier Andrieu                */
/* distributed under the terms of the GPL version 2         */

#include <gsl/gsl_blas.h>

#define FLOAT_COMPLEX

#include "mlgsl_complex.h"
#include "mlgsl_vector_complex_float.h"
#include "mlgsl_matrix_complex_float.h"

#include "mlgsl_blas.h"


/* LEVEL1 complex float */

CAMLprim value ml_gsl_blas_cdotu(value X, value Y)
{
  gsl_complex_float r;
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_cdotu(&v_X, &v_Y, &r);
  return copy_complex(&r);
}

CAMLprim value ml_gsl_blas_cdotc(value X, value Y)
{
  gsl_complex_float r;
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_cdotc(&v_X, &v_Y, &r);
  return copy_complex(&r);
}

CAMLprim value ml_gsl_blas_scnrm2(value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  return copy_double(gsl_blas_scnrm2(&v_X));
}

CAMLprim value ml_gsl_blas_scasum(value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  return copy_double(gsl_blas_scasum(&v_X));
}

CAMLprim value ml_gsl_blas_icamax(value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  return Val_int(gsl_blas_icamax(&v_X));
}

CAMLprim value ml_gsl_blas_cswap(value X, value Y)
{
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_cswap(&v_X, &v_Y);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_ccopy(value X, value Y)
{
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_ccopy(&v_X, &v_Y);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_caxpy(value alpha, value X, value Y)
{
  _DECLARE_VECTOR2(X, Y);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_VECTOR2(X, Y);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_caxpy(z_alpha, &v_X, &v_Y);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_cscal(value alpha, value X)
{
  _DECLARE_VECTOR(X);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_VECTOR(X);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_cscal(z_alpha, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_csscal(value alpha, value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  gsl_blas_csscal(Double_val(alpha), &v_X);
  return Val_unit;
}



/* LEVEL2 complex float */

CAMLprim value ml_gsl_blas_cgemv(value transa, value alpha, value A, 
				 value X, value beta, value Y)
{
  _DECLARE_MATRIX(A);
  _DECLARE_COMPLEX2(alpha, beta);
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_MATRIX(A);
  _CONVERT_COMPLEX2(alpha, beta);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_cgemv(CBLAS_TRANS_val(transa), z_alpha,
		 &m_A, &v_X, z_beta, &v_Y);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_cgemv_bc(value *argv, int argc)
{
  return ml_gsl_blas_cgemv(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}

CAMLprim value ml_gsl_blas_ctrmv(value uplo, value transa, value diag,
				 value A, value X)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(X);
  gsl_blas_ctrmv(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(transa),
		 CBLAS_DIAG_val(diag), &m_A, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_ctrsv(value uplo, value transa, value diag,
				 value A, value X)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(X);
  gsl_blas_ctrsv(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(transa),
		 CBLAS_DIAG_val(diag), &m_A, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_chemv(value uplo, value alpha, value A,
				 value X, value beta, value Y)
{
  _DECLARE_MATRIX(A);
  _DECLARE_COMPLEX2(alpha, beta);
  _DECLARE_VECTOR2(X,Y);
  _CONVERT_MATRIX(A);
  _CONVERT_COMPLEX2(alpha, beta);
  _CONVERT_VECTOR2(X,Y);
  gsl_blas_chemv(CBLAS_UPLO_val(uplo), z_alpha, &m_A, &v_X,
		 z_beta, &v_Y);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_chemv_bc(value *argv, int argc)
{
  return ml_gsl_blas_chemv(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}

CAMLprim value ml_gsl_blas_cgeru(value alpha, value X, value Y, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X,Y);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X,Y);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_cgeru(z_alpha, &v_X, &v_Y, &m_A);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_cgerc(value alpha, value X, value Y, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X,Y);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X,Y);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_cgerc(z_alpha, &v_X, &v_Y, &m_A);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_cher(value uplo, value alpha, value X, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(X);
  gsl_blas_cher(CBLAS_UPLO_val(uplo), Double_val(alpha), &v_X, &m_A);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_cher2(value uplo, value alpha, 
				 value X, value Y, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X,Y);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X,Y);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_cher2(CBLAS_UPLO_val(uplo), z_alpha, &v_X, &v_Y, &m_A);
  return Val_unit;
}




/* LEVEL3 complex float */

CAMLprim value ml_gsl_blas_cgemm(value transa, value transb, 
				 value alpha, value A, value B, 
				 value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _DECLARE_COMPLEX2(alpha, beta);
  _CONVERT_MATRIX3(A, B, C);
  _CONVERT_COMPLEX2(alpha, beta);
  gsl_blas_cgemm(CBLAS_TRANS_val(transa), CBLAS_TRANS_val(transb),
		 z_alpha, &m_A, &m_B, z_beta, &m_C);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_cgemm_bc(value *argv, int argc)
{
  return ml_gsl_blas_cgemm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}


CAMLprim value ml_gsl_blas_csymm(value side, value uplo,
				 value alpha, value A, value B, 
				 value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _DECLARE_COMPLEX2(alpha, beta);
  _CONVERT_MATRIX3(A, B, C);
  _CONVERT_COMPLEX2(alpha, beta);
  gsl_blas_csymm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 z_alpha, &m_A, &m_B, z_beta, &m_C);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_csymm_bc(value *argv, int argc)
{
  return ml_gsl_blas_csymm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value ml_gsl_blas_csyrk(value uplo, value trans, value alpha, 
				 value A, value beta, value C)
{
  _DECLARE_MATRIX2(A, C);
  _DECLARE_COMPLEX2(alpha, beta);
  _CONVERT_MATRIX2(A, C);
  _CONVERT_COMPLEX2(alpha, beta);
  gsl_blas_csyrk(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(trans), 
		 z_alpha, &m_A,
		 z_beta, &m_C);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_csyrk_bc(value *argv, int argc)
{
  return ml_gsl_blas_csyrk(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}


CAMLprim value ml_gsl_blas_csyr2k(value uplo, value trans, value alpha, 
				  value A, value B, value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _DECLARE_COMPLEX2(alpha, beta);
  _CONVERT_MATRIX3(A, B, C);
  _CONVERT_COMPLEX2(alpha, beta);
  gsl_blas_csyr2k(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(trans), 
		  z_alpha, &m_A, &m_B,
		  z_beta, &m_C);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_csyr2k_bc(value *argv, int argc)
{
  return ml_gsl_blas_csyr2k(argv[0], argv[1], argv[2],
			    argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value ml_gsl_blas_ctrmm(value side, value uplo,
				 value transa, value diag,
				 value alpha, value A, value B)
{
  _DECLARE_MATRIX2(A, B);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_MATRIX2(A, B);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_ctrmm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 CBLAS_TRANS_val(transa), CBLAS_DIAG_val(diag),
		 z_alpha, &m_A, &m_B);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_ctrmm_bc(value *argv, int argc)
{
  return ml_gsl_blas_ctrmm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value ml_gsl_blas_ctrsm(value side, value uplo,
				 value transa, value diag,
				 value alpha, value A, value B)
{
  _DECLARE_MATRIX2(A, B);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_MATRIX2(A, B);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_ctrsm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 CBLAS_TRANS_val(transa), CBLAS_DIAG_val(diag),
		 z_alpha, &m_A, &m_B);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_ctrsm_bc(value *argv, int argc)
{
  return ml_gsl_blas_ctrsm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value ml_gsl_blas_chemm(value side, value uplo,
				 value alpha, value A, value B,
				 value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _DECLARE_COMPLEX2(alpha, beta);
  _CONVERT_MATRIX3(A, B, C);
  _CONVERT_COMPLEX2(alpha, beta);
  gsl_blas_chemm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 z_alpha, &m_A, &m_B, z_beta, &m_C);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_chemm_bc(value *argv, int argc)
{
  return ml_gsl_blas_chemm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value ml_gsl_blas_cherk(value uplo, value trans,
				 value alpha, value A,
				 value beta, value C)
{
  _DECLARE_MATRIX2(A, C);
  _CONVERT_MATRIX2(A, C);
  gsl_blas_cherk(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(trans), 
		 Double_val(alpha), &m_A, Double_val(beta), &m_C);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_cherk_bc(value *argv, int argc)
{
  return ml_gsl_blas_cherk(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}

CAMLprim value ml_gsl_blas_cher2k(value uplo, value trans,
				  value alpha, value A, value B,
				  value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_MATRIX3(A, B, C);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_cher2k(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(trans),
		  z_alpha, &m_A, &m_B, Double_val(beta), &m_C);
  return Val_unit;
}

CAMLprim value ml_gsl_blas_cher2k_bc(value *argv, int argc)
{
  return ml_gsl_blas_cher2k(argv[0], argv[1], argv[2],
			    argv[3], argv[4], argv[5], argv[6]);
}
