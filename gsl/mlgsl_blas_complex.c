/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002 - Olivier Andrieu                     */
/* distributed under the terms of the GPL version 2         */

#include <gsl/gsl_blas.h>

#include "mlgsl_complex.h"
#include "mlgsl_vector_complex.h"
#include "mlgsl_matrix_complex.h"

#include "mlgsl_blas.h"


/* LEVEL1 complex */

value ml_gsl_blas_zdotu(value X, value Y)
{
  gsl_complex r;
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_zdotu(&v_X, &v_Y, &r);
  return copy_complex(&r);
}

value ml_gsl_blas_zdotc(value X, value Y)
{
  gsl_complex r;
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_zdotc(&v_X, &v_Y, &r);
  return copy_complex(&r);
}

value ml_gsl_blas_znrm2(value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  return copy_double(gsl_blas_dznrm2(&v_X));
}

value ml_gsl_blas_zasum(value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  return copy_double(gsl_blas_dzasum(&v_X));
}

value ml_gsl_blas_izamax(value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  return Val_int(gsl_blas_izamax(&v_X));
}

value ml_gsl_blas_zswap(value X, value Y)
{
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_zswap(&v_X, &v_Y);
  return Val_unit;
}

value ml_gsl_blas_zcopy(value X, value Y)
{
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_zcopy(&v_X, &v_Y);
  return Val_unit;
}

value ml_gsl_blas_zaxpy(value alpha, value X, value Y)
{
  _DECLARE_VECTOR2(X, Y);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_VECTOR2(X, Y);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_zaxpy(z_alpha, &v_X, &v_Y);
  return Val_unit;
}

value ml_gsl_blas_zscal(value alpha, value X)
{
  _DECLARE_VECTOR(X);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_VECTOR(X);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_zscal(z_alpha, &v_X);
  return Val_unit;
}

value ml_gsl_blas_zdscal(value alpha, value X)
{
  _DECLARE_VECTOR(X);
  _CONVERT_VECTOR(X);
  gsl_blas_zdscal(Double_val(alpha), &v_X);
  return Val_unit;
}



/* LEVEL2 complex */

value ml_gsl_blas_zgemv(value transa, value alpha, value A, 
			value X, value beta, value Y)
{
  _DECLARE_MATRIX(A);
  _DECLARE_COMPLEX2(alpha, beta);
  _DECLARE_VECTOR2(X, Y);
  _CONVERT_MATRIX(A);
  _CONVERT_COMPLEX2(alpha, beta);
  _CONVERT_VECTOR2(X, Y);
  gsl_blas_zgemv(CBLAS_TRANS_val(transa), z_alpha,
		 &m_A, &v_X, z_beta, &v_Y);
  return Val_unit;
}

value ml_gsl_blas_zgemv_bc(value *argv, int argc)
{
  return ml_gsl_blas_zgemv(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}

value ml_gsl_blas_ztrmv(value uplo, value transa, value diag,
			value A, value X)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(X);
  gsl_blas_ztrmv(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(transa),
		 CBLAS_DIAG_val(diag), &m_A, &v_X);
  return Val_unit;
}

value ml_gsl_blas_ztrsv(value uplo, value transa, value diag,
			value A, value X)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(X);
  gsl_blas_ztrsv(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(transa),
		 CBLAS_DIAG_val(diag), &m_A, &v_X);
  return Val_unit;
}

value ml_gsl_blas_zhemv(value uplo, value alpha, value A,
			value X, value beta, value Y)
{
  _DECLARE_MATRIX(A);
  _DECLARE_COMPLEX2(alpha, beta);
  _DECLARE_VECTOR2(X,Y);
  _CONVERT_MATRIX(A);
  _CONVERT_COMPLEX2(alpha, beta);
  _CONVERT_VECTOR2(X,Y);
  gsl_blas_zhemv(CBLAS_UPLO_val(uplo), z_alpha, &m_A, &v_X,
		 z_beta, &v_Y);
  return Val_unit;
}

value ml_gsl_blas_zhemv_bc(value *argv, int argc)
{
  return ml_gsl_blas_zhemv(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}

value ml_gsl_blas_zgeru(value alpha, value X, value Y, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X,Y);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X,Y);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_zgeru(z_alpha, &v_X, &v_Y, &m_A);
  return Val_unit;
}

value ml_gsl_blas_zgerc(value alpha, value X, value Y, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X,Y);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X,Y);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_zgerc(z_alpha, &v_X, &v_Y, &m_A);
  return Val_unit;
}

value ml_gsl_blas_zher(value uplo, value alpha, value X, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(X);
  gsl_blas_zher(CBLAS_UPLO_val(uplo), Double_val(alpha), &v_X, &m_A);
  return Val_unit;
}

value ml_gsl_blas_zher2(value uplo, value alpha, 
			value X, value Y, value A)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(X,Y);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(X,Y);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_zher2(CBLAS_UPLO_val(uplo), z_alpha, &v_X, &v_Y, &m_A);
  return Val_unit;
}




/* LEVEL3 complex */

value ml_gsl_blas_zgemm(value transa, value transb, 
			value alpha, value A, value B, 
			value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _DECLARE_COMPLEX2(alpha, beta);
  _CONVERT_MATRIX3(A, B, C);
  _CONVERT_COMPLEX2(alpha, beta);
  gsl_blas_zgemm(CBLAS_TRANS_val(transa), CBLAS_TRANS_val(transb),
		 z_alpha, &m_A, &m_B, z_beta, &m_C);
  return Val_unit;
}

value ml_gsl_blas_zgemm_bc(value *argv, int argc)
{
  return ml_gsl_blas_zgemm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}


value ml_gsl_blas_zsymm(value side, value uplo,
			value alpha, value A, value B, 
			value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _DECLARE_COMPLEX2(alpha, beta);
  _CONVERT_MATRIX3(A, B, C);
  _CONVERT_COMPLEX2(alpha, beta);
  gsl_blas_zsymm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 z_alpha, &m_A, &m_B, z_beta, &m_C);
  return Val_unit;
}

value ml_gsl_blas_zsymm_bc(value *argv, int argc)
{
  return ml_gsl_blas_zsymm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

value ml_gsl_blas_zsyrk(value uplo, value trans, value alpha, 
			value A, value beta, value C)
{
  _DECLARE_MATRIX2(A, C);
  _DECLARE_COMPLEX2(alpha, beta);
  _CONVERT_MATRIX2(A, C);
  _CONVERT_COMPLEX2(alpha, beta);
  gsl_blas_zsyrk(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(trans), 
		 z_alpha, &m_A,
		 z_beta, &m_C);
  return Val_unit;
}

value ml_gsl_blas_zsyrk_bc(value *argv, int argc)
{
  return ml_gsl_blas_zsyrk(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}


value ml_gsl_blas_zsyr2k(value uplo, value trans, value alpha, 
			 value A, value B, value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _DECLARE_COMPLEX2(alpha, beta);
  _CONVERT_MATRIX3(A, B, C);
  _CONVERT_COMPLEX2(alpha, beta);
  gsl_blas_zsyr2k(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(trans), 
		  z_alpha, &m_A, &m_B,
		  z_beta, &m_C);
  return Val_unit;
}

value ml_gsl_blas_zsyr2k_bc(value *argv, int argc)
{
  return ml_gsl_blas_zsyr2k(argv[0], argv[1], argv[2],
			    argv[3], argv[4], argv[5], argv[6]);
}

value ml_gsl_blas_ztrmm(value side, value uplo,
			value transa, value diag,
			value alpha, value A, value B)
{
  _DECLARE_MATRIX2(A, B);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_MATRIX2(A, B);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_ztrmm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 CBLAS_TRANS_val(transa), CBLAS_DIAG_val(diag),
		 z_alpha, &m_A, &m_B);
  return Val_unit;
}

value ml_gsl_blas_ztrmm_bc(value *argv, int argc)
{
  return ml_gsl_blas_ztrmm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

value ml_gsl_blas_ztrsm(value side, value uplo,
			value transa, value diag,
			value alpha, value A, value B)
{
  _DECLARE_MATRIX2(A, B);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_MATRIX2(A, B);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_ztrsm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 CBLAS_TRANS_val(transa), CBLAS_DIAG_val(diag),
		 z_alpha, &m_A, &m_B);
  return Val_unit;
}

value ml_gsl_blas_ztrsm_bc(value *argv, int argc)
{
  return ml_gsl_blas_ztrsm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

value ml_gsl_blas_zhemm(value side, value uplo,
			value alpha, value A, value B,
			value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _DECLARE_COMPLEX2(alpha, beta);
  _CONVERT_MATRIX3(A, B, C);
  _CONVERT_COMPLEX2(alpha, beta);
  gsl_blas_zhemm(CBLAS_SIDE_val(side), CBLAS_UPLO_val(uplo),
		 z_alpha, &m_A, &m_B, z_beta, &m_C);
  return Val_unit;
}

value ml_gsl_blas_zhemm_bc(value *argv, int argc)
{
  return ml_gsl_blas_zhemm(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}

value ml_gsl_blas_zherk(value uplo, value trans,
			value alpha, value A,
			value beta, value C)
{
  _DECLARE_MATRIX2(A, C);
  _CONVERT_MATRIX2(A, C);
  gsl_blas_zherk(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(trans), 
		 Double_val(alpha), &m_A, Double_val(beta), &m_C);
  return Val_unit;
}

value ml_gsl_blas_zherk_bc(value *argv, int argc)
{
  return ml_gsl_blas_zherk(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5]);
}

value ml_gsl_blas_zher2k(value uplo, value trans,
			 value alpha, value A, value B,
			 value beta, value C)
{
  _DECLARE_MATRIX3(A, B, C);
  _DECLARE_COMPLEX(alpha);
  _CONVERT_MATRIX3(A, B, C);
  _CONVERT_COMPLEX(alpha);
  gsl_blas_zher2k(CBLAS_UPLO_val(uplo), CBLAS_TRANS_val(trans),
		  z_alpha, &m_A, &m_B, Double_val(beta), &m_C);
  return Val_unit;
}

value ml_gsl_blas_zher2k_bc(value *argv, int argc)
{
  return ml_gsl_blas_zher2k(argv[0], argv[1], argv[2],
			   argv[3], argv[4], argv[5], argv[6]);
}
