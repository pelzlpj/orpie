/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002-2005 - Olivier Andrieu                */
/* distributed under the terms of the GPL version 2         */


#include <gsl/gsl_linalg.h>

#include "mlgsl_matrix_double.h"
#include "mlgsl_vector_double.h"
#include "mlgsl_permut.h"


/* simple matrix operations */
CAMLprim value ml_gsl_linalg_matmult_mod(value A, value omodA,
					 value B, value omodB,
					 value C)
{
  gsl_linalg_matrix_mod_t modA = Opt_arg(omodA, Int_val, GSL_LINALG_MOD_NONE);
  gsl_linalg_matrix_mod_t modB = Opt_arg(omodB, Int_val, GSL_LINALG_MOD_NONE);
  _DECLARE_MATRIX3(A, B, C);
  _CONVERT_MATRIX3(A, B, C);
  gsl_linalg_matmult_mod(&m_A, modA, &m_B, modB, &m_C);
  return Val_unit;
}



/* LU decomposition */

CAMLprim value ml_gsl_linalg_LU_decomp(value A, value P)
{
  int sign;
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX(A);
  _CONVERT_MATRIX(A);
  gsl_linalg_LU_decomp(&m_A, &perm_P, &sign);
  return Val_int(sign);
}

CAMLprim value ml_gsl_linalg_LU_solve(value LU, value P, value B, value X)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX(LU);
  _DECLARE_VECTOR2(B,X);
  _CONVERT_MATRIX(LU);
  _CONVERT_VECTOR2(B,X);
  gsl_linalg_LU_solve(&m_LU, &perm_P, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LU_svx(value LU, value P, value X)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX(LU);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(LU);
  _CONVERT_VECTOR(X);
  gsl_linalg_LU_svx(&m_LU, &perm_P, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LU_refine(value A, value LU, value P, 
				       value B, value X, value RES)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX2(A, LU);
  _DECLARE_VECTOR3(B, X, RES);
  _CONVERT_MATRIX2(A, LU);
  _CONVERT_VECTOR3(B, X, RES);
  gsl_linalg_LU_refine(&m_A, &m_LU, &perm_P, &v_B, &v_X, &v_RES);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LU_refine_bc(value *argv, int argc)
{
  return ml_gsl_linalg_LU_refine(argv[0], argv[1], argv[2],
				 argv[3], argv[4], argv[5]);
}

CAMLprim value ml_gsl_linalg_LU_invert(value LU, value P, value INV)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX2(LU, INV);
  _CONVERT_MATRIX2(LU, INV);
  gsl_linalg_LU_invert(&m_LU, &perm_P, &m_INV);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LU_det(value LU, value sig)
{
  _DECLARE_MATRIX(LU);
  _CONVERT_MATRIX(LU);
  return copy_double(gsl_linalg_LU_det(&m_LU, Int_val(sig)));
}

CAMLprim value ml_gsl_linalg_LU_lndet(value LU)
{
  _DECLARE_MATRIX(LU);
  _CONVERT_MATRIX(LU);
  return copy_double(gsl_linalg_LU_lndet(&m_LU));
}

CAMLprim value ml_gsl_linalg_LU_sgndet(value LU, value sig)
{
  _DECLARE_MATRIX(LU);
  _CONVERT_MATRIX(LU);
  return Val_int(gsl_linalg_LU_sgndet(&m_LU, Int_val(sig)));
}



/* QR decomposition */
CAMLprim value ml_gsl_linalg_QR_decomp(value A, value TAU)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(TAU);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(TAU);
  gsl_linalg_QR_decomp(&m_A, &v_TAU);
  return Val_unit;
}


CAMLprim value ml_gsl_linalg_QR_solve(value QR, value TAU, value B, value X)
{
  _DECLARE_MATRIX(QR);
  _DECLARE_VECTOR3(B,X,TAU);
  _CONVERT_MATRIX(QR);
  _CONVERT_VECTOR3(B,X,TAU);
  gsl_linalg_QR_solve(&m_QR, &v_TAU, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QR_svx(value QR, value TAU, value X)
{
  _DECLARE_MATRIX(QR);
  _DECLARE_VECTOR2(TAU, X);
  _CONVERT_MATRIX(QR);
  _CONVERT_VECTOR2(TAU, X);
  gsl_linalg_QR_svx(&m_QR, &v_TAU, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QR_lssolve(value QR, value TAU, value B, value X, 
			       value RES)
{
  _DECLARE_MATRIX(QR);
  _DECLARE_VECTOR4(TAU, RES, B, X);
  _CONVERT_MATRIX(QR);
  _CONVERT_VECTOR4(TAU, RES, B, X);
  gsl_linalg_QR_lssolve(&m_QR, &v_TAU, &v_B, &v_X, &v_RES);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QR_QTvec(value QR, value TAU, value V)
{
  _DECLARE_MATRIX(QR);
  _DECLARE_VECTOR2(TAU, V);
  _CONVERT_MATRIX(QR);
  _CONVERT_VECTOR2(TAU, V);
  gsl_linalg_QR_QTvec(&m_QR, &v_TAU, &v_V);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QR_Qvec(value QR, value TAU, value V)
{
  _DECLARE_MATRIX(QR);
  _DECLARE_VECTOR2(TAU, V);
  _CONVERT_MATRIX(QR);
  _CONVERT_VECTOR2(TAU, V);
  gsl_linalg_QR_Qvec(&m_QR, &v_TAU, &v_V);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QR_Rsolve(value QR, value B, value X)
{
  _DECLARE_MATRIX(QR);
  _DECLARE_VECTOR2(B,X);
  _CONVERT_MATRIX(QR);
  _CONVERT_VECTOR2(B,X);
  gsl_linalg_QR_Rsolve(&m_QR, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QR_Rsvx(value QR, value X)
{
  _DECLARE_MATRIX(QR);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(QR);
  _CONVERT_VECTOR(X);
  gsl_linalg_QR_Rsvx(&m_QR, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QR_unpack(value QR, value TAU, value Q, value R)
{
  _DECLARE_MATRIX3(QR, Q, R);
  _DECLARE_VECTOR(TAU);
  _CONVERT_MATRIX3(QR, Q, R);
  _CONVERT_VECTOR(TAU);
  gsl_linalg_QR_unpack(&m_QR, &v_TAU, &m_Q, &m_R);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QR_QRsolve(value Q, value R, value B, value X)
{
  _DECLARE_MATRIX2(Q, R);
  _DECLARE_VECTOR2(B, X);
  _CONVERT_MATRIX2(Q, R);
  _CONVERT_VECTOR2(B, X);
  gsl_linalg_QR_QRsolve(&m_Q, &m_R, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QR_update(value Q, value R, value W, value V)
{
  _DECLARE_MATRIX2(Q, R);
  _DECLARE_VECTOR2(W, V);
  _CONVERT_MATRIX2(Q, R);
  _CONVERT_VECTOR2(W, V);
  gsl_linalg_QR_update(&m_Q, &m_R, &v_W, &v_V);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_R_solve(value R, value B, value X)
{
  _DECLARE_MATRIX(R);
  _DECLARE_VECTOR2(B, X);
  _CONVERT_MATRIX(R);
  _CONVERT_VECTOR2(B, X);
  gsl_linalg_R_solve(&m_R, &v_B, &v_X);
  return Val_unit;
}

/* missing ? */
/*  value ml_gsl_linalg_R_svx(value R, value X) */
/*  { */
/*    DECLARE_MATRIX(R); */
/*    DECLARE_VECTOR(X); */
/*    gsl_linalg_R_svx(&m_R, &v_X); */
/*    return Val_unit; */
/*  } */



/* QR Decomposition with Column Pivoting */
CAMLprim value ml_gsl_linalg_QRPT_decomp(value A, value TAU, value P, value NORM)
{
  int signum;
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(TAU, NORM);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(TAU, NORM);
  gsl_linalg_QRPT_decomp(&m_A, &v_TAU, &perm_P, &signum, &v_NORM);
  return Val_int(signum);
}

CAMLprim value ml_gsl_linalg_QRPT_decomp2(value A, value Q, value R,
					  value TAU, value P, value NORM)
{
  int signum;
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX3(A, Q, R);
  _DECLARE_VECTOR2(TAU, NORM);
  _CONVERT_MATRIX3(A, Q, R);
  _CONVERT_VECTOR2(TAU, NORM);
  gsl_linalg_QRPT_decomp2(&m_A, &m_Q, &m_R, &v_TAU, &perm_P, &signum, &v_NORM);
  return Val_int(signum);
}

CAMLprim value ml_gsl_linalg_QRPT_decomp2_bc(value *argv, int argc)
{
  return ml_gsl_linalg_QRPT_decomp2(argv[0], argv[1], argv[2],
				    argv[3], argv[4], argv[5]);
}

CAMLprim value ml_gsl_linalg_QRPT_solve(value QR, value TAU, value P, 
					value B, value X)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX(QR);
  _DECLARE_VECTOR3(TAU, B, X);
  _CONVERT_MATRIX(QR);
  _CONVERT_VECTOR3(TAU, B, X);
  gsl_linalg_QRPT_solve(&m_QR, &v_TAU, &perm_P, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QRPT_svx(value QR, value TAU, value P, value X)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX(QR);
  _DECLARE_VECTOR2(TAU, X);
  _CONVERT_MATRIX(QR);
  _CONVERT_VECTOR2(TAU, X);
  gsl_linalg_QRPT_svx(&m_QR, &v_TAU, &perm_P, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QRPT_QRsolve(value Q, value R, value P, 
					  value B, value X)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX2(Q, R);
  _DECLARE_VECTOR2(B, X);
  _CONVERT_MATRIX2(Q, R);
  _CONVERT_VECTOR2(B, X);
  gsl_linalg_QRPT_QRsolve(&m_Q, &m_R, &perm_P, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QRPT_update(value Q, value R, value P, 
				value U, value V)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX2(Q, R);
  _DECLARE_VECTOR2(U, V);
  _CONVERT_MATRIX2(Q, R);
  _CONVERT_VECTOR2(U, V);
  gsl_linalg_QRPT_update(&m_Q, &m_R, &perm_P, &v_U, &v_V);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QRPT_Rsolve(value QR, value P, 
				value B, value X)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX(QR);
  _DECLARE_VECTOR2(B, X);
  _CONVERT_MATRIX(QR);
  _CONVERT_VECTOR2(B, X);
  gsl_linalg_QRPT_Rsolve(&m_QR, &perm_P, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_QRPT_Rsvx(value QR, value P, value X)
{
  GSL_PERMUT_OF_BIGARRAY(P);
  _DECLARE_MATRIX(QR);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(QR);
  _CONVERT_VECTOR(X);
  gsl_linalg_QRPT_Rsvx(&m_QR, &perm_P, &v_X);
  return Val_unit;
}



/* Singular Value Decomposition */
CAMLprim value ml_gsl_linalg_SV_decomp(value A, value V, value S, value WORK)
{
  _DECLARE_MATRIX2(A, V);
  _DECLARE_VECTOR2(S, WORK);
  _CONVERT_MATRIX2(A, V);
  _CONVERT_VECTOR2(S, WORK);
  gsl_linalg_SV_decomp(&m_A, &m_V, &v_S, &v_WORK);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_SV_decomp_mod(value A, value X, value V, 
					   value S, value WORK)
{
  _DECLARE_MATRIX3(A, V, X);
  _DECLARE_VECTOR2(S, WORK);
  _CONVERT_MATRIX3(A, V, X);
  _CONVERT_VECTOR2(S, WORK);
  gsl_linalg_SV_decomp_mod(&m_A, &m_X, &m_V, &v_S, &v_WORK);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_SV_decomp_jacobi(value A, value V, value S)
{
  _DECLARE_MATRIX2(A, V);
  _DECLARE_VECTOR(S);
  _CONVERT_MATRIX2(A, V);
  _CONVERT_VECTOR(S);
  gsl_linalg_SV_decomp_jacobi(&m_A, &m_V, &v_S);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_SV_solve(value U, value V, value S, value B, value X)
{
  _DECLARE_MATRIX2(U, V);
  _DECLARE_VECTOR3(S, B, X);
  _CONVERT_MATRIX2(U, V);
  _CONVERT_VECTOR3(S, B, X);
  gsl_linalg_SV_solve(&m_U, &m_V, &v_S, &v_B, &v_X);
  return Val_unit;
}



/* LQ decomposition */
CAMLprim value ml_gsl_linalg_LQ_decomp(value A, value TAU)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(TAU);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(TAU);
  gsl_linalg_LQ_decomp(&m_A, &v_TAU);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LQ_solve_T(value LQ, value TAU, value B, value X)
{
  _DECLARE_MATRIX(LQ);
  _DECLARE_VECTOR3(B,X,TAU);
  _CONVERT_MATRIX(LQ);
  _CONVERT_VECTOR3(B,X,TAU);
  gsl_linalg_LQ_solve_T(&m_LQ, &v_TAU, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LQ_svx_T(value LQ, value TAU, value X)
{
  _DECLARE_MATRIX(LQ);
  _DECLARE_VECTOR2(TAU, X);
  _CONVERT_MATRIX(LQ);
  _CONVERT_VECTOR2(TAU, X);
  gsl_linalg_LQ_svx_T(&m_LQ, &v_TAU, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LQ_lssolve_T(value LQ, value TAU, 
					  value B, value X, 
					  value RES)
{
  _DECLARE_MATRIX(LQ);
  _DECLARE_VECTOR4(TAU, RES, B, X);
  _CONVERT_MATRIX(LQ);
  _CONVERT_VECTOR4(TAU, RES, B, X);
  gsl_linalg_LQ_lssolve_T(&m_LQ, &v_TAU, &v_B, &v_X, &v_RES);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LQ_Lsolve_T(value LQ, value B, value X)
{
  _DECLARE_MATRIX(LQ);
  _DECLARE_VECTOR2(B,X);
  _CONVERT_MATRIX(LQ);
  _CONVERT_VECTOR2(B,X);
  gsl_linalg_LQ_Lsolve_T(&m_LQ, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LQ_Lsvx_T(value LQ, value X)
{
  _DECLARE_MATRIX(LQ);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(LQ);
  _CONVERT_VECTOR(X);
  gsl_linalg_LQ_Lsvx_T(&m_LQ, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_L_solve_T(value L, value B, value X)
{
  _DECLARE_MATRIX(L);
  _DECLARE_VECTOR2(B,X);
  _CONVERT_MATRIX(L);
  _CONVERT_VECTOR2(B,X);
  gsl_linalg_L_solve_T(&m_L, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LQ_vecQ(value LQ, value TAU, value V)
{
  _DECLARE_MATRIX(LQ);
  _DECLARE_VECTOR2(V,TAU);
  _CONVERT_MATRIX(LQ);
  _CONVERT_VECTOR2(V,TAU);
  gsl_linalg_LQ_vecQ(&m_LQ, &v_TAU, &v_V);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LQ_vecQT(value LQ, value TAU, value V)
{
  _DECLARE_MATRIX(LQ);
  _DECLARE_VECTOR2(V,TAU);
  _CONVERT_MATRIX(LQ);
  _CONVERT_VECTOR2(V,TAU);
  gsl_linalg_LQ_vecQT(&m_LQ, &v_TAU, &v_V);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LQ_unpack(value LQ, value TAU, value Q, value L)
{
  _DECLARE_MATRIX3(LQ,Q,L);
  _DECLARE_VECTOR(TAU);
  _CONVERT_MATRIX3(LQ,Q,L);
  _CONVERT_VECTOR(TAU);
  gsl_linalg_LQ_unpack(&m_LQ, &v_TAU, &m_Q, &m_L);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LQ_update(value LQ, value R, value V, value W)
{
  _DECLARE_MATRIX2(LQ,R);
  _DECLARE_VECTOR2(V,W);
  _CONVERT_MATRIX2(LQ,R);
  _CONVERT_VECTOR2(V,W);
  gsl_linalg_LQ_update(&m_LQ, &m_R, &v_V, &v_W);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_LQ_LQsolve(value Q, value L, value B, value X)
{
  _DECLARE_MATRIX2(Q,L);
  _DECLARE_VECTOR2(B,X);
  _CONVERT_MATRIX2(Q,L);
  _CONVERT_VECTOR2(B,X);
  gsl_linalg_LQ_LQsolve(&m_Q, &m_L, &v_B, &v_X);
  return Val_unit;
}


/* P^T L Q decomposition */
CAMLprim value ml_gsl_linalg_PTLQ_decomp (value A, value TAU, value P, value NORM)
{
  int signum;
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(TAU,NORM);
  GSL_PERMUT_OF_BIGARRAY(P);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(TAU,NORM);
  gsl_linalg_PTLQ_decomp (&m_A, &v_TAU, &perm_P, &signum, &v_NORM);
  return Val_int (signum);
}

CAMLprim value ml_gsl_linalg_PTLQ_decomp2 (value A, value Q, value R, 
					   value TAU, value P, value NORM)
{
  int signum;
  _DECLARE_MATRIX3(A,Q,R);
  _DECLARE_VECTOR2(TAU,NORM);
  GSL_PERMUT_OF_BIGARRAY(P);
  _CONVERT_MATRIX3(A,Q,R);
  _CONVERT_VECTOR2(TAU,NORM);
  gsl_linalg_PTLQ_decomp2 (&m_A, &m_Q, &m_R, &v_TAU, &perm_P, &signum, &v_NORM);
  return Val_int (signum);
}

CAMLprim value ml_gsl_linalg_PTLQ_decomp2_bc (value *argv, int argc)
{
  return ml_gsl_linalg_PTLQ_decomp2 (argv[0], argv[1], argv[2],
				     argv[3], argv[4], argv[5]);
}

CAMLprim value ml_gsl_linalg_PTLQ_solve_T (value QR, value TAU, value P, value B, value X)
{
  _DECLARE_MATRIX(QR);
  _DECLARE_VECTOR3(TAU,B,X);
  GSL_PERMUT_OF_BIGARRAY(P);
  _CONVERT_MATRIX(QR);
  _CONVERT_VECTOR3(TAU,B,X);
  gsl_linalg_PTLQ_solve_T (&m_QR, &v_TAU, &perm_P, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_PTLQ_svx_T (value QR, value TAU, value P, value X)
{
  _DECLARE_MATRIX(QR);
  _DECLARE_VECTOR2(TAU,X);
  GSL_PERMUT_OF_BIGARRAY(P);
  _CONVERT_MATRIX(QR);
  _CONVERT_VECTOR2(TAU,X);
  gsl_linalg_PTLQ_svx_T (&m_QR, &v_TAU, &perm_P, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_PTLQ_LQsolve_T (value Q, value L, value P, value B, value X)
{
  _DECLARE_MATRIX2(Q,L);
  _DECLARE_VECTOR2(B,X);
  GSL_PERMUT_OF_BIGARRAY(P);
  _CONVERT_MATRIX2(Q,L);
  _CONVERT_VECTOR2(B,X);
  gsl_linalg_PTLQ_LQsolve_T (&m_Q, &m_L, &perm_P, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_PTLQ_Lsolve_T (value LQ, value P, value B, value X)
{
  _DECLARE_MATRIX(LQ);
  _DECLARE_VECTOR2(B,X);
  GSL_PERMUT_OF_BIGARRAY(P);
  _CONVERT_MATRIX(LQ);
  _CONVERT_VECTOR2(B,X);
  gsl_linalg_PTLQ_Lsolve_T (&m_LQ, &perm_P, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_PTLQ_Lsvx_T (value LQ, value P, value X)
{
  _DECLARE_MATRIX(LQ);
  _DECLARE_VECTOR(X);
  GSL_PERMUT_OF_BIGARRAY(P);
  _CONVERT_MATRIX(LQ);
  _CONVERT_VECTOR(X);
  gsl_linalg_PTLQ_Lsvx_T (&m_LQ, &perm_P, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_PTLQ_update (value Q, value L, value P, value V, value W)
{
  _DECLARE_MATRIX2(Q,L);
  _DECLARE_VECTOR2(V,W);
  GSL_PERMUT_OF_BIGARRAY(P);
  _CONVERT_MATRIX2(Q,L);
  _CONVERT_VECTOR2(V,W);
  gsl_linalg_PTLQ_update (&m_Q, &m_L, &perm_P, &v_V, &v_W);
  return Val_unit;
}


/* Cholesky decomposition */

CAMLprim value ml_gsl_linalg_cholesky_decomp(value A)
{
  _DECLARE_MATRIX(A);
  _CONVERT_MATRIX(A);
  gsl_linalg_cholesky_decomp(&m_A);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_cholesky_solve(value CHO, value B, value X)
{
  _DECLARE_MATRIX(CHO);
  _DECLARE_VECTOR2(B, X);
  _CONVERT_MATRIX(CHO);
  _CONVERT_VECTOR2(B, X);
  gsl_linalg_cholesky_solve(&m_CHO, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_cholesky_svx(value CHO, value X)
{
  _DECLARE_MATRIX(CHO);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(CHO);
  _CONVERT_VECTOR(X);
  gsl_linalg_cholesky_svx(&m_CHO, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_cholesky_decomp_unit(value CHO, value D)
{
  _DECLARE_MATRIX(CHO);
  _DECLARE_VECTOR(D);
  _CONVERT_MATRIX(CHO);
  _CONVERT_VECTOR(D);
  gsl_linalg_cholesky_decomp_unit(&m_CHO, &v_D);
  return Val_unit;
}



/* Tridiagonal Decomposition of Real Symmetric Matrices */
CAMLprim value ml_gsl_linalg_symmtd_decomp(value A, value TAU)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(TAU);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(TAU);
  gsl_linalg_symmtd_decomp(&m_A, &v_TAU);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_symmtd_unpack(value A, value TAU, value Q, 
					   value DIAG, value SUBDIAG)
{
  _DECLARE_MATRIX2(A, Q);
  _DECLARE_VECTOR3(TAU, DIAG, SUBDIAG);
  _CONVERT_MATRIX2(A, Q);
  _CONVERT_VECTOR3(TAU, DIAG, SUBDIAG);
  gsl_linalg_symmtd_unpack(&m_A, &v_TAU, &m_Q, &v_DIAG, &v_SUBDIAG);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_symmtd_unpack_T(value A, value DIAG, value SUBDIAG)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(DIAG, SUBDIAG);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(DIAG, SUBDIAG);
  gsl_linalg_symmtd_unpack_T(&m_A, &v_DIAG, &v_SUBDIAG);
  return Val_unit;
}



/* Tridiagonal Decomposition of Hermitian Matrices */



/* Bidiagonalization */
CAMLprim value ml_gsl_linalg_bidiag_decomp(value A, value TAU_U, value TAU_V)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(TAU_U, TAU_V);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(TAU_U, TAU_V);
  gsl_linalg_bidiag_decomp(&m_A, &v_TAU_U, &v_TAU_V);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_bidiag_unpack(value A, value TAU_U, value U, 
					   value TAU_V, value V, 
					   value DIAG, value SUPERDIAG)
{
  _DECLARE_MATRIX3(A, U, V);
  _DECLARE_VECTOR4(TAU_U, TAU_V, DIAG, SUPERDIAG);
  _CONVERT_MATRIX3(A, U, V);
  _CONVERT_VECTOR4(TAU_U, TAU_V, DIAG, SUPERDIAG);
  gsl_linalg_bidiag_unpack(&m_A, &v_TAU_U, &m_U, &v_TAU_V, &m_V,
			   &v_DIAG, &v_SUPERDIAG);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_bidiag_unpack_bc(value *argv, int argc)
{
  return ml_gsl_linalg_bidiag_unpack(argv[0], argv[1], argv[2], 
				     argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value ml_gsl_linalg_bidiag_unpack2(value A, value TAU_U, value TAU_V, value V)
{
  _DECLARE_MATRIX2(A, V);
  _DECLARE_VECTOR2(TAU_U, TAU_V);
  _CONVERT_MATRIX2(A, V);
  _CONVERT_VECTOR2(TAU_U, TAU_V);
  gsl_linalg_bidiag_unpack2(&m_A, &v_TAU_U, &v_TAU_V, &m_V);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_bidiag_unpack_B(value A, value DIAG, value SUPERDIAG)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(DIAG, SUPERDIAG);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(DIAG, SUPERDIAG);
  gsl_linalg_bidiag_unpack_B(&m_A, &v_DIAG, &v_SUPERDIAG);
  return Val_unit;
}



/* Householder solver */
CAMLprim value ml_gsl_linalg_HH_solve(value A, value B, value X)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR2(B,X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR2(B,X);
  gsl_linalg_HH_solve(&m_A, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_HH_svx(value A, value X)
{
  _DECLARE_MATRIX(A);
  _DECLARE_VECTOR(X);
  _CONVERT_MATRIX(A);
  _CONVERT_VECTOR(X);
  gsl_linalg_HH_svx(&m_A, &v_X);
  return Val_unit;
}


/* Tridiagonal Systems */
CAMLprim value ml_gsl_linalg_solve_symm_tridiag(value DIAG, value E, value B, value X)
{
  _DECLARE_VECTOR4(DIAG, E, B, X);
  _CONVERT_VECTOR4(DIAG, E, B, X);
  gsl_linalg_solve_symm_tridiag(&v_DIAG, &v_E, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_solve_tridiag(value DIAG, value ABOVE, value BELOW,
					   value B, value X)
{
  _DECLARE_VECTOR5(DIAG, ABOVE, BELOW, B, X);
  _CONVERT_VECTOR5(DIAG, ABOVE, BELOW, B, X);
  gsl_linalg_solve_tridiag(&v_DIAG, &v_ABOVE, &v_BELOW, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_solve_symm_cyc_tridiag(value DIAG, value E, value B, value X)
{
  _DECLARE_VECTOR4(DIAG, E, B, X);
  _CONVERT_VECTOR4(DIAG, E, B, X);
  gsl_linalg_solve_symm_cyc_tridiag(&v_DIAG, &v_E, &v_B, &v_X);
  return Val_unit;
}

CAMLprim value ml_gsl_linalg_solve_cyc_tridiag(value DIAG, value ABOVE, value BELOW,
					       value B, value X)
{
  _DECLARE_VECTOR5(DIAG, ABOVE, BELOW, B, X);
  _CONVERT_VECTOR5(DIAG, ABOVE, BELOW, B, X);
  gsl_linalg_solve_cyc_tridiag(&v_DIAG, &v_ABOVE, &v_BELOW, &v_B, &v_X);
  return Val_unit;
}


/* exponential */
#define GSL_MODE_val Int_val

CAMLprim value ml_gsl_linalg_exponential_ss(value A, value eA, value mode)
{
  _DECLARE_MATRIX2(A, eA);
  _CONVERT_MATRIX2(A, eA);
  gsl_linalg_exponential_ss(&m_A, &m_eA, GSL_MODE_val(mode));
  return Val_unit;
}
