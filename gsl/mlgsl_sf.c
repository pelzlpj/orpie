/* ocamlgsl - OCaml interface to GSL                        */
/* Copyright (©) 2002 - Olivier Andrieu                     */
/* distributed under the terms of the GPL version 2         */

#include <gsl/gsl_mode.h>
#include <gsl/gsl_sf.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#include "wrappers.h"


static inline value val_of_result(gsl_sf_result *result)
{
  return copy_two_double_arr(result->val, result->err);
}

static inline value val_of_result_e10(gsl_sf_result_e10 *result)
{
  CAMLparam0();
  CAMLlocal1(r) ;
  r = alloc_small(3, 0);
  Store_field(r, 0, copy_double(result->val));
  Store_field(r, 1, copy_double(result->err));
  Store_field(r, 2, Int_val(result->e10));
  CAMLreturn(r);
}

value ml_gsl_sf_result_smash_e(value e10)
{
  gsl_sf_result r;
  gsl_sf_result_e10 e = { 
    /*.val =*/ Double_val(Field(e10, 0)),
    /*.err =*/ Double_val(Field(e10, 1)),
    /*.e10 =*/ Int_val(Field(e10, 2)) } ;
  gsl_sf_result_smash_e(&e, &r);
  return val_of_result(&r);
}

#define GSL_MODE_val Int_val


#define ML1_res(name, conv1) \
  value ml_##name(value arg1) \
  { gsl_sf_result res; \
    name(conv1(arg1), &res); \
    return val_of_result(&res); }
#define ML2_res(name, conv1, conv2) \
  value ml_##name(value arg1, value arg2) \
  { gsl_sf_result res; \
    name(conv1(arg1), conv2(arg2), &res); \
    return val_of_result(&res); }
#define ML3_res(name, conv1, conv2, conv3) \
  value ml_##name(value arg1, value arg2, value arg3) \
  { gsl_sf_result res; \
    name(conv1(arg1), conv2(arg2), conv3(arg3), &res); \
    return val_of_result(&res); }
#define ML4_res(name, conv1, conv2, conv3, conv4) \
  value ml_##name(value arg1, value arg2, value arg3, value arg4) \
  { gsl_sf_result res; \
    name(conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), &res); \
    return val_of_result(&res); }
#define ML5_res(name, conv1, conv2, conv3, conv4, conv5) \
  value ml_##name(value arg1, value arg2, value arg3, value arg4, value arg5) \
  { gsl_sf_result res; \
    name(conv1(arg1), conv2(arg2), conv3(arg3), conv4(arg4), conv5(arg5), &res); \
    return val_of_result(&res); }

#define SF1(name, conv1) \
  ML1(gsl_sf_##name, conv1, copy_double) \
  ML1_res(gsl_sf_##name##_e, conv1)
#define SF2(name, conv1, conv2) \
  ML2(gsl_sf_##name, conv1, conv2, copy_double) \
  ML2_res(gsl_sf_##name##_e, conv1, conv2)
#define SF3(name, conv1, conv2, conv3) \
  ML3(gsl_sf_##name, conv1, conv2, conv3, copy_double) \
  ML3_res(gsl_sf_##name##_e, conv1, conv2, conv3)
#define SF4(name, conv1, conv2, conv3, conv4) \
  ML4(gsl_sf_##name, conv1, conv2, conv3, conv4, copy_double) \
  ML4_res(gsl_sf_##name##_e, conv1, conv2, conv3, conv4)
#define SF5(name, conv1, conv2, conv3, conv4, conv5) \
  ML5(gsl_sf_##name, conv1, conv2, conv3, conv4, conv5, copy_double) \
  ML5_res(gsl_sf_##name##_e, conv1, conv2, conv3, conv4, conv5)



/* AIRY functions */
SF2(airy_Ai, Double_val, GSL_MODE_val)
SF2(airy_Bi, Double_val, GSL_MODE_val)
SF2(airy_Ai_scaled, Double_val, GSL_MODE_val)
SF2(airy_Bi_scaled, Double_val, GSL_MODE_val)
SF2(airy_Ai_deriv, Double_val, GSL_MODE_val)
SF2(airy_Bi_deriv, Double_val, GSL_MODE_val)
SF2(airy_Ai_deriv_scaled, Double_val, GSL_MODE_val)
SF2(airy_Bi_deriv_scaled, Double_val, GSL_MODE_val)

SF1(airy_zero_Ai, Int_val)
SF1(airy_zero_Bi, Int_val)
SF1(airy_zero_Ai_deriv, Int_val)
SF1(airy_zero_Bi_deriv, Int_val)


/* BESSEL functions */
#define BESSEL_CYL(l) \
  SF1(bessel_##l##0, Double_val) \
  SF1(bessel_##l##1, Double_val) \
  SF2(bessel_##l##n, Int_val, Double_val) \
  value ml_gsl_sf_bessel_##l##n_array(value nmin, value x, value r_arr){\
    int NMIN=Int_val(nmin); \
    int NMAX=NMIN+Double_array_length(r_arr)-1; \
    gsl_sf_bessel_##l##n_array(NMIN, NMAX, Double_val(x), Double_array_val(r_arr));\
    return Val_unit; } 
#define BESSEL_CYL_SCALED(l) \
  SF1(bessel_##l##0_scaled, Double_val) \
  SF1(bessel_##l##1_scaled, Double_val) \
  SF2(bessel_##l##n_scaled, Int_val, Double_val) \
  value ml_gsl_sf_bessel_##l##n_scaled_array(value nmin, value x, value r_arr){\
    int NMIN=Int_val(nmin); \
    int NMAX=NMIN+Double_array_length(r_arr)-1; \
    gsl_sf_bessel_##l##n_array(NMIN, NMAX, Double_val(x), Double_array_val(r_arr));\
    return Val_unit; } 

BESSEL_CYL(J)
BESSEL_CYL(Y)
BESSEL_CYL(I)
BESSEL_CYL_SCALED(I)
BESSEL_CYL(K)
BESSEL_CYL_SCALED(K)

#define BESSEL_SPH(c) \
  SF1(bessel_##c##0, Double_val) \
  SF1(bessel_##c##1, Double_val) \
  SF1(bessel_##c##2, Double_val) \
  SF2(bessel_##c##l, Int_val, Double_val) \
  value ml_gsl_sf_bessel_##c##l_array(value x, value r_arr){\
    int LMAX=Double_array_length(r_arr)-1; \
    gsl_sf_bessel_##c##l_array(LMAX, Double_val(x), Double_array_val(r_arr));\
    return Val_unit; } 
#define BESSEL_SPH_SCALED(c) \
  SF1(bessel_##c##0_scaled, Double_val) \
  SF1(bessel_##c##1_scaled, Double_val) \
  SF2(bessel_##c##l_scaled, Int_val, Double_val) \
  value ml_gsl_sf_bessel_##c##l_scaled_array(value x, value r_arr){\
    int LMAX=Double_array_length(r_arr)-1; \
    gsl_sf_bessel_##c##l_scaled_array(LMAX, Double_val(x), Double_array_val(r_arr));\
    return Val_unit; } 

BESSEL_SPH(j)
value ml_gsl_sf_bessel_jl_steed_array(value x, value x_arr)
{
  gsl_sf_bessel_jl_steed_array(Double_array_length(x_arr)-1, 
			       Double_val(x),
			       Double_array_val(x_arr));
  return Val_unit;
}
BESSEL_SPH(y)
BESSEL_SPH_SCALED(i)
BESSEL_SPH_SCALED(k)

SF2(bessel_Jnu, Double_val, Double_val)
value ml_gsl_sf_bessel_sequence_Jnu_e(value nu, value mode, value x)
{
  gsl_sf_bessel_sequence_Jnu_e(Double_val(nu), GSL_MODE_val(mode),
			       Double_array_length(x), 
			       Double_array_val(x));
  return Val_unit;
}
SF2(bessel_Ynu, Double_val, Double_val)
SF2(bessel_Inu, Double_val, Double_val)
SF2(bessel_Inu_scaled, Double_val, Double_val)
SF2(bessel_Knu, Double_val, Double_val)
SF2(bessel_lnKnu, Double_val, Double_val)
SF2(bessel_Knu_scaled, Double_val, Double_val)

SF1(bessel_zero_J0, Int_val)
SF1(bessel_zero_J1, Int_val)
SF2(bessel_zero_Jnu, Double_val, Int_val)

/* CLAUSEN functions */
SF1(clausen, Double_val)

/* COULOMB functions */
SF2(hydrogenicR_1, Double_val, Double_val)
SF4(hydrogenicR, Int_val, Int_val, Double_val, Double_val)
/* FIXME: COULOMB wave functions */
ML2_res(gsl_sf_coulomb_CL_e, Double_val, Double_val)
value ml_gsl_sf_coulomb_CL_array(value lmin, value eta, value c_arr)
{
  gsl_sf_coulomb_CL_array(Double_val(lmin), 
			  Double_array_length(c_arr)-1,
			  Double_val(eta),
			  Double_array_val(c_arr));
  return Val_unit;
}

/* FIXME: coupling coeffs */

/* DAWSON function */
SF1(dawson, Double_val)

/* DEBYE functions */
SF1(debye_1, Double_val)
SF1(debye_2, Double_val)
SF1(debye_3, Double_val)
SF1(debye_4, Double_val)

/* DILOGARITHM */
SF1(dilog, Double_val)
value ml_gsl_sf_complex_dilog_e(value r, value theta)
{
  gsl_sf_result re,im;
  gsl_sf_complex_dilog_e(Double_val(r), Double_val(theta), &re, &im);
  {
    CAMLparam0();
    CAMLlocal3(v,v_re,v_im);
    v_re=val_of_result(&re);
    v_im=val_of_result(&im);
    v=alloc_small(2, 0);
    Store_field(v, 0, v_re);
    Store_field(v, 1, v_im);
    CAMLreturn(v);
  }
}

/* ELEMENTARY operations */
ML2_res(gsl_sf_multiply_e, Double_val, Double_val)
ML4_res(gsl_sf_multiply_err_e, Double_val, Double_val, Double_val, Double_val)

/* ELLIPTIC integrals */
SF2(ellint_Kcomp, Double_val, GSL_MODE_val)
SF2(ellint_Ecomp, Double_val, GSL_MODE_val)
SF3(ellint_F, Double_val, Double_val, GSL_MODE_val)
SF3(ellint_E, Double_val, Double_val, GSL_MODE_val)
SF4(ellint_P, Double_val, Double_val, Double_val, GSL_MODE_val)
SF4(ellint_D, Double_val, Double_val, Double_val, GSL_MODE_val)
SF3(ellint_RC, Double_val, Double_val, GSL_MODE_val)
SF4(ellint_RD, Double_val, Double_val, Double_val, GSL_MODE_val)
SF4(ellint_RF, Double_val, Double_val, Double_val, GSL_MODE_val)
SF5(ellint_RJ, Double_val, Double_val, Double_val, Double_val, GSL_MODE_val)
/* FIXME: gsl_sf_elljac_e */

/* ERROR functions */
SF1(erf, Double_val)
SF1(erfc, Double_val)
SF1(log_erfc, Double_val)
SF1(erf_Z, Double_val)
SF1(erf_Q, Double_val)

/* EXPONENTIAL functions */
SF1(exp, Double_val)
value ml_gsl_sf_exp_e10_e(value x)
{
  gsl_sf_result_e10 res;
  gsl_sf_exp_e10_e(Double_val(x), &res);
  return val_of_result_e10(&res);
}
SF2(exp_mult, Double_val, Double_val)
value ml_gsl_sf_exp_mult_e10_e(value x, value y)
{
  gsl_sf_result_e10 res;
  gsl_sf_exp_mult_e10_e(Double_val(x), Double_val(y), &res);
  return val_of_result_e10(&res);
}
SF1(expm1, Double_val)
SF1(exprel, Double_val)
SF1(exprel_2, Double_val)
SF2(exprel_n, Int_val, Double_val)
ML2_res(gsl_sf_exp_err_e, Double_val, Double_val)
value ml_gsl_sf_exp_err_e10_e(value x, value dx)
{
  gsl_sf_result_e10 res;
  gsl_sf_exp_err_e10_e(Double_val(x), Double_val(dx), &res);
  return val_of_result_e10(&res);
}
ML4_res(gsl_sf_exp_mult_err_e, Double_val, Double_val, Double_val, Double_val)
value ml_gsl_sf_exp_mult_err_e10_e(value x, value dx, value y, value dy)
{
  gsl_sf_result_e10 res;
  gsl_sf_exp_mult_err_e10_e(Double_val(x), Double_val(dx), 
			    Double_val(y), Double_val(dy), &res);
  return val_of_result_e10(&res);
}

/* EXPONENTIAL integrals */
SF1(expint_E1, Double_val)
SF1(expint_E2, Double_val)
SF1(expint_E1_scaled, Double_val)
SF1(expint_E2_scaled, Double_val)
SF1(expint_Ei, Double_val)
SF1(expint_Ei_scaled, Double_val)
SF1(Shi, Double_val)
SF1(Chi, Double_val)
SF1(expint_3, Double_val)
SF1(Si, Double_val)
SF1(Ci, Double_val)
SF1(atanint, Double_val)

/* FERMI-DIRAC functions */
SF1(fermi_dirac_m1, Double_val)
SF1(fermi_dirac_0, Double_val)
SF1(fermi_dirac_1, Double_val)
SF1(fermi_dirac_2, Double_val)
SF2(fermi_dirac_int, Int_val, Double_val)
SF1(fermi_dirac_mhalf, Double_val)
SF1(fermi_dirac_half, Double_val)
SF1(fermi_dirac_3half, Double_val)
SF2(fermi_dirac_inc_0, Double_val, Double_val)

/* GAMMA function */
SF1(gamma, Double_val)
SF1(lngamma, Double_val)
value ml_gsl_sf_lngamma_sgn_e(value x)
{
  gsl_sf_result res;
  double sgn;
  gsl_sf_lngamma_sgn_e(Double_val(x), &res, &sgn);
  {
    CAMLparam0();
    CAMLlocal3(v,r,s);
    r=val_of_result(&res);
    s=copy_double(sgn);
    v=alloc_small(2, 0);
    Field(v, 0)=r;
    Field(v, 1)=s;
    CAMLreturn(v);
  }
}
SF1(gammastar, Double_val)
SF1(gammainv, Double_val)
value ml_gsl_sf_lngamma_complex_e(value zr, value zi)
{
  gsl_sf_result lnr, arg;
  gsl_sf_lngamma_complex_e(Double_val(zr), Double_val(zi),&lnr, &arg);
  {
    CAMLparam0();
    CAMLlocal3(v,r_r,r_a);
    r_r=val_of_result(&lnr);
    r_a=val_of_result(&arg);
    v=alloc_small(2, 0);
    Field(v, 0)=r_r;
    Field(v, 1)=r_a;
    CAMLreturn(v);
  }
}
SF2(taylorcoeff, Int_val, Double_val)
SF1(fact, Int_val)
SF1(doublefact, Int_val)
SF1(lnfact, Int_val)
SF1(lndoublefact, Int_val)
SF2(choose, Int_val, Int_val)
SF2(lnchoose, Int_val, Int_val)
SF2(poch, Double_val, Double_val)
SF2(lnpoch, Double_val, Double_val)
value ml_gsl_sf_lnpoch_sgn_e(value a, value x)
{
  gsl_sf_result res;
  double sgn;
  gsl_sf_lnpoch_sgn_e(Double_val(a), Double_val(x), &res, &sgn);
  {
    CAMLparam0();
    CAMLlocal3(v,r,s);
    r=val_of_result(&res);
    s=copy_double(sgn);
    v=alloc_small(2, 0);
    Field(v, 0)=r;
    Field(v, 1)=s;
    CAMLreturn(v);
  }
}
SF2(pochrel, Double_val, Double_val)
SF2(gamma_inc_Q, Double_val, Double_val)
SF2(gamma_inc_P, Double_val, Double_val)
SF2(beta, Double_val, Double_val)
SF2(lnbeta, Double_val, Double_val)
SF3(beta_inc, Double_val, Double_val, Double_val)

/* GEGENBAUER functions */
SF2(gegenpoly_1, Double_val, Double_val)
SF2(gegenpoly_2, Double_val, Double_val)
SF2(gegenpoly_3, Double_val, Double_val)
SF3(gegenpoly_n, Int_val, Double_val, Double_val)
value ml_gsl_sf_gegenpoly_array(value lambda, value x, value r_arr)
{
  gsl_sf_gegenpoly_array(Double_array_length(r_arr)-1,
			 Double_val(lambda), 
			 Double_val(x),
			 Double_array_val(r_arr));
  return Val_unit;
}

/* HYPERGEOMETRIC functions */
/* FIXME */

/* LAGUERRE functions */
SF2(laguerre_1, Double_val, Double_val)
SF2(laguerre_2, Double_val, Double_val)
SF2(laguerre_3, Double_val, Double_val)
SF3(laguerre_n, Int_val, Double_val, Double_val)

/* LAMBERT W functions */
SF1(lambert_W0, Double_val)
SF1(lambert_Wm1, Double_val)

/* LEGENDRE functions */
SF1(legendre_P1, Double_val)
SF1(legendre_P2, Double_val)
SF1(legendre_P3, Double_val)
SF2(legendre_Pl, Int_val, Double_val)
value ml_gsl_sf_legendre_Pl_array(value x, value r_arr)
{
  gsl_sf_legendre_Pl_array(Double_array_length(r_arr)-1,
			   Double_val(x),
			   Double_array_val(r_arr));
  return Val_unit;
}
SF1(legendre_Q0, Double_val)
SF1(legendre_Q1, Double_val)
SF2(legendre_Ql, Int_val, Double_val)

/* LOGARITHM and related functions */
SF1(log, Double_val)
SF1(log_abs, Double_val)
value ml_gsl_sf_complex_log_e(value zr, value zi)
{
  gsl_sf_result lnr, theta;
  gsl_sf_complex_log_e(Double_val(zr), Double_val(zi), &lnr, &theta);
  {
    CAMLparam0();
    CAMLlocal3(v,r_r,r_a);
    r_r=val_of_result(&lnr);
    r_a=val_of_result(&theta);
    v=alloc_small(2, 0);
    Field(v, 0)=r_r;
    Field(v, 1)=r_a;
    CAMLreturn(v);
  }
}
SF1(log_1plusx, Double_val)
SF1(log_1plusx_mx, Double_val)

/* POWER function */
SF2(pow_int, Double_val, Int_val)

/* PSI functions */
SF1(psi_int, Int_val)
SF1(psi, Double_val)
SF1(psi_1piy, Double_val)
SF1(psi_1_int, Int_val)
SF2(psi_n, Int_val, Double_val)

/* SYNCHROTRON functions */
SF1(synchrotron_1, Double_val)
SF1(synchrotron_2, Double_val)

/* TRANSPORT functions */
SF1(transport_2, Double_val)
SF1(transport_3, Double_val)
SF1(transport_4, Double_val)
SF1(transport_5, Double_val)

/* TRIGONOMETRIC functions */
SF1(sin, Double_val)
SF1(cos, Double_val)
SF2(hypot, Double_val, Double_val)
SF1(sinc, Double_val)
value ml_gsl_sf_complex_sin_e(value zr, value zi)
{
  gsl_sf_result szr, szi;
  gsl_sf_complex_sin_e(Double_val(zr), Double_val(zi), &szr, &szi);
  {
    CAMLparam0();
    CAMLlocal3(v,r_r,r_a);
    r_r=val_of_result(&szr);
    r_a=val_of_result(&szi);
    v=alloc_small(2, 0);
    Field(v, 0)=r_r;
    Field(v, 1)=r_a;
    CAMLreturn(v);
  }
}
value ml_gsl_sf_complex_cos_e(value zr, value zi)
{
  gsl_sf_result szr, szi;
  gsl_sf_complex_cos_e(Double_val(zr), Double_val(zi), &szr, &szi);
  {
    CAMLparam0();
    CAMLlocal3(v,r_r,r_a);
    r_r=val_of_result(&szr);
    r_a=val_of_result(&szi);
    v=alloc_small(2, 0);
    Field(v, 0)=r_r;
    Field(v, 1)=r_a;
    CAMLreturn(v);
  }
}
value ml_gsl_sf_complex_logsin_e(value zr, value zi)
{
  gsl_sf_result lszr, lszi;
  gsl_sf_complex_logsin_e(Double_val(zr), Double_val(zi), &lszr, &lszi);
  {
    CAMLparam0();
    CAMLlocal3(v,r_r,r_a);
    r_r=val_of_result(&lszr);
    r_a=val_of_result(&lszi);
    v=alloc_small(2, 0);
    Field(v, 0)=r_r;
    Field(v, 1)=r_a;
    CAMLreturn(v);
  }
}
SF1(lnsinh, Double_val)
SF1(lncosh, Double_val)
value ml_gsl_sf_polar_to_rect(value r, value theta)
{
  gsl_sf_result x, y;
  gsl_sf_polar_to_rect(Double_val(r), Double_val(theta), &x, &y);
  { 
    CAMLparam0();
    CAMLlocal3(v,r_x,r_y);
    r_x=val_of_result(&x);
    r_y=val_of_result(&y);
    v=alloc_small(2, 0);
    Field(v, 0)=r_x;
    Field(v, 1)=r_y;
    CAMLreturn(v);
  }
}
value ml_gsl_sf_rect_to_polar(value x, value y)
{
  gsl_sf_result r, theta;
  gsl_sf_rect_to_polar(Double_val(x), Double_val(y), &r, &theta);
  { 
    CAMLparam0();
    CAMLlocal3(v,r_r,r_theta);
    r_r=val_of_result(&r);
    r_theta=val_of_result(&theta);
    v=alloc_small(2, 0);
    Field(v, 0)=r_r;
    Field(v, 1)=r_theta;
    CAMLreturn(v);
  }
}
ML1(gsl_sf_angle_restrict_symm, Double_val, copy_double)
ML1(gsl_sf_angle_restrict_pos, Double_val, copy_double)
ML2_res(gsl_sf_sin_err_e, Double_val, Double_val)
ML2_res(gsl_sf_cos_err_e, Double_val, Double_val)

/* ZETA functions */
SF1(zeta_int, Int_val)
SF1(zeta, Double_val)
SF2(hzeta, Double_val, Double_val)
SF1(eta_int, Int_val)
SF1(eta, Double_val)

