(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002 - Olivier Andrieu                     *)
(* distributed under the terms of the GPL version 2         *)

open Gsl_fun

(* AIRY functions *)
external airy_Ai : float -> mode -> float
    = "ml_gsl_sf_airy_Ai"

external airy_Ai_e : float -> mode -> result
    = "ml_gsl_sf_airy_Ai_e"


external airy_Bi : float -> mode -> float
    = "ml_gsl_sf_airy_Bi"

external airy_Bi_e : float -> mode -> result
    = "ml_gsl_sf_airy_Bi_e"


external airy_Ai_scaled : float -> mode -> float
    = "ml_gsl_sf_airy_Ai_scaled"

external airy_Ai_scaled_e : float -> mode -> result
    = "ml_gsl_sf_airy_Ai_scaled_e"


external airy_Bi_scaled : float -> mode -> float
    = "ml_gsl_sf_airy_Bi_scaled"

external airy_Bi_scaled_e : float -> mode -> result
    = "ml_gsl_sf_airy_Bi_scaled_e"


external airy_Ai_deriv : float -> mode -> float
    = "ml_gsl_sf_airy_Ai_deriv"

external airy_Ai_deriv_e : float -> mode -> result
    = "ml_gsl_sf_airy_Ai_deriv_e"


external airy_Bi_deriv : float -> mode -> float
    = "ml_gsl_sf_airy_Bi_deriv"

external airy_Bi_deriv_e : float -> mode -> result
    = "ml_gsl_sf_airy_Bi_deriv_e"


external airy_Ai_deriv_scaled : float -> mode -> float
    = "ml_gsl_sf_airy_Ai_deriv_scaled"

external airy_Ai_deriv_scaled_e : float -> mode -> result
    = "ml_gsl_sf_airy_Ai_deriv_scaled_e"


external airy_Bi_deriv_scaled : float -> mode -> float
    = "ml_gsl_sf_airy_Bi_deriv_scaled"

external airy_Bi_deriv_scaled_e : float -> mode -> result
    = "ml_gsl_sf_airy_Bi_deriv_scaled_e"



external airy_zero_Ai : int -> float
    = "ml_gsl_sf_airy_zero_Ai"

external airy_zero_Ai_e : int -> result
    = "ml_gsl_sf_airy_zero_Ai_e"


external airy_zero_Bi : int -> float
    = "ml_gsl_sf_airy_zero_Bi"

external airy_zero_Bi_e : int -> result
    = "ml_gsl_sf_airy_zero_Bi_e"



(* BESSEL functions *)
external bessel_J0 : float -> float
    = "ml_gsl_sf_bessel_J0" "gsl_sf_bessel_J0" "float"

external bessel_J0_e : float -> result
    = "ml_gsl_sf_bessel_J0_e"

external bessel_J1 : float -> float
    = "ml_gsl_sf_bessel_J1" "gsl_sf_bessel_J1" "float"

external bessel_J1_e : float -> result
    = "ml_gsl_sf_bessel_J1_e"

external bessel_Jn : int -> float -> float
    = "ml_gsl_sf_bessel_Jn"

external bessel_Jn_e : int -> float -> result
    = "ml_gsl_sf_bessel_Jn_e"

external bessel_Jn_array : int -> float -> float array -> unit
    = "ml_gsl_sf_bessel_Jn_array"


external bessel_Y0 : float -> float
    = "ml_gsl_sf_bessel_Y0" "gsl_sf_bessel_Y0" "float"

external bessel_Y0_e : float -> result
    = "ml_gsl_sf_bessel_Y0_e"

external bessel_Y1 : float -> float
    = "ml_gsl_sf_bessel_Y1" "gsl_sf_bessel_Y1" "float"

external bessel_Y1_e : float -> result
    = "ml_gsl_sf_bessel_Y1_e"

external bessel_Yn : int -> float -> float
    = "ml_gsl_sf_bessel_Yn"

external bessel_Yn_e : int -> float -> result
    = "ml_gsl_sf_bessel_Yn_e"

external bessel_Yn_array : int -> float -> float array -> unit
    = "ml_gsl_sf_bessel_Yn_array"


external bessel_I0 : float -> float
    = "ml_gsl_sf_bessel_I0" "gsl_sf_bessel_I0" "float"

external bessel_I0_e : float -> result
    = "ml_gsl_sf_bessel_I0_e"

external bessel_I1 : float -> float
    = "ml_gsl_sf_bessel_I1" "gsl_sf_bessel_I1" "float"

external bessel_I1_e : float -> result
    = "ml_gsl_sf_bessel_I1_e"

external bessel_In : int -> float -> float
    = "ml_gsl_sf_bessel_In"

external bessel_In_e : int -> float -> result
    = "ml_gsl_sf_bessel_In_e"

external bessel_In_array : int -> float -> float array -> unit
    = "ml_gsl_sf_bessel_In_array"


external bessel_K0 : float -> float
    = "ml_gsl_sf_bessel_K0" "gsl_sf_bessel_K0" "float"

external bessel_K0_e : float -> result
    = "ml_gsl_sf_bessel_K0_e"

external bessel_K1 : float -> float
    = "ml_gsl_sf_bessel_K1" "gsl_sf_bessel_K1" "float"

external bessel_K1_e : float -> result
    = "ml_gsl_sf_bessel_K1_e"

external bessel_Kn : int -> float -> float
    = "ml_gsl_sf_bessel_Kn"

external bessel_Kn_e : int -> float -> result
    = "ml_gsl_sf_bessel_Kn_e"

external bessel_Kn_array : int -> float -> float array -> unit
    = "ml_gsl_sf_bessel_Kn_array"


external bessel_I0_scaled : float -> float
    = "ml_gsl_sf_bessel_I0_scaled" "gsl_sf_bessel_I0_scaled" "float"

external bessel_I0_scaled_e : float -> result
    = "ml_gsl_sf_bessel_I0_scaled_e"

external bessel_I1_scaled : float -> float
    = "ml_gsl_sf_bessel_I1_scaled" "gsl_sf_bessel_I1_scaled" "float"

external bessel_I1_scaled_e : float -> result
    = "ml_gsl_sf_bessel_I1_scaled_e"

external bessel_In : int -> float -> float
    = "ml_gsl_sf_bessel_In"

external bessel_In_e : int -> float -> result
    = "ml_gsl_sf_bessel_In_e"

external bessel_In_scaled_array : int -> float -> float array -> unit
    = "ml_gsl_sf_bessel_In_scaled_array"


external bessel_K0_scaled : float -> float
    = "ml_gsl_sf_bessel_K0_scaled" "gsl_sf_bessel_K0_scaled" "float"

external bessel_K0_scaled_e : float -> result
    = "ml_gsl_sf_bessel_K0_scaled_e"

external bessel_K1_scaled : float -> float
    = "ml_gsl_sf_bessel_K1_scaled" "gsl_sf_bessel_K1_scaled" "float"

external bessel_K1_scaled_e : float -> result
    = "ml_gsl_sf_bessel_K1_scaled_e"

external bessel_Kn : int -> float -> float
    = "ml_gsl_sf_bessel_Kn"

external bessel_Kn_e : int -> float -> result
    = "ml_gsl_sf_bessel_Kn_e"

external bessel_Kn_scaled_array : int -> float -> float array -> unit
    = "ml_gsl_sf_bessel_Kn_scaled_array"



external bessel_j0 : float -> float
    = "ml_gsl_sf_bessel_j0" "gsl_sf_bessel_j0" "float"

external bessel_j0_e : float -> result
    = "ml_gsl_sf_bessel_j0_e"

external bessel_j1 : float -> float
    = "ml_gsl_sf_bessel_j1" "gsl_sf_bessel_j1" "float"

external bessel_j1_e : float -> result
    = "ml_gsl_sf_bessel_j1_e"

external bessel_j2 : float -> float
    = "ml_gsl_sf_bessel_j2" "gsl_sf_bessel_j2" "float"

external bessel_j2_e : float -> result
    = "ml_gsl_sf_bessel_j2_e"

external bessel_jl : int -> float -> float
    = "ml_gsl_sf_bessel_jl"

external bessel_jl_e : int -> float -> result
    = "ml_gsl_sf_bessel_jl_e"

external bessel_jl_array : int -> float -> float array -> unit
    = "ml_gsl_sf_bessel_jl_array"


external  bessel_jl_steed_array : float -> float array -> unit 
    = "ml_gsl_sf_bessel_jl_steed_array"


external bessel_y0 : float -> float
    = "ml_gsl_sf_bessel_y0" "gsl_sf_bessel_y0" "float"

external bessel_y0_e : float -> result
    = "ml_gsl_sf_bessel_y0_e"

external bessel_y1 : float -> float
    = "ml_gsl_sf_bessel_y1" "gsl_sf_bessel_y1" "float"

external bessel_y1_e : float -> result
    = "ml_gsl_sf_bessel_y1_e"

external bessel_y2 : float -> float
    = "ml_gsl_sf_bessel_y2" "gsl_sf_bessel_y2" "float"

external bessel_y2_e : float -> result
    = "ml_gsl_sf_bessel_y2_e"

external bessel_yl : int -> float -> float
    = "ml_gsl_sf_bessel_yl"

external bessel_yl_e : int -> float -> result
    = "ml_gsl_sf_bessel_yl_e"

external bessel_yl_array : int -> float -> float array -> unit
    = "ml_gsl_sf_bessel_yl_array"


external bessel_i0_scaled : float -> float
    = "ml_gsl_sf_bessel_i0_scaled" "gsl_sf_bessel_i0_scaled" "float"

external bessel_i0_scaled_e : float -> result
    = "ml_gsl_sf_bessel_i0_scaled_e"

external bessel_i1_scaled : float -> float
    = "ml_gsl_sf_bessel_i1_scaled" "gsl_sf_bessel_i1_scaled" "float"

external bessel_i1_scaled_e : float -> result
    = "ml_gsl_sf_bessel_i1_scaled_e"

external bessel_il_scaled : int -> float -> float
    = "ml_gsl_sf_bessel_il_scaled"

external bessel_il_scaled_e : int -> float -> result
    = "ml_gsl_sf_bessel_il_scaled_e"

external bessel_il_scaled_array : int -> float -> float array -> unit
    = "ml_gsl_sf_bessel_il_scaled_array"


external bessel_k0_scaled : float -> float
    = "ml_gsl_sf_bessel_k0_scaled" "gsl_sf_bessel_k0_scaled" "float"

external bessel_k0_scaled_e : float -> result
    = "ml_gsl_sf_bessel_k0_scaled_e"

external bessel_k1_scaled : float -> float
    = "ml_gsl_sf_bessel_k1_scaled" "gsl_sf_bessel_k1_scaled" "float"

external bessel_k1_scaled_e : float -> result
    = "ml_gsl_sf_bessel_k1_scaled_e"

external bessel_kl_scaled : int -> float -> float
    = "ml_gsl_sf_bessel_kl_scaled"

external bessel_kl_scaled_e : int -> float -> result
    = "ml_gsl_sf_bessel_kl_scaled_e"

external bessel_kl_scaled_array : int -> float -> float array -> unit
    = "ml_gsl_sf_bessel_kl_scaled_array"



external bessel_Jnu : float -> float -> float
    = "ml_gsl_sf_bessel_Jnu" "gsl_sf_bessel_Jnu" "float"

external bessel_Jnu_e : float -> float -> result
    = "ml_gsl_sf_bessel_Jnu_e"


external  bessel_sequence_Jnu_e : float -> mode -> float array -> unit
    = "ml_gsl_sf_bessel_sequence_Jnu_e"


external bessel_Ynu : float -> float -> float
    = "ml_gsl_sf_bessel_Ynu" "gsl_sf_bessel_Ynu" "float"

external bessel_Ynu_e : float -> float -> result
    = "ml_gsl_sf_bessel_Ynu_e"


external bessel_Inu : float -> float -> float
    = "ml_gsl_sf_bessel_Inu" "gsl_sf_bessel_Inu" "float"

external bessel_Inu_e : float -> float -> result
    = "ml_gsl_sf_bessel_Inu_e"


external bessel_Inu_scaled : float -> float -> float
    = "ml_gsl_sf_bessel_Inu_scaled" "gsl_sf_bessel_Inu_scaled" "float"

external bessel_Inu_scaled_e : float -> float -> result
    = "ml_gsl_sf_bessel_Inu_scaled_e"


external bessel_Knu : float -> float -> float
    = "ml_gsl_sf_bessel_Knu" "gsl_sf_bessel_Knu" "float"

external bessel_Knu_e : float -> float -> result
    = "ml_gsl_sf_bessel_Knu_e"


external bessel_lnKnu : float -> float -> float
    = "ml_gsl_sf_bessel_lnKnu" "gsl_sf_bessel_lnKnu" "float"

external bessel_lnKnu_e : float -> float -> result
    = "ml_gsl_sf_bessel_lnKnu_e"


external bessel_Knu_scaled : float -> float -> float
    = "ml_gsl_sf_bessel_Knu_scaled" "gsl_sf_bessel_Knu_scaled" "float"

external bessel_Knu_scaled_e : float -> float -> result
    = "ml_gsl_sf_bessel_Knu_scaled_e"



external bessel_zero_J0 : int -> float
    = "ml_gsl_sf_bessel_zero_J0"

external bessel_zero_J0_e : int -> result
    = "ml_gsl_sf_bessel_zero_J0_e"


external bessel_zero_J1 : int -> float
    = "ml_gsl_sf_bessel_zero_J1"

external bessel_zero_J1_e : int -> result
    = "ml_gsl_sf_bessel_zero_J1_e"


external bessel_zero_Jnu : float -> int -> float
    = "ml_gsl_sf_bessel_zero_Jnu"

external bessel_zero_Jnu_e : float -> int -> result
    = "ml_gsl_sf_bessel_zero_Jnu_e"



(* CLAUSEN functions *)
external clausen : float -> float
    = "ml_gsl_sf_clausen" "gsl_sf_clausen" "float"

external clausen_e : float -> result
    = "ml_gsl_sf_clausen_e"



(* COULOMB functions *)
external hydrogenicR_1 : float -> float -> float
    = "ml_gsl_sf_hydrogenicR_1" "gsl_sf_hydrogenicR_1" "float"

external hydrogenicR_1_e : float -> float -> result
    = "ml_gsl_sf_hydrogenicR_1_e"


external hydrogenicR : int -> int -> float -> float -> float
    = "ml_gsl_sf_hydrogenicR"

external hydrogenicR_e : int -> int -> float -> float -> result
    = "ml_gsl_sf_hydrogenicR_e"


(* FIXME: COULOMB wave functions *)
external  coulomb_CL_e : float -> float -> result 
    = "ml_gsl_sf_coulomb_CL_e"


external  coulomb_CL_array : float -> float -> float array -> unit 
    = "ml_gsl_sf_coulomb_CL_array"



(* FIXME: coupling coeffs *)

(* DAWSON functions *)
external dawson : float -> float
    = "ml_gsl_sf_dawson" "gsl_sf_dawson" "float"

external dawson_e : float -> result
    = "ml_gsl_sf_dawson_e"



(* DEBYE functions *)
external debye_1 : float -> float
    = "ml_gsl_sf_debye_1" "gsl_sf_debye_1" "float"

external debye_1_e : float -> result
    = "ml_gsl_sf_debye_1_e"


external debye_2 : float -> float
    = "ml_gsl_sf_debye_2" "gsl_sf_debye_2" "float"

external debye_2_e : float -> result
    = "ml_gsl_sf_debye_2_e"


external debye_3 : float -> float
    = "ml_gsl_sf_debye_3" "gsl_sf_debye_3" "float"

external debye_3_e : float -> result
    = "ml_gsl_sf_debye_3_e"


external debye_4 : float -> float
    = "ml_gsl_sf_debye_4" "gsl_sf_debye_4" "float"

external debye_4_e : float -> result
    = "ml_gsl_sf_debye_4_e"



(* DILOGARITHM *)
external dilog : float -> float
    = "ml_gsl_sf_dilog" "gsl_sf_dilog" "float"

external dilog_e : float -> result
    = "ml_gsl_sf_dilog_e"


external  complex_dilog_e : float -> float -> result * result 
    = "ml_gsl_sf_complex_dilog_e"



(* ELEMENTARY operations *)
external  multiply_e : float -> float -> result 
    = "ml_gsl_sf_multiply_e"


external  multiply_err_e : x:float -> dx:float -> y:float -> dy:float -> result 
    = "ml_gsl_sf_multiply_err_e"



(* ELLIPTIC integrals *)
external ellint_Kcomp : float -> mode -> float
    = "ml_gsl_sf_ellint_Kcomp"

external ellint_Kcomp_e : float -> mode -> result
    = "ml_gsl_sf_ellint_Kcomp_e"


external ellint_Ecomp : float -> mode -> float
    = "ml_gsl_sf_ellint_Ecomp"

external ellint_Ecomp_e : float -> mode -> result
    = "ml_gsl_sf_ellint_Ecomp_e"


external ellint_F : float -> float -> mode -> float
    = "ml_gsl_sf_ellint_F"

external ellint_F_e : float -> float -> mode -> result
    = "ml_gsl_sf_ellint_F_e"


external ellint_E : float -> float -> mode -> float
    = "ml_gsl_sf_ellint_E"

external ellint_E_e : float -> float -> mode -> result
    = "ml_gsl_sf_ellint_E_e"


external ellint_P : float -> float -> float -> mode -> float
    = "ml_gsl_sf_ellint_P"

external ellint_P_e : float -> float -> float -> mode -> result
    = "ml_gsl_sf_ellint_P_e"


external ellint_D : float -> float -> float -> mode -> float
    = "ml_gsl_sf_ellint_D"

external ellint_D_e : float -> float -> float -> mode -> result
    = "ml_gsl_sf_ellint_D_e"


external ellint_RC : float -> float -> mode -> float
    = "ml_gsl_sf_ellint_RC"

external ellint_RC_e : float -> float -> mode -> result
    = "ml_gsl_sf_ellint_RC_e"


external ellint_RD : float -> float -> float -> mode -> float
    = "ml_gsl_sf_ellint_RD"

external ellint_RD_e : float -> float -> float -> mode -> result
    = "ml_gsl_sf_ellint_RD_e"


external ellint_RF : float -> float -> float -> mode -> float
    = "ml_gsl_sf_ellint_RF"

external ellint_RF_e : float -> float -> float -> mode -> result
    = "ml_gsl_sf_ellint_RF_e"


external ellint_RJ : float -> float -> float -> float -> mode -> float
    = "ml_gsl_sf_ellint_RJ"

external ellint_RJ_e : float -> float -> float -> float -> mode -> result
    = "ml_gsl_sf_ellint_RJ_e"


(* FIXME: elljac_e *)

(* ERROR function *)
external erf : float -> float
    = "ml_gsl_sf_erf" "gsl_sf_erf" "float"

external erf_e : float -> result
    = "ml_gsl_sf_erf_e"


external erfc : float -> float
    = "ml_gsl_sf_erfc" "gsl_sf_erfc" "float"

external erfc_e : float -> result
    = "ml_gsl_sf_erfc_e"


external log_erfc : float -> float
    = "ml_gsl_sf_log_erfc" "gsl_sf_log_erfc" "float"

external log_erfc_e : float -> result
    = "ml_gsl_sf_log_erfc_e"


external erf_Z : float -> float
    = "ml_gsl_sf_erf_Z" "gsl_sf_erf_Z" "float"

external erf_Z_e : float -> result
    = "ml_gsl_sf_erf_Z_e"


external erf_Q : float -> float
    = "ml_gsl_sf_erf_Q" "gsl_sf_erf_Q" "float"

external erf_Q_e : float -> result
    = "ml_gsl_sf_erf_Q_e"



(* EXPONENTIAL functions *)
external exp : float -> float
    = "ml_gsl_sf_exp" "gsl_sf_exp" "float"

external exp_e : float -> result
    = "ml_gsl_sf_exp_e"


external  exp_e10 : float -> result_e10 
    = "ml_gsl_sf_exp_e10_e"


external exp_mult : float -> float -> float
    = "ml_gsl_sf_exp_mult" "gsl_sf_exp_mult" "float"

external exp_mult_e : float -> float -> result
    = "ml_gsl_sf_exp_mult_e"


external  exp_mult_e10 : float -> float -> result_e10 
    = "ml_gsl_sf_exp_mult_e10_e"



external expm1 : float -> float
    = "ml_gsl_sf_expm1" "gsl_sf_expm1" "float"

external expm1_e : float -> result
    = "ml_gsl_sf_expm1_e"


external exprel : float -> float
    = "ml_gsl_sf_exprel" "gsl_sf_exprel" "float"

external exprel_e : float -> result
    = "ml_gsl_sf_exprel_e"


external exprel_2 : float -> float
    = "ml_gsl_sf_exprel_2" "gsl_sf_exprel_2" "float"

external exprel_2_e : float -> result
    = "ml_gsl_sf_exprel_2_e"


external exprel_n : int -> float -> float
    = "ml_gsl_sf_exprel_n"

external exprel_n_e : int -> float -> result
    = "ml_gsl_sf_exprel_n_e"


external  exp_err_e : x:float -> dx:float -> result 
    = "ml_gsl_sf_exp_err_e"


external  exp_err_e10 : x:float -> dx:float -> result_e10 
    = "ml_gsl_sf_exp_err_e10_e"


external  exp_mult_err_e : x:float -> dx:float -> y:float -> dy:float -> result 
    = "ml_gsl_sf_exp_mult_err_e"


external  exp_mult_err_e10_e : x:float -> dx:float -> y:float -> dy:float -> result_e10 
    = "ml_gsl_sf_exp_mult_err_e10_e"



(* EXPONENTIAL integrals *)
external expint_E1 : float -> float
    = "ml_gsl_sf_expint_E1" "gsl_sf_expint_E1" "float"

external expint_E1_e : float -> result
    = "ml_gsl_sf_expint_E1_e"


external expint_E2 : float -> float
    = "ml_gsl_sf_expint_E2" "gsl_sf_expint_E2" "float"

external expint_E2_e : float -> result
    = "ml_gsl_sf_expint_E2_e"


external expint_E1_scaled : float -> float
    = "ml_gsl_sf_expint_E1_scaled" "gsl_sf_expint_E1_scaled" "float"

external expint_E1_scaled_e : float -> result
    = "ml_gsl_sf_expint_E1_scaled_e"


external expint_E2_scaled : float -> float
    = "ml_gsl_sf_expint_E2_scaled" "gsl_sf_expint_E2_scaled" "float"

external expint_E2_scaled_e : float -> result
    = "ml_gsl_sf_expint_E2_scaled_e"


external expint_Ei : float -> float
    = "ml_gsl_sf_expint_Ei" "gsl_sf_expint_Ei" "float"

external expint_Ei_e : float -> result
    = "ml_gsl_sf_expint_Ei_e"


external expint_Ei_scaled : float -> float
    = "ml_gsl_sf_expint_Ei_scaled" "gsl_sf_expint_Ei_scaled" "float"

external expint_Ei_scaled_e : float -> result
    = "ml_gsl_sf_expint_Ei_scaled_e"


external  shi : float -> float 
    = "ml_gsl_sf_Shi"


external  chi : float -> float 
    = "ml_gsl_sf_Chi"


external expint_3 : float -> float
    = "ml_gsl_sf_expint_3" "gsl_sf_expint_3" "float"

external expint_3_e : float -> result
    = "ml_gsl_sf_expint_3_e"


external  si : float -> float 
    = "ml_gsl_sf_Si"


external  ci : float -> float 
    = "ml_gsl_sf_Ci"


external atanint : float -> float
    = "ml_gsl_sf_atanint" "gsl_sf_atanint" "float"

external atanint_e : float -> result
    = "ml_gsl_sf_atanint_e"



(* fermi-dirac *)
external fermi_dirac_m1 : float -> float
    = "ml_gsl_sf_fermi_dirac_m1" "gsl_sf_fermi_dirac_m1" "float"

external fermi_dirac_m1_e : float -> result
    = "ml_gsl_sf_fermi_dirac_m1_e"


external fermi_dirac_0 : float -> float
    = "ml_gsl_sf_fermi_dirac_0" "gsl_sf_fermi_dirac_0" "float"

external fermi_dirac_0_e : float -> result
    = "ml_gsl_sf_fermi_dirac_0_e"


external fermi_dirac_1 : float -> float
    = "ml_gsl_sf_fermi_dirac_1" "gsl_sf_fermi_dirac_1" "float"

external fermi_dirac_1_e : float -> result
    = "ml_gsl_sf_fermi_dirac_1_e"


external fermi_dirac_2 : float -> float
    = "ml_gsl_sf_fermi_dirac_2" "gsl_sf_fermi_dirac_2" "float"

external fermi_dirac_2_e : float -> result
    = "ml_gsl_sf_fermi_dirac_2_e"


external fermi_dirac_int : int -> float -> float
    = "ml_gsl_sf_fermi_dirac_int"

external fermi_dirac_int_e : int -> float -> result
    = "ml_gsl_sf_fermi_dirac_int_e"


external fermi_dirac_mhalf : float -> float
    = "ml_gsl_sf_fermi_dirac_mhalf" "gsl_sf_fermi_dirac_mhalf" "float"

external fermi_dirac_mhalf_e : float -> result
    = "ml_gsl_sf_fermi_dirac_mhalf_e"


external fermi_dirac_half : float -> float
    = "ml_gsl_sf_fermi_dirac_half" "gsl_sf_fermi_dirac_half" "float"

external fermi_dirac_half_e : float -> result
    = "ml_gsl_sf_fermi_dirac_half_e"


external fermi_dirac_3half : float -> float
    = "ml_gsl_sf_fermi_dirac_3half" "gsl_sf_fermi_dirac_3half" "float"

external fermi_dirac_3half_e : float -> result
    = "ml_gsl_sf_fermi_dirac_3half_e"


external fermi_dirac_inc_0 : float -> float -> float
    = "ml_gsl_sf_fermi_dirac_inc_0" "gsl_sf_fermi_dirac_inc_0" "float"

external fermi_dirac_inc_0_e : float -> float -> result
    = "ml_gsl_sf_fermi_dirac_inc_0_e"



(* Gamma function *)
external gamma : float -> float
    = "ml_gsl_sf_gamma" "gsl_sf_gamma" "float"

external gamma_e : float -> result
    = "ml_gsl_sf_gamma_e"


external lngamma : float -> float
    = "ml_gsl_sf_lngamma" "gsl_sf_lngamma" "float"

external lngamma_e : float -> result
    = "ml_gsl_sf_lngamma_e"


external  lngamma_sgn_e : float -> result * float 
    = "ml_gsl_sf_lngamma_sgn_e"


external gammastar : float -> float
    = "ml_gsl_sf_gammastar" "gsl_sf_gammastar" "float"

external gammastar_e : float -> result
    = "ml_gsl_sf_gammastar_e"


external gammainv : float -> float
    = "ml_gsl_sf_gammainv" "gsl_sf_gammainv" "float"

external gammainv_e : float -> result
    = "ml_gsl_sf_gammainv_e"


external  lngamma_complex_e : float -> float -> result * result 
    = "ml_gsl_sf_lngamma_complex_e"


external taylorcoeff : int -> float -> float
    = "ml_gsl_sf_taylorcoeff"

external taylorcoeff_e : int -> float -> result
    = "ml_gsl_sf_taylorcoeff_e"


external fact : int -> float
    = "ml_gsl_sf_fact"

external fact_e : int -> result
    = "ml_gsl_sf_fact_e"


external doublefact : int -> float
    = "ml_gsl_sf_doublefact"

external doublefact_e : int -> result
    = "ml_gsl_sf_doublefact_e"


external lnfact : int -> float
    = "ml_gsl_sf_lnfact"

external lnfact_e : int -> result
    = "ml_gsl_sf_lnfact_e"


external lndoublefact : int -> float
    = "ml_gsl_sf_lndoublefact"

external lndoublefact_e : int -> result
    = "ml_gsl_sf_lndoublefact_e"


external choose : int -> int -> float
    = "ml_gsl_sf_choose"

external choose_e : int -> int -> result
    = "ml_gsl_sf_choose_e"


external lnchoose : int -> int -> float
    = "ml_gsl_sf_lnchoose"

external lnchoose_e : int -> int -> result
    = "ml_gsl_sf_lnchoose_e"


external poch : float -> float -> float
    = "ml_gsl_sf_poch" "gsl_sf_poch" "float"

external poch_e : float -> float -> result
    = "ml_gsl_sf_poch_e"


external lnpoch : float -> float -> float
    = "ml_gsl_sf_lnpoch" "gsl_sf_lnpoch" "float"

external lnpoch_e : float -> float -> result
    = "ml_gsl_sf_lnpoch_e"


external  lnpoch_sgn_e : float -> float -> result * float 
    = "ml_gsl_sf_lngamma_sgn_e"


external pochrel : float -> float -> float
    = "ml_gsl_sf_pochrel" "gsl_sf_pochrel" "float"

external pochrel_e : float -> float -> result
    = "ml_gsl_sf_pochrel_e"


external gamma_inc_Q : float -> float -> float
    = "ml_gsl_sf_gamma_inc_Q" "gsl_sf_gamma_inc_Q" "float"

external gamma_inc_Q_e : float -> float -> result
    = "ml_gsl_sf_gamma_inc_Q_e"


external gamma_inc_P : float -> float -> float
    = "ml_gsl_sf_gamma_inc_P" "gsl_sf_gamma_inc_P" "float"

external gamma_inc_P_e : float -> float -> result
    = "ml_gsl_sf_gamma_inc_P_e"


external beta : float -> float -> float
    = "ml_gsl_sf_beta" "gsl_sf_beta" "float"

external beta_e : float -> float -> result
    = "ml_gsl_sf_beta_e"


external lnbeta : float -> float -> float
    = "ml_gsl_sf_lnbeta" "gsl_sf_lnbeta" "float"

external lnbeta_e : float -> float -> result
    = "ml_gsl_sf_lnbeta_e"


external beta_inc : float -> float -> float -> float
    = "ml_gsl_sf_beta_inc" "gsl_sf_beta_inc" "float"

external beta_inc_e : float -> float -> float -> result
    = "ml_gsl_sf_beta_inc_e"



(* GEGENBAUER functions *)
external gegenpoly_1 : float -> float -> float
    = "ml_gsl_sf_gegenpoly_1" "gsl_sf_gegenpoly_1" "float"

external gegenpoly_1_e : float -> float -> result
    = "ml_gsl_sf_gegenpoly_1_e"


external gegenpoly_2 : float -> float -> float
    = "ml_gsl_sf_gegenpoly_2" "gsl_sf_gegenpoly_2" "float"

external gegenpoly_2_e : float -> float -> result
    = "ml_gsl_sf_gegenpoly_2_e"


external gegenpoly_3 : float -> float -> float
    = "ml_gsl_sf_gegenpoly_3" "gsl_sf_gegenpoly_3" "float"

external gegenpoly_3_e : float -> float -> result
    = "ml_gsl_sf_gegenpoly_3_e"


external gegenpoly_n : int -> float -> float -> float
    = "ml_gsl_sf_gegenpoly_n"

external gegenpoly_n_e : int -> float -> float -> result
    = "ml_gsl_sf_gegenpoly_n_e"


external  gegenpoly_array : float -> float -> float array -> unit 
    = "ml_gsl_sf_gegenpoly_array"



(* HYPERGEOMETRIC functions *)
(* FIXME *)

(* LAGUERRE functions *)
external laguerre_1 : float -> float -> float
    = "ml_gsl_sf_laguerre_1" "gsl_sf_laguerre_1" "float"

external laguerre_1_e : float -> float -> result
    = "ml_gsl_sf_laguerre_1_e"


external laguerre_2 : float -> float -> float
    = "ml_gsl_sf_laguerre_2" "gsl_sf_laguerre_2" "float"

external laguerre_2_e : float -> float -> result
    = "ml_gsl_sf_laguerre_2_e"


external laguerre_3 : float -> float -> float
    = "ml_gsl_sf_laguerre_3" "gsl_sf_laguerre_3" "float"

external laguerre_3_e : float -> float -> result
    = "ml_gsl_sf_laguerre_3_e"


external laguerre_n : int -> float -> float -> float
    = "ml_gsl_sf_laguerre_n"

external laguerre_n_e : int -> float -> float -> result
    = "ml_gsl_sf_laguerre_n_e"



(* LAMBERT W functions *)
external lambert_W0 : float -> float
    = "ml_gsl_sf_lambert_W0" "gsl_sf_lambert_W0" "float"

external lambert_W0_e : float -> result
    = "ml_gsl_sf_lambert_W0_e"


external lambert_Wm1 : float -> float
    = "ml_gsl_sf_lambert_Wm1" "gsl_sf_lambert_Wm1" "float"

external lambert_Wm1_e : float -> result
    = "ml_gsl_sf_lambert_Wm1_e"



(* LEGENDRE functions *)
external legendre_P1 : float -> float
    = "ml_gsl_sf_legendre_P1" "gsl_sf_legendre_P1" "float"

external legendre_P1_e : float -> result
    = "ml_gsl_sf_legendre_P1_e"


external legendre_P2 : float -> float
    = "ml_gsl_sf_legendre_P2" "gsl_sf_legendre_P2" "float"

external legendre_P2_e : float -> result
    = "ml_gsl_sf_legendre_P2_e"


external legendre_P3 : float -> float
    = "ml_gsl_sf_legendre_P3" "gsl_sf_legendre_P3" "float"

external legendre_P3_e : float -> result
    = "ml_gsl_sf_legendre_P3_e"


external legendre_Pl : int -> float -> float
    = "ml_gsl_sf_legendre_Pl"

external legendre_Pl_e : int -> float -> result
    = "ml_gsl_sf_legendre_Pl_e"


external  legendre_Pl_array : float -> float array -> unit 
    = "ml_gsl_sf_legendre_Pl_array"


external legendre_Q0 : float -> float
    = "ml_gsl_sf_legendre_Q0" "gsl_sf_legendre_Q0" "float"

external legendre_Q0_e : float -> result
    = "ml_gsl_sf_legendre_Q0_e"


external legendre_Q1 : float -> float
    = "ml_gsl_sf_legendre_Q1" "gsl_sf_legendre_Q1" "float"

external legendre_Q1_e : float -> result
    = "ml_gsl_sf_legendre_Q1_e"


external legendre_Ql : int -> float -> float
    = "ml_gsl_sf_legendre_Ql"

external legendre_Ql_e : int -> float -> result
    = "ml_gsl_sf_legendre_Ql_e"



(* LOGARITHM and related functions *)
external log : float -> float
    = "ml_gsl_sf_log" "gsl_sf_log" "float"

external log_e : float -> result
    = "ml_gsl_sf_log_e"


external log_abs : float -> float
    = "ml_gsl_sf_log_abs" "gsl_sf_log_abs" "float"

external log_abs_e : float -> result
    = "ml_gsl_sf_log_abs_e"


external  log_complex_e : float -> float -> result * result 
    = "ml_gsl_sf_complex_log_e"


external log_1plusx : float -> float
    = "ml_gsl_sf_log_1plusx" "gsl_sf_log_1plusx" "float"

external log_1plusx_e : float -> result
    = "ml_gsl_sf_log_1plusx_e"


external log_1plusx_mx : float -> float
    = "ml_gsl_sf_log_1plusx_mx" "gsl_sf_log_1plusx_mx" "float"

external log_1plusx_mx_e : float -> result
    = "ml_gsl_sf_log_1plusx_mx_e"



(* POWER function *)
external pow_int : float -> int -> float
    = "ml_gsl_sf_pow_int"

external pow_int_e : float -> int -> result
    = "ml_gsl_sf_pow_int_e"



(* PSI function *)
external psi_int : int -> float
    = "ml_gsl_sf_psi_int"

external psi_int_e : int -> result
    = "ml_gsl_sf_psi_int_e"


external psi : float -> float
    = "ml_gsl_sf_psi" "gsl_sf_psi" "float"

external psi_e : float -> result
    = "ml_gsl_sf_psi_e"


external psi_1piy : float -> float
    = "ml_gsl_sf_psi_1piy" "gsl_sf_psi_1piy" "float"

external psi_1piy_e : float -> result
    = "ml_gsl_sf_psi_1piy_e"


external psi_1_int : int -> float
    = "ml_gsl_sf_psi_1_int"

external psi_1_int_e : int -> result
    = "ml_gsl_sf_psi_1_int_e"


external psi_n : int -> float -> float
    = "ml_gsl_sf_psi_n"

external psi_n_e : int -> float -> result
    = "ml_gsl_sf_psi_n_e"



(* SYNCHROTRON functions *)
external synchrotron_1 : float -> float
    = "ml_gsl_sf_synchrotron_1" "gsl_sf_synchrotron_1" "float"

external synchrotron_1_e : float -> result
    = "ml_gsl_sf_synchrotron_1_e"


external synchrotron_2 : float -> float
    = "ml_gsl_sf_synchrotron_2" "gsl_sf_synchrotron_2" "float"

external synchrotron_2_e : float -> result
    = "ml_gsl_sf_synchrotron_2_e"



(* TRANSPORT functions *)
external transport_2 : float -> float
    = "ml_gsl_sf_transport_2" "gsl_sf_transport_2" "float"

external transport_2_e : float -> result
    = "ml_gsl_sf_transport_2_e"


external transport_3 : float -> float
    = "ml_gsl_sf_transport_3" "gsl_sf_transport_3" "float"

external transport_3_e : float -> result
    = "ml_gsl_sf_transport_3_e"


external transport_4 : float -> float
    = "ml_gsl_sf_transport_4" "gsl_sf_transport_4" "float"

external transport_4_e : float -> result
    = "ml_gsl_sf_transport_4_e"


external transport_5 : float -> float
    = "ml_gsl_sf_transport_5" "gsl_sf_transport_5" "float"

external transport_5_e : float -> result
    = "ml_gsl_sf_transport_5_e"



(* TRIGONOMETRIC functions *)
external sin : float -> float
    = "ml_gsl_sf_sin" "gsl_sf_sin" "float"

external sin_e : float -> result
    = "ml_gsl_sf_sin_e"


external cos : float -> float
    = "ml_gsl_sf_cos" "gsl_sf_cos" "float"

external cos_e : float -> result
    = "ml_gsl_sf_cos_e"


external hypot : float -> float
    = "ml_gsl_sf_hypot" "gsl_sf_hypot" "float"

external hypot_e : float -> result
    = "ml_gsl_sf_hypot_e"


external sinc : float -> float
    = "ml_gsl_sf_sinc" "gsl_sf_sinc" "float"

external sinc_e : float -> result
    = "ml_gsl_sf_sinc_e"


external  complex_sin_e : float -> float -> result * result 
    = "ml_gsl_sf_complex_sin_e"


external  complex_cos_e : float -> float -> result * result 
    = "ml_gsl_sf_complex_cos_e"


external  complex_logsin_e : float -> float -> result * result 
    = "ml_gsl_sf_complex_logsin_e"


external lnsinh : float -> float
    = "ml_gsl_sf_lnsinh" "gsl_sf_lnsinh" "float"

external lnsinh_e : float -> result
    = "ml_gsl_sf_lnsinh_e"


external lncosh : float -> float
    = "ml_gsl_sf_lncosh" "gsl_sf_lncosh" "float"

external lncosh_e : float -> result
    = "ml_gsl_sf_lncosh_e"


external  rect_of_polar : r:float -> theta:float -> result * result 
    = "ml_gsl_sf_polar_to_rect"


external  polar_of_rect : x:float -> y:float -> result * result 
    = "ml_gsl_sf_rect_to_polar"


external  angle_restrict_symm : float -> float 
    = "ml_gsl_sf_angle_restrict_symm"


external  angle_restrict_pos : float -> float 
    = "ml_gsl_sf_angle_restrict_pos"


external  sin_err_e : float -> dx:float -> result 
    = "ml_gsl_sf_sin_err_e"


external  cos_err_e : float -> dx:float -> result 
    = "ml_gsl_sf_cos_err_e"



(* ZETA functions *)
external zeta_int : int -> float
    = "ml_gsl_sf_zeta_int"

external zeta_int_e : int -> result
    = "ml_gsl_sf_zeta_int_e"


external zeta : float -> float
    = "ml_gsl_sf_zeta" "gsl_sf_zeta" "float"

external zeta_e : float -> result
    = "ml_gsl_sf_zeta_e"


external hzeta : float -> float -> float
    = "ml_gsl_sf_hzeta" "gsl_sf_hzeta" "float"

external hzeta_e : float -> float -> result
    = "ml_gsl_sf_hzeta_e"


external eta_int : int -> float
    = "ml_gsl_sf_eta_int"

external eta_int_e : int -> result
    = "ml_gsl_sf_eta_int_e"


external eta : float -> float
    = "ml_gsl_sf_eta" "gsl_sf_eta" "float"

external eta_e : float -> result
    = "ml_gsl_sf_eta_e"




