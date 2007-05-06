(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002-2005 - Olivier Andrieu                *)
(* distributed under the terms of the GPL version 2         *)


type result = {
    res : float ;
    err : float ; }

type result_e10 = {
    res_e10 : float ;
    err_e10 : float ;
    e10     : int ;
  } 

type mode = 
  | DOUBLE
  | SIMPLE
  | APPROX

external smash : result_e10 -> result 
    = "ml_gsl_sf_result_smash_e"


type gsl_fun = float -> float
type gsl_fun_fdf = {
   f   : float -> float ;
   df  : float -> float ;
   fdf : float -> float * float ;
  } 

type monte_fun = float array -> float

open Gsl_vector

type multi_fun = x:vector -> f:vector -> unit
type multi_fun_fdf = {
    multi_f   : x:vector -> f:vector -> unit ;
    multi_df  : x:vector -> j:Gsl_matrix.matrix -> unit ;
    multi_fdf : x:vector -> f:vector -> j:Gsl_matrix.matrix -> unit ;
  } 

type multim_fun = x:vector -> float
type multim_fun_fdf = {
    multim_f   : x:vector -> float ;
    multim_df  : x:vector -> g:vector -> unit ;
    multim_fdf : x:vector -> g:vector -> float ;
  }
