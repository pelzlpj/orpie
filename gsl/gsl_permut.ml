(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002 - Olivier Andrieu                     *)
(* distributed under the terms of the GPL version 2         *)


open Bigarray

type permut = 
    (int, int_elt, c_layout) Array1.t

let of_array arr =
  Array1.of_array int c_layout arr

let to_array perm = 
  let len = Array1.dim perm in
  Array.init len (Array1.get perm)

external init : permut -> unit
    = "ml_gsl_permutation_init"

let create len = 
  Array1.create int c_layout len

let make len = 
  let p = create len in
  init p ;
  p

let swap p i j =
  let tmp_i = p.{i} in
  let tmp_j = p.{j} in
  p.{i} <- tmp_j ;
  p.{j} <- tmp_i

let size = 
  Array1.dim

external _valid : permut -> bool
    = "ml_gsl_permutation_valid"

let valid p =
  try _valid p
  with Gsl_error.Gsl_exn (Gsl_error.FAILURE, _) -> false

external reverse : permut -> unit
    = "ml_gsl_permutation_reverse"

external _inverse : src:permut -> dst:permut -> unit
    = "ml_gsl_permutation_inverse"

let inverse p =
  let i = create (size p) in
  _inverse p i ;
  i

external next : permut -> unit
    = "ml_gsl_permutation_next"

external prev : permut -> unit
    = "ml_gsl_permutation_prev"

external permute : permut -> 'a array -> unit
    = "ml_gsl_permute"
external permute_barr : permut -> ('a, 'b, 'c) Bigarray.Array1.t -> unit
    = "ml_gsl_permute_barr"
external permute_complex : permut -> Gsl_complex.complex_array -> unit
    = "ml_gsl_permute_complex"

external permute_inverse : permut -> 'a array -> unit
    = "ml_gsl_permute_inverse"
external permute_inverse_barr : permut -> 
  ('a, 'b, 'c) Bigarray.Array1.t -> unit
    = "ml_gsl_permute_inverse_barr"
external permute_inverse_complex : permut -> Gsl_complex.complex_array -> unit
    = "ml_gsl_permute_inverse_complex"
