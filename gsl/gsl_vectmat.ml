(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002 - Olivier Andrieu                     *)
(* distributed under the terms of the GPL version 2         *)


type vec = [
  | `V  of Gsl_vector.vector
  | `VF of Gsl_vector_flat.vector ]

let vec_convert ?(protect=false) = function
  | `A arr when protect ->
      `VF (Gsl_vector_flat.of_array arr)
  | `A arr ->
      `VF (Gsl_vector_flat.view_array arr)
  | `VF vec when protect ->
      `VF (Gsl_vector_flat.copy vec)
  | `VF vec as v ->
      v
  | `V vec when protect ->
      `V (Gsl_vector.copy vec)
  | `V vec as v ->
      v

type mat = [
  | `M  of Gsl_matrix.matrix
  | `MF of Gsl_matrix_flat.matrix ]

let mat_convert ?(protect=false) = function
  | `M mat when protect ->
      `M (Gsl_matrix.copy mat)
  | `M mat as m ->
      m
  | `MF mat when protect ->
      `MF (Gsl_matrix_flat.copy mat)
  | `MF mat as m ->
      m
  | `A (arr, d1, d2) when protect ->
      `MF (Gsl_matrix_flat.of_array arr d1 d2)
  | `A (arr, d1, d2) ->
      `MF (Gsl_matrix_flat.view_array arr d1 d2)
  | `AA arr ->
      `MF (Gsl_matrix_flat.of_arrays arr)

let mat_flat ?(protect=false) = function
  | `M mat ->
      let (d1, d2) = Gsl_matrix.dims mat in
      let arr = Gsl_matrix.to_array mat in
      Gsl_matrix_flat.view_array arr d1 d2
  | `MF mat when protect ->
      Gsl_matrix_flat.copy mat
  | `MF mat ->
      mat
  | `A (arr, d1, d2) when protect ->
      Gsl_matrix_flat.of_array arr d1 d2
  | `A (arr, d1, d2) ->
      Gsl_matrix_flat.view_array arr d1 d2
  | `AA arr ->
      Gsl_matrix_flat.of_arrays arr


(* Complex values *)

type cvec = [
  | `CV  of Gsl_vector_complex.vector
  | `CVF of Gsl_vector_complex_flat.vector ]

type cmat = [
  | `CM  of Gsl_matrix_complex.matrix
  | `CMF of Gsl_matrix_complex_flat.matrix ]

let cmat_convert ?(protect=false) = function
  | `CM mat when protect ->
      `CM (Gsl_matrix_complex.copy mat)
  | `CM mat as m ->
      m
  | `CMF mat when protect ->
      `CMF (Gsl_matrix_complex_flat.copy mat)
  | `CMF mat as m ->
      m
  | `CA (arr, d1, d2) when protect ->
      `CMF (Gsl_matrix_complex_flat.of_complex_array arr d1 d2)
  | `CA (arr, d1, d2) ->
      `CMF (Gsl_matrix_complex_flat.view_complex_array arr d1 d2)



(* Generic vector operations *)

let length = function
  | `VF v -> Gsl_vector_flat.length v
  | `V  v -> Gsl_vector.length v
  | `CV  v -> Gsl_vector_complex.length v
  | `CVF  v -> Gsl_vector_complex_flat.length v

let to_array = function
  | `VF v -> Gsl_vector_flat.to_array v
  | `V  v -> Gsl_vector.to_array v

let v_copy = function
  | `VF v -> `VF (Gsl_vector_flat.copy v)
  | `V  v -> `V (Gsl_vector.copy v)

let subvector v ~off ~len = 
  match v with
  | `VF v -> `VF (Gsl_vector_flat.subvector v ~off ~len)
  | `V  v -> `V  (Gsl_vector.subvector v ~off ~len)

external v_memcpy : [< vec]-> [< vec]-> unit 
    = "ml_gsl_vector_memcpy"

external v_add : [< vec]-> [< vec]-> unit 
    = "ml_gsl_vector_add"
external v_sub : [< vec]-> [< vec]-> unit 
    = "ml_gsl_vector_sub"
external v_mul : [< vec]-> [< vec]-> unit 
    = "ml_gsl_vector_mul"
external v_div : [< vec]-> [< vec]-> unit 
    = "ml_gsl_vector_div"
external v_scale : [< vec]-> float -> unit 
    = "ml_gsl_vector_scale"
external v_add_constant : [< vec]-> float -> unit
    = "ml_gsl_vector_add_constant"
external v_is_null : [< vec]-> bool
    = "ml_gsl_vector_isnull"

external v_max : [< vec]-> float
    = "ml_gsl_vector_max"
external v_min : [< vec]-> float
    = "ml_gsl_vector_min"
external v_minmax : [< vec]-> float * float
    = "ml_gsl_vector_minmax"
external v_max_index : [< vec]-> int
    = "ml_gsl_vector_maxindex"
external v_min_index : [< vec]-> int
    = "ml_gsl_vector_minindex"
external v_minmax_index : [< vec]-> int * int
    = "ml_gsl_vector_minmaxindex"


(* Generic matrix operations *)

let dims = function
  | `MF v -> Gsl_matrix_flat.dims v
  | `M v  -> Gsl_matrix.dims v
  | `CM m -> Gsl_matrix_complex.dims m
  | `CMF m -> Gsl_matrix_complex_flat.dims m

let to_arrays = function
  | `M mat  -> Gsl_matrix.to_arrays mat
  | `MF mat -> Gsl_matrix_flat.to_arrays mat

let tmp mat = 
  let (d1, d2) = dims mat in
  `M (Gsl_matrix.create d1 d2)

let m_copy = function
  | `MF v -> `MF (Gsl_matrix_flat.copy v)
  | `M  v -> `M (Gsl_matrix.copy v)

external m_memcpy : [< mat] -> [< mat] -> unit 
    = "ml_gsl_matrix_memcpy"

external m_add : [< mat] -> [< mat] -> unit 
    = "ml_gsl_matrix_add"
external m_sub : [< mat] -> [< mat] -> unit 
    = "ml_gsl_matrix_sub"
external m_mul : [< mat] -> [< mat] -> unit 
    = "ml_gsl_matrix_mul"
external m_div : [< mat] -> [< mat] -> unit 
    = "ml_gsl_matrix_div"
external m_scale : [< mat] -> float -> unit 
    = "ml_gsl_matrix_scale"
external m_add_constant : [< mat] -> float -> unit
    = "ml_gsl_matrix_add_constant"
external m_add_diagonal : [< mat] -> float -> unit
    = "ml_gsl_matrix_add_diagonal"
external m_is_null : [< mat] -> bool
    = "ml_gsl_matrix_isnull"

external swap_rows : [< mat] -> int -> int -> unit 
    = "ml_gsl_matrix_swap_rows"
external swap_columns : [< mat] -> int -> int -> unit
    = "ml_gsl_matrix_swap_columns"
external swap_rowcol : [< mat] -> int -> int -> unit
    = "ml_gsl_matrix_swap_rowcol"
external transpose : [< mat] -> [< mat] -> unit
    = "ml_gsl_matrix_transpose_memcpy"
external transpose_in_place : [< mat] -> unit
    = "ml_gsl_matrix_transpose"


let is_null x = 
  match x with
  | `VF _ | `V _ as v -> v_is_null v
  | `MF _ | `M _ as m -> m_is_null m

let scale x c = 
  match x with
  | `VF _ | `V _ as v -> v_scale v c
  | `MF _ | `M _ as m -> m_scale m c

let add_constant x c = 
  match x with
  | `VF _ | `V _ as v -> v_add_constant v c
  | `MF _ | `M _ as m -> m_add_constant m c
