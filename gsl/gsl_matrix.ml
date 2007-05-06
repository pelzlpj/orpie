(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002-2005 - Olivier Andrieu                *)
(* distributed under the terms of the GPL version 2         *)


open Bigarray

type double_mat_bigarr = 
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

type matrix = double_mat_bigarr

let create ?init dimx dimy =
 let barr = Array2.create float64 c_layout dimx dimy in
 begin match init with
 | None -> ()
 | Some x -> Array2.fill barr x
 end ;
 barr

let dims mat = 
  (Array2.dim1 mat, Array2.dim2 mat)

let of_array arr dim1 dim2 =
  let mat = create dim1 dim2 in
  for i=0 to pred dim1 do
    for j=0 to pred dim2 do
      mat.{i,j} <- arr.(dim2*i+j)
    done
  done ;
  mat
  
let of_arrays arr =
  Array2.of_array float64 c_layout arr

let to_array (mat : matrix) = 
  let d1 = Array2.dim1 mat in
  let d2 = Array2.dim2 mat in
  Array.init (d1*d2) (fun i -> mat.{i/d2, i mod d2})

let to_arrays (mat : matrix) = 
  let d1 = Array2.dim1 mat in
  let d2 = Array2.dim2 mat in
  let a = Array.init d1 (fun _ -> Array.make d2 0.) in
  for i=0 to pred d1 do
    for j=0 to pred d2 do
      a.(i).(j) <- mat.{i,j}
    done
  done ;
  a

let get (m : matrix) i j = Array2.get m i j
let set (m : matrix) i j x = Array2.set m i j x
let set_all = Array2.fill
let set_zero m = set_all m 0.
let set_id m = 
  set_zero m ;
  for i=0 to pred (min (Array2.dim1 m) (Array2.dim2 m)) do
    set m i i 1.
  done
  
let memcpy ~src ~dst = 
  Array2.blit src dst

let copy m =
  let m' = create (Array2.dim1 m) (Array2.dim2 m) in
  Array2.blit m m' ;
  m'

external add : matrix -> matrix -> unit = "ml_gsl_matrix_add"
external sub : matrix -> matrix -> unit = "ml_gsl_matrix_sub"
external mul_elements : matrix -> matrix -> unit = "ml_gsl_matrix_mul"
external div_elements : matrix -> matrix -> unit = "ml_gsl_matrix_div"
external scale : matrix -> float -> unit = "ml_gsl_matrix_scale"
external add_constant : matrix -> float -> unit = "ml_gsl_matrix_add_constant"
external add_diagonal : matrix -> float -> unit = "ml_gsl_matrix_add_diagonal"
external is_null : matrix -> bool = "ml_gsl_matrix_isnull"
external swap_rows : matrix -> int -> int -> unit = "ml_gsl_matrix_swap_rows"
external swap_columns : matrix -> int -> int -> unit = "ml_gsl_matrix_swap_columns"
external swap_rowcol : matrix -> int -> int -> unit = "ml_gsl_matrix_swap_rowcol"
external transpose : matrix -> matrix -> unit = "ml_gsl_matrix_transpose_memcpy"
external transpose_in_place : matrix -> unit = "ml_gsl_matrix_transpose"

let row =
  Array2.slice_left



module Single = 
  struct
  type float_mat_bigarr = 
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array2.t

  type matrix = float_mat_bigarr

  let create ?init dimx dimy =
    let barr = Array2.create float32 c_layout dimx dimy in
    begin match init with
    | None -> ()
    | Some x -> Array2.fill barr x
    end ;
    barr

  let dims = dims

  let of_array arr dim1 dim2 =
    let mat = create dim1 dim2 in
    for i=0 to pred dim1 do
      for j=0 to pred dim2 do
	mat.{i,j} <- arr.(dim2*i+j)
      done
    done ;
    mat
      
  let of_arrays arr =
    Array2.of_array float32 c_layout arr

  let to_array (mat : matrix) = 
    let d1 = Array2.dim1 mat in
    let d2 = Array2.dim2 mat in
    Array.init (d1*d2) (fun i -> mat.{i/d2, i mod d2})

  let to_arrays (mat : matrix) = 
    let d1 = Array2.dim1 mat in
    let d2 = Array2.dim2 mat in
    let a = Array.init d1 (fun _ -> Array.make d2 0.) in
    for i=0 to pred d1 do
      for j=0 to pred d2 do
	a.(i).(j) <- mat.{i,j}
      done
    done ;
    a

  let get (m : matrix) i j = Array2.get m i j
  let set (m : matrix) i j x = Array2.set m i j x
  let set_all = set_all
  let set_zero = set_zero
  let set_id m = 
    set_zero m ;
    for i=0 to pred (min (Array2.dim1 m) (Array2.dim2 m)) do
      set m i i 1.
    done
      
  let memcpy = memcpy

  let copy m =
    let m' = create (Array2.dim1 m) (Array2.dim2 m) in
    Array2.blit m m' ;
    m'

  let row = row

  external add : matrix -> matrix -> unit = "ml_gsl_matrix_float_add"
  external sub : matrix -> matrix -> unit = "ml_gsl_matrix_float_sub"
  external mul_elements : matrix -> matrix -> unit = "ml_gsl_matrix_float_mul"
  external div_elements : matrix -> matrix -> unit = "ml_gsl_matrix_float_div"
  external scale : matrix -> float -> unit = "ml_gsl_matrix_float_scale"
  external add_constant : matrix -> float -> unit = "ml_gsl_matrix_float_add_constant"
  external add_diagonal : matrix -> float -> unit = "ml_gsl_matrix_float_add_diagonal"
  external is_null : matrix -> bool = "ml_gsl_matrix_float_isnull"
  external swap_rows : matrix -> int -> int -> unit = "ml_gsl_matrix_float_swap_rows"
  external swap_columns : matrix -> int -> int -> unit = "ml_gsl_matrix_float_swap_columns"
  external swap_rowcol : matrix -> int -> int -> unit = "ml_gsl_matrix_float_swap_rowcol"
  external transpose : matrix -> matrix -> unit = "ml_gsl_matrix_float_transpose_memcpy"
  external transpose_in_place : matrix -> unit = "ml_gsl_matrix_float_transpose"
  end
