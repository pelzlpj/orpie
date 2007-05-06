(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002-2005 - Olivier Andrieu                *)
(* distributed under the terms of the GPL version 2         *)

type complex_mat_flat = 
    { data : float array ;
      off  : int ;
      dim1 : int ;
      dim2 : int ; 
      tda  : int ; }

type matrix = complex_mat_flat

open Gsl_complex

let create ?(init=Complex.zero) dim1 dim2 =
  let mat = { 
    data = Array.create (2 * dim1 * dim2) init.Complex.re ;
    off = 0 ; dim1 = dim1 ; dim2 = dim2 ; tda = dim2 } in
  if init.Complex.im <> init.Complex.re
  then for i=0 to pred (dim1*dim2) do
    mat.data.(2*i+1) <- init.Complex.im
  done ;
  mat
    
let dims mat = 
  (mat.dim1, mat.dim2)

let get m i j =
  let k = 2 * (m.off + i*m.tda + j) in
  complex m.data.(k) m.data.(k+1)

let set m i j c =
  let k = 2 * (m.off + i*m.tda + j) in
  m.data.(k) <- c.re ;
  m.data.(k+1) <- c.im

let of_arrays arr = 
  let dim1 = Array.length arr in
  if dim1 = 0 then invalid_arg "of_arrays" ;
  let dim2 = Array.length arr.(0) in
  let tab = Array.make (2 * dim1 * dim2) 0. in
  let mat = { data = tab ; off = 0 ; 
	      dim1 = dim1 ; dim2 = dim2 ; tda = dim2 } in
  for i=0 to pred dim1 do
    let a = arr.(i) in
    for j=0 to pred dim2 do
      set mat i j a.(j)
    done
  done ;
  mat

let to_arrays mat = 
  let arr = Array.make_matrix mat.dim1 mat.dim2 Complex.zero in
  for i=0 to pred mat.dim1 do
    let a = arr.(i) in
    for j=0 to pred mat.dim2 do
      a.(j) <- get mat i j
    done
  done ;
  arr

let of_array arr dim1 dim2 =
  let len = Array.length arr in
  if dim1 * dim2 <> len
  then invalid_arg "of_array" ;
  let tab = Array.make (2 * dim1 * dim2) 0. in
  let mat = { data = tab ; off = 0 ; 
	      dim1 = dim1 ; dim2 = dim2 ; tda = dim2 } in
  for i=0 to pred dim1 do
    for j=0 to pred dim2 do
      set mat i j arr.(i*dim2+j)
    done
  done ;
  mat

let to_array mat = 
  let arr = Array.make (mat.dim1 * mat.dim2) Complex.zero in
  for i=0 to pred mat.dim1 do
    for j=0 to pred mat.dim2 do
      arr.(i*mat.dim2+j) <- get mat i j
    done
  done ;
  arr

let of_complex_array arr dim1 dim2 =
  let len = Array.length arr in
  if 2 * dim1 * dim2 <> len
  then invalid_arg "of_array" ;
  { data = Array.copy arr ; off = 0 ; 
    dim1 = dim1 ; dim2 = dim2 ; tda = dim2 }

let to_complex_array mat =
  if mat.tda = mat.dim2 && mat.off = 0
  then Array.copy mat.data
  else begin
    let tab = Array.create (2*mat.dim1*mat.dim2) 0. in
    for i=0 to pred mat.dim1 do
      for j=0 to pred mat.dim2 do
	Gsl_complex.set tab (i*mat.dim2 + j) (get mat i j)
      done
    done ;
    tab
  end

let set_all m c =
  for i=0 to pred m.dim1 do
    for j=0 to pred m.dim2 do
      set m i j c
    done
  done

let set_zero m =
  set_all m Complex.zero

let set_id m =
  set_zero m ;
  for i=0 to pred (min m.dim1 m.dim2) do
    set m i i Complex.one
  done

let memcpy ~src:m ~dst:m' =
  if m.dim1 <> m'.dim1 || m.dim2 <> m'.dim2
  then invalid_arg "wrong dimensions" ;
  for i=0 to pred m.dim1 do
    Array.blit 
      m.data  (2 * (m.off + i*m.tda))
      m'.data (2 * (m'.off + i*m'.tda)) (2 * m.dim2)
  done

let copy m = 
  let m' = create m.dim1 m.dim2 in
  memcpy m m' ;
  m'

let submatrix m ~k1 ~k2 ~n1 ~n2 =
  { m with 
    off = m.off + (k1*m.tda)+k2 ;
    dim1 = n1 ; dim2 = n2 ;
    tda = m.tda ; }
      
let view_complex_array arr ?(off=0) dim1 ?tda dim2 =
  let tda = match tda with
  | None -> dim2
  | Some v -> v in
  let len = Array.length arr in
  if dim1 * tda > len/2 - off || dim2 > tda
  then invalid_arg "view_array" ;
  { data = arr; off = off; dim1 = dim1; dim2 = dim2; tda = tda }

external add : matrix -> matrix -> unit = "ml_gsl_matrix_complex_add"
external sub : matrix -> matrix -> unit = "ml_gsl_matrix_complex_sub"
external mul_elements : matrix -> matrix -> unit = "ml_gsl_matrix_complex_mul"
external div_elements : matrix -> matrix -> unit = "ml_gsl_matrix_complex_div"
external scale : matrix -> float -> unit = "ml_gsl_matrix_complex_scale"
external add_constant : matrix -> float -> unit = "ml_gsl_matrix_complex_add_constant"
external add_diagonal : matrix -> complex -> unit = "ml_gsl_matrix_complex_add_diagonal"
external is_null : matrix -> bool = "ml_gsl_matrix_complex_isnull"

external swap_rows : matrix -> int -> int -> unit = "ml_gsl_matrix_complex_swap_rows"
external swap_columns : matrix -> int -> int -> unit = "ml_gsl_matrix_complex_swap_columns"
external swap_rowcol : matrix -> int -> int -> unit = "ml_gsl_matrix_complex_swap_rowcol"
external transpose : matrix -> matrix -> unit = "ml_gsl_matrix_complex_transpose_memcpy"
external transpose_in_place : matrix -> unit = "ml_gsl_matrix_complex_transpose"

let row m i =
  Gsl_vector_complex_flat.view_complex_array
    ~off:(m.off + i * m.tda)
    ~len:m.dim2
    m.data

let column m j =
  Gsl_vector_complex_flat.view_complex_array
    ~stride:m.tda
    ~off:(m.off + j)
    ~len:m.dim1
    m.data

let diagonal m =
  Gsl_vector_complex_flat.view_complex_array
    ~stride:(m.tda + 1)
    ~off:m.off
    ~len:(min m.dim1 m.dim2)
    m.data

let subdiagonal m k =
  Gsl_vector_complex_flat.view_complex_array
    ~stride:(m.tda + 1)
    ~off:(m.off + k * m.tda)
    ~len:(min (m.dim1 - k) m.dim2)
    m.data

let superdiagonal m k =
  Gsl_vector_complex_flat.view_complex_array
    ~stride:(m.tda + 1)
    ~off:(m.off + k)
    ~len:(min m.dim1 (m.dim2 - k))
    m.data

let view_vector v ?(off=0) dim1 ?tda dim2 =
  let tda = match tda with
  | None -> dim2
  | Some v -> v in
  let len = Gsl_vector_complex_flat.length v in
  if dim1 * tda > len - off || dim2 > tda
  then invalid_arg "view_vector" ;
  { data = v.Gsl_vector_complex_flat.data; 
    off  = v.Gsl_vector_complex_flat.off + off; 
    dim1 = dim1; dim2 = dim2; tda = tda }
