(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002 - Olivier Andrieu                     *)
(* distributed under the terms of the GPL version 2         *)

type complex_vector_flat = 
    { data   : float array ;
      off    : int ;
      len    : int ;
      stride : int ; }

type vector = complex_vector_flat

let create ?(init=Complex.zero) len = 
  let arr = { 
    data = Array.create (2*len) init.Complex.re ; 
    off = 0; 
    len = len; 
    stride = 1; } in
  if init.Complex.im <> init.Complex.re
  then for i=0 to pred len do
    arr.data.(2*i+1) <- init.Complex.im
  done ;
  arr

let of_array arr =
  let carr = Gsl_complex.pack arr in
  { data = carr; off = 0; 
    len = Array.length arr; stride = 1; }

let length { len = len } = 
  len 

let get v i = 
  Gsl_complex.get v.data (v.off + i*v.stride)

let set v i d =
  Gsl_complex.set v.data (v.off + i*v.stride) d

let set_all v d = 
  for i=0 to pred v.len do
    set v i d
  done

let set_zero v = 
  set_all v Complex.zero

let set_basis v i = 
  set_zero v ;
  set v i Complex.one

let to_array v =
  Array.init v.len (get v)

let of_complex_array carr =
  { data = Array.copy carr; off = 0; 
    len = (Array.length carr)/2; stride = 1; }

let to_complex_array arr =
  let carr = Array.create (2*arr.len) 0. in
  for i=0 to pred arr.len do
    Gsl_complex.set carr i (get arr i)
  done ;
  carr

let real carr =
  { Gsl_vector_flat.data   = carr.data ;
    Gsl_vector_flat.len    = carr.len ;
    Gsl_vector_flat.off    = 2 * carr.off ;
    Gsl_vector_flat.stride = 2 * carr.stride ; }

let imag carr =
  { Gsl_vector_flat.data   = carr.data ;
    Gsl_vector_flat.len    = carr.len ;
    Gsl_vector_flat.off    = 2 * carr.off + 1 ;
    Gsl_vector_flat.stride = 2 * carr.stride ; }

let subvector ?(stride=1) v ~off ~len =
  { v with 
    off = off * v.stride + v.off ;
    len = len ;
    stride = stride * v.stride ; }

let view_complex_array ?(stride=1) ?(off=0) ?len arr =
  let alen = Array.length arr in
  if alen mod 2 <> 0
  then invalid_arg "complex_array dim" ;
  let len = match len with
  | None -> alen / 2
  | Some l -> l in
  { data = arr ; off = off ;
    stride = stride ; len = len }

let memcpy v w = 
  if v.len <> w.len
  then invalid_arg "Gsl_vector.memcpy" ;
  for i=0 to pred v.len do
    set w i (get v i)
  done

let copy v = 
  { v with data = Array.copy v.data }

let swap_element v i j =
  let d  = get v i in
  let d' = get v j in
  set v j d ;
  set v i d'

let reverse v = 
  for i=0 to pred (v.len/2) do
    swap_element v i (pred v.len - i)
  done
