(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002-2005 - Olivier Andrieu                *)
(* distributed under the terms of the GPL version 2         *)

type double_vector_flat = 
    { data   : float array ;
      off    : int ;
      len    : int ;
      stride : int ; }

type vector = double_vector_flat

let check v =
  let size = Array.length v.data in
  if v.off < 0 || v.len < 0 || v.stride < 1 ||
     v.off + (v.len - 1) * v.stride >= size
  then failwith "Gsl_vector_flat.check" ;
  v

let create ?(init=0.) len = 
  { data = Array.create len init; 
    off = 0; 
    len = len; 
    stride = 1; }

let of_array arr =
  { data = Array.copy arr; off = 0; 
    len = Array.length arr; stride = 1; }

let length { len = len } = 
  len 

let get v i = 
  v.data.(v.off + i*v.stride)

let set v i d =
  v.data.(v.off + i*v.stride) <- d

let set_all v d = 
  for i=0 to pred v.len do
    set v i d
  done

let set_zero v = 
  set_all v 0.

let set_basis v i = 
  set_zero v ;
  set v i 1.

let to_array v =
  Array.init v.len (get v)

let subvector ?(stride=1) v ~off ~len =
  check
    { v with 
      off = off * v.stride + v.off ;
      len = len ;
      stride = stride * v.stride ; }
      
let view_array ?(stride=1) ?(off=0) ?len arr =
  let len = match len with
  | None -> Array.length arr
  | Some l -> l in
  check
    { data = arr ; off = off ;
      stride = stride ; len = len }

let memcpy ~src:v ~dst:w = 
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


external add : vector -> vector -> unit 
    = "ml_gsl_vector_add"
external sub : vector -> vector -> unit 
    = "ml_gsl_vector_sub"
external mul : vector -> vector -> unit 
    = "ml_gsl_vector_mul"
external div : vector -> vector -> unit 
    = "ml_gsl_vector_div"
external scale : vector -> float -> unit 
    = "ml_gsl_vector_scale"
external add_constant : vector -> float -> unit
    = "ml_gsl_vector_add_constant"
external is_null : vector -> bool
    = "ml_gsl_vector_isnull"

external max : vector -> float
    = "ml_gsl_vector_max"
external min : vector -> float
    = "ml_gsl_vector_min"
external minmax : vector -> float * float
    = "ml_gsl_vector_minmax"
external max_index : vector -> int
    = "ml_gsl_vector_maxindex"
external min_index : vector -> int
    = "ml_gsl_vector_minindex"
external minmax_index : vector -> int * int
    = "ml_gsl_vector_minmaxindex"
