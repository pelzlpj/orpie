(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002-2005 - Olivier Andrieu                *)
(* distributed under the terms of the GPL version 2         *)

open Bigarray

type double_vector_bigarr = 
    (float, float64_elt, c_layout) Bigarray.Array1.t

type vector = double_vector_bigarr

let create ?init len = 
  let barr = Array1.create float64 c_layout len in
  begin match init with
  | None -> ()
  | Some x -> Array1.fill barr x
  end ;
  barr

let length = Array1.dim

let of_array arr =
  Array1.of_array float64 c_layout arr

let to_array v =
  Array.init (Array1.dim v) (Array1.get v)

let get (v : vector) i = Array1.get v i

let set (v : vector) i x = Array1.set v i x

let set_all =
  Array1.fill 

let set_zero v = 
  set_all v 0.

let set_basis v i = 
  set_zero v ;
  set v i 1.

let subvector v ~off ~len =
  Array1.sub v off len

let memcpy ~src:v ~dst:w = 
  if length v <> length w
  then invalid_arg "Gsl_vector.memcpy" ;
  Array1.blit v w

let copy v = 
  let w = create (length v) in
  memcpy v w ;
  w

let swap_element v i j =
  let d  = get v i in
  let d' = get v j in
  set v j d ;
  set v i d'

let reverse v = 
  let len = length v in
  for i=0 to pred (len/2) do
    swap_element v i (pred len - i)
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




module Single = 
struct
  type float_vector_bigarr = 
      (float, float32_elt, c_layout) Bigarray.Array1.t

  type vector = float_vector_bigarr
	
  let create ?init len = 
    let barr = Array1.create float32 c_layout len in
    begin match init with
    | None -> ()
    | Some x -> Array1.fill barr x
    end ;
    barr
      
  let length = length
      
  let of_array arr =
    Array1.of_array float32 c_layout arr

  let to_array = to_array

  let get (v : vector) i = Array1.get v i

  let set (v : vector) i x = Array1.set v i x

  let set_all = set_all

  let set_zero = set_zero

  let set_basis v i = 
    set_zero v ;
    set v i 1.

  let subvector = subvector

  let memcpy = memcpy

  let copy v = 
  let w = create (length v) in
  memcpy v w ;
  w
    
  let swap_element v i j =
    let d  = get v i in
    let d' = get v j in
    set v j d ;
    set v i d'

  let reverse v = 
    let len = length v in
    for i=0 to pred (len/2) do
      swap_element v i (pred len - i)
    done
      
  external add : vector -> vector -> unit 
      = "ml_gsl_vector_float_add"
  external sub : vector -> vector -> unit 
      = "ml_gsl_vector_float_sub"
  external mul : vector -> vector -> unit 
      = "ml_gsl_vector_float_mul"
  external div : vector -> vector -> unit 
      = "ml_gsl_vector_float_div"
  external scale : vector -> float -> unit 
      = "ml_gsl_vector_float_scale"
  external add_constant : vector -> float -> unit
      = "ml_gsl_vector_float_add_constant"
  external is_null : vector -> bool
      = "ml_gsl_vector_float_isnull"
      
  external max : vector -> float
    = "ml_gsl_vector_float_max"
  external min : vector -> float
      = "ml_gsl_vector_float_min"
  external minmax : vector -> float * float
      = "ml_gsl_vector_float_minmax"
  external max_index : vector -> int
      = "ml_gsl_vector_float_maxindex"
  external min_index : vector -> int
      = "ml_gsl_vector_float_minindex"
  external minmax_index : vector -> int * int
      = "ml_gsl_vector_float_minmaxindex"
end
