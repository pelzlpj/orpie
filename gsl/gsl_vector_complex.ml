(* ocamlgsl - OCaml interface to GSL                        *)
(* Copyright (©) 2002 - Olivier Andrieu                     *)
(* distributed under the terms of the GPL version 2         *)

open Bigarray

type complex_double_vector_bigarr = 
    (Complex.t, complex64_elt, c_layout) Bigarray.Array1.t

type vector = complex_double_vector_bigarr

let create ?init len = 
  let barr = Array1.create complex64 c_layout len in
  begin match init with
  | None -> ()
  | Some x -> Array1.fill barr x
  end ;
  barr

let length = Array1.dim

let of_array arr =
  Array1.of_array complex64 c_layout arr

let to_array v =
  Array.init (Array1.dim v) (Array1.get v)

let of_complex_array arr = 
  let n = (Array.length arr) / 2 in
  let barr = create n in
  for i=0 to pred n do
    barr.{i} <- Gsl_complex.get arr i
  done ;
  barr

let to_complex_array barr =
  let n = Array1.dim barr in
  let arr = Array.create (2*n) 0. in
  for i=0 to pred n do
    Gsl_complex.set arr i barr.{i}
  done ;
  arr

let get (v : vector) i = Array1.get v i

let set (v : vector) i x = Array1.set v i x

let set_all = Array1.fill 

let set_zero v = 
  set_all v Complex.zero

let set_basis v i = 
  set_zero v ;
  set v i Complex.one

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




module Single =
  struct
  
  type complex_float_vector_bigarr = 
      (Complex.t, complex32_elt, c_layout) Bigarray.Array1.t

  type vector = complex_float_vector_bigarr
	
  let create ?init len = 
    let barr = Array1.create complex32 c_layout len in
    begin match init with
    | None -> ()
    | Some x -> Array1.fill barr x
    end ;
    barr
      
  let length = length
      
  let of_array arr =
    Array1.of_array complex32 c_layout arr

  let to_array = to_array

  let of_complex_array arr = 
    let n = (Array.length arr) / 2 in
    let barr = create n in
    for i=0 to pred n do
      barr.{i} <- Gsl_complex.get arr i
    done ;
    barr

  let to_complex_array barr =
    let n = Array1.dim barr in
    let arr = Array.create (2*n) 0. in
    for i=0 to pred n do
      Gsl_complex.set arr i barr.{i}
    done ;
    arr

  let get (v : vector) i = Array1.get v i

  let set (v : vector) i x = Array1.set v i x

  let set_all = set_all

  let set_zero = set_zero

  let set_basis v i = 
    set_zero v ;
    set v i Complex.one

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
  end
