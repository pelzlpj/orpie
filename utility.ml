let cmpx_of_int i   = {Complex.re=Big_int.float_of_big_int i; Complex.im=0.0}
let cmpx_of_float f = {Complex.re=f; Complex.im=0.0}
let cmat_of_fmat fm =
   let rows, cols = Gsl_matrix.dims fm and
   f_array = Gsl_matrix.to_array fm in
   let c_array = Array.map cmpx_of_float f_array in
   Gsl_matrix_complex.of_array c_array rows cols

(* arch-tag: DO_NOT_CHANGE_a87790db-2dd0-496c-9620-ed968f3253fd *)
