open Rpc_stack
open Utility
open Big_int

let mult (stack : rpc_stack) =
   if stack#length > 1 then
      let gen_el2 = stack#pop in
      let gen_el1 = stack#pop in
      match gen_el1 with
      |`Int el1 -> (
         match gen_el2 with
         |`Int el2 ->
            stack#push (`Int (mult_big_int el1 el2))
         |`Float el2 ->
            stack#push (`Float ((float_of_big_int el1) *. el2))
         |`Complex el2 ->
            let c_el1 = cmpx_of_int el1 in
            stack#push (`Complex (Complex.mul c_el1 el2))
         |`FloatMatrix el2 ->
            let result = Gsl_matrix.copy el2 in
            (Gsl_matrix.scale result (float_of_big_int el1);
            stack#push (`FloatMatrix result))
         |`ComplexMatrix el2 ->
            let c_el1 = cmpx_of_int el1 in
            (Gsl_matrix_complex.scale el2 c_el1;
            stack#push (`ComplexMatrix el2))
         )
      |`Float el1 -> (
         match gen_el2 with
         |`Int el2 ->
            stack#push (`Float (el1 *. float_of_big_int el2))
         |`Float el2 ->
            stack#push (`Float (el1 *. el2))
         |`Complex el2 ->
            let c_el1 = cmpx_of_float el1 in
            stack#push (`Complex (Complex.mul c_el1 el2))
         |`FloatMatrix el2 ->
            let result = Gsl_matrix.copy el2 in
            (Gsl_matrix.scale result el1;
            stack#push (`FloatMatrix result))
         |`ComplexMatrix el2 ->
            let c_el1 = cmpx_of_float el1 in
            (Gsl_matrix_complex.scale el2 c_el1;
            stack#push (`ComplexMatrix el2))
         )
      |`Complex el1 -> (
         match gen_el2 with
         |`Int el2 ->
            let c_el2 = cmpx_of_int el2 in
            stack#push (`Complex (Complex.mul el1 c_el2))
         |`Float el2 ->
            let c_el2 = cmpx_of_float el2 in
            stack#push (`Complex (Complex.mul el1 c_el2))
         |`Complex el2 ->
            stack#push (`Complex (Complex.mul el1 el2))
         |`FloatMatrix el2 ->
            let c_el2 = cmat_of_fmat el2 in
            (Gsl_matrix_complex.scale c_el2 el1;
            stack#push (`ComplexMatrix c_el2))
         |`ComplexMatrix el2 ->
            (Gsl_matrix_complex.scale el2 el1;
            stack#push (`ComplexMatrix el2))
         )
      |`FloatMatrix el1 -> (
         match gen_el2 with
         |`Int el2 ->
            let result = Gsl_matrix.copy el1 in
            (Gsl_matrix.scale result (float_of_big_int el2);
            stack#push (`FloatMatrix result))
         |`Float el2 ->
            let result = Gsl_matrix.copy el1 in
            (Gsl_matrix.scale result el2;
            stack#push (`FloatMatrix result))
         |`Complex el2 ->
            let c_el1 = cmat_of_fmat el1 in
            (Gsl_matrix_complex.scale c_el1 el2;
            stack#push (`ComplexMatrix c_el1))
         |`FloatMatrix el2 ->
            let n1, m1 = (Gsl_matrix.dims el1) and
            n2, m2     = (Gsl_matrix.dims el2) in
            if m1 = n2 then
               let result = Gsl_matrix.create n1 m2 in
               (Gsl_blas.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans 1.0 el1 el2 0.0 result;
               stack#push (`FloatMatrix result))
            else
               (stack#push gen_el2;
               stack#push gen_el1;
               raise (Invalid_argument "incompatible dimensions"))
         |`ComplexMatrix el2 ->
            let n1, m1 = (Gsl_matrix.dims el1) and
            n2, m2     = (Gsl_matrix_complex.dims el2) in
            if m1 = n2 then
               let c_el1 = cmat_of_fmat el1 and
               result = Gsl_matrix_complex.create n1 m2 in
               (Gsl_blas.Complex.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
                  Complex.one c_el1 el2 Complex.zero result;
               stack#push (`ComplexMatrix result))
            else
               (stack#push gen_el2; 
               stack#push gen_el1;
               raise (Invalid_argument "incompatible dimensions"))
         )
      |`ComplexMatrix el1 -> (
         match gen_el2 with
         |`Int el2 ->
            let c_el2 = cmpx_of_int el2 in
            (Gsl_matrix_complex.scale el1 c_el2;
            stack#push (`ComplexMatrix el1))
         |`Float el2 ->
            let c_el2 = cmpx_of_float el2 in
            (Gsl_matrix_complex.scale el1 c_el2;
            stack#push (`ComplexMatrix el1))
         |`Complex el2 ->
            (Gsl_matrix_complex.scale el1 el2;
            stack#push (`ComplexMatrix el1))
         |`FloatMatrix el2 ->
            let n1, m1 = (Gsl_matrix_complex.dims el1) and
            n2, m2     = (Gsl_matrix.dims el2) in
            if m1 = n2 then
               let c_el2 = cmat_of_fmat el2 and
               result = Gsl_matrix_complex.create m1 n2 in
               (Gsl_blas.Complex.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
                  Complex.one el1 c_el2 Complex.zero result;
               stack#push (`ComplexMatrix result))
            else
               (stack#push gen_el2;
               stack#push gen_el1;
               raise (Invalid_argument "incompatible dimensions"))
         |`ComplexMatrix el2 ->
            let n1, m1 = (Gsl_matrix_complex.dims el1) and
            n2, m2     = (Gsl_matrix_complex.dims el2) in
            if m1 = n2 then
               let result = Gsl_matrix_complex.create m1 n2 in
               (Gsl_blas.Complex.gemm Gsl_blas.NoTrans Gsl_blas.NoTrans
                  Complex.one el1 el2 Complex.zero result;
               stack#push (`ComplexMatrix result))
            else
               (stack#push gen_el2;
               stack#push gen_el1;
               raise (Invalid_argument "incompatible dimensions"))
         )
   else
      raise (Invalid_argument "insufficient arguments")
(* arch-tag: DO_NOT_CHANGE_5fc03e41-d1d3-40da-8b68-9a85d96148d0 *)
