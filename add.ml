open Rpc_stack
open Utility
open Big_int

let add (stack : rpc_stack) =
   if stack#length > 1 then
      let gen_el2 = stack#pop in
      let gen_el1 = stack#pop in
      match gen_el1 with 
      |`Int el1 -> (
         match gen_el2 with 
         |`Int el2 ->
            stack#push (`Int (add_big_int el1 el2))
         |`Float el2 ->
            stack#push (`Float ((float_of_big_int el1) +. el2))
         |`Complex el2 ->
            let c_el1 = cmpx_of_int el1 in
            stack#push (`Complex (Complex.add c_el1 el2))
         |_ ->
            (* if the elements are incompatible, we have to
               put them back on the stack *)
            (stack#push gen_el2;
            stack#push gen_el1;
            raise (Invalid_argument "incompatible types"))
         )
      |`Float el1 -> (
         match gen_el2 with 
         |`Int el2 ->
            stack#push (`Float (el1 +. float_of_big_int el2))
         |`Float el2 ->
            stack#push (`Float (el1 +. el2))
         |`Complex el2 ->
            let c_el1 = cmpx_of_float el1 in
            stack#push (`Complex (Complex.add c_el1 el2))
         |_ ->
            (* if the elements are incompatible, we have to
               put them back on the stack *)
            (stack#push gen_el2;
            stack#push gen_el1;
            raise (Invalid_argument "incompatible types"))
         )
      |`Complex el1 -> (
         match gen_el2 with
         |`Int el2 ->
            let c_el2 = cmpx_of_int el2 in
            stack#push (`Complex (Complex.add el1 c_el2))
         |`Float el2 ->
            let c_el2 = cmpx_of_float el2 in
            stack#push (`Complex (Complex.add el1 c_el2))
         |`Complex el2 ->
            stack#push (`Complex (Complex.add el1 el2))
         |_ ->
            (* if the elements are incompatible, we have to
               put them back on the stack *)
            (stack#push gen_el2;
            stack#push gen_el1;
            raise (Invalid_argument "incompatible types"))
         )
      |`FloatMatrix el1 -> (
         match gen_el2 with
         |`FloatMatrix el2 ->
            let dim1 = (Gsl_matrix.dims el1) and
            dim2     = (Gsl_matrix.dims el2) in
            if dim1 = dim2 then
               let result = Gsl_matrix.copy el1 in
               (Gsl_matrix.add result el2;
               stack#push (`FloatMatrix result))
            else
               (* clean up *)
               (stack#push gen_el2;
               stack#push gen_el1;
               raise (Invalid_argument "incompatible dimension"))
         |`ComplexMatrix el2 ->
            let dim1 = (Gsl_matrix.dims el1) and
            dim2     = (Gsl_matrix_complex.dims el2) in
            if dim1 = dim2 then
               let c_el1 = cmat_of_fmat el1 in
               (Gsl_matrix_complex.add c_el1 el2;
               stack#push (`ComplexMatrix c_el1))
            else
               (stack#push gen_el2;
               stack#push gen_el1;
               raise (Invalid_argument "incompatible dimension"))
         |_ ->
            (* if the elements are incompatible, we have to
               put them back on the stack *)
            (stack#push gen_el2;
            stack#push gen_el1;
            raise (Invalid_argument "incompatible types"))
         )
      |`ComplexMatrix el1 -> (
         match gen_el2 with
         |`FloatMatrix el2 ->
            let dim1 = (Gsl_matrix_complex.dims el1) and
            dim2     = (Gsl_matrix.dims el2) in
            if dim1 = dim2 then
               let c_el2 = cmat_of_fmat el2 in
               (Gsl_matrix_complex.add el1 c_el2;
               stack#push (`ComplexMatrix el1))
            else
               (stack#push gen_el2;
               stack#push gen_el1;
               raise (Invalid_argument "incompatible dimension"))
         |`ComplexMatrix el2 ->
            let dim1 = (Gsl_matrix_complex.dims el1) and
            dim2     = (Gsl_matrix_complex.dims el2) in
            if dim1 = dim2 then
               (Gsl_matrix_complex.add el1 el2;
               stack#push (`ComplexMatrix el1))
            else
               (stack#push gen_el2;
               stack#push gen_el1;
               raise (Invalid_argument "incompatible dimension"))
         |_ ->
            (* if the elements are incompatible, we have to
               put them back on the stack *)
            (stack#push gen_el2;
            stack#push gen_el1;
            raise (Invalid_argument "incompatible types"))
         )
   else
      raise (Invalid_argument "insufficient arguments")

(* arch-tag: DO_NOT_CHANGE_f59cab0f-755e-4d09-9d30-114114945b38 *)
