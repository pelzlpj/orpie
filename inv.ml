open Rpc_stack
open Gsl_error
open Utility


let inv (stack : rpc_stack) =
   if stack#length > 0 then
      let gen_el = stack#pop in
      match gen_el with
      |RpcFloat el ->
         stack#push (RpcFloat (1.0 /. el))
      |RpcComplex el ->
         stack#push (RpcComplex (Complex.inv el))
      |RpcFloatMatrix el ->
         let n, m = (Gsl_matrix.dims el) in
         if n = m then
            let copy_el = Gsl_vectmat.mat_convert ~protect:true (`M el) and
            perm = Gsl_permut.create m and
            inv = Gsl_matrix.create m m in
            try
               let sign = Gsl_linalg._LU_decomp copy_el perm in
               (Gsl_linalg._LU_invert copy_el perm (`M inv);
               stack#push (RpcFloatMatrix inv))
            with Gsl_exn _ ->
               (stack#push gen_el;
               raise (Invalid_argument "singular matrix"))
         else
            (stack#push gen_el;
            raise (Invalid_argument "non-square matrix"))
      |RpcComplexMatrix el ->
         let n, m = (Gsl_matrix_complex.dims el) in
         if n = m then
            let copy_el = Gsl_vectmat.cmat_convert ~protect:true (`CM el) and
            perm = Gsl_permut.create m and
            inv = Gsl_matrix_complex.create m m in
            try
               let sign = Gsl_linalg.complex_LU_decomp copy_el perm in
               (Gsl_linalg.complex_LU_invert copy_el perm (`CM inv);
               stack#push (RpcComplexMatrix inv))
            with Gsl_exn _ ->
               (stack#push gen_el;
               raise (Invalid_argument "singular matrix"))
         else
            (stack#push gen_el;
            raise (Invalid_argument "non-square matrix"))
      |_ ->
         raise (Invalid_argument "invalid argument")

   else
      raise (Invalid_argument "empty stack")



(* arch-tag: DO_NOT_CHANGE_d8ce074c-3d77-4448-b3c6-9e239b853aad *)
