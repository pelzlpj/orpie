open Rpc_stack
open Utility
open Big_int
      
class rpc_calc =
   object
      val stack = new rpc_stack

      method add =
         Add.add stack

      method sub =
         Sub.sub stack

      method mult =
         Mult.mult stack

      method div =
         Div.div stack

      method inv =
         Inv.inv stack

      method pow =
         Pow.pow stack

      method neg =
         if stack#length > 0 then
            let gen_el = stack#pop in
            match gen_el with
            |RpcInt el ->
               stack#push (RpcInt (minus_big_int el))
            |RpcFloat el ->
               stack#push (RpcFloat (0.0 -. el))
            |RpcComplex el ->
               stack#push (RpcComplex (Complex.neg el))
            |RpcFloatMatrix el ->
               (Gsl_matrix.scale el (-1.0);
               stack#push (RpcFloatMatrix el))
            |RpcComplexMatrix el ->
               (Gsl_matrix_complex.scale el {Complex.re=(-1.0); Complex.im=0.0};
               stack#push (RpcComplexMatrix el))
         else
            raise (Invalid_argument "empty stack")

      method sqrt =
         if stack#length > 0 then
            let gen_el = stack#pop in
            match gen_el with
            |RpcFloat el ->
               stack#push (RpcFloat (sqrt el))
            |RpcComplex el ->
               stack#push (RpcComplex (Complex.sqrt el))
            |_ ->
               (stack#push gen_el;
               raise (Invalid_argument "invalid argument"))
         else
            raise (Invalid_argument "empty stack")

      method abs =
         if stack#length > 0 then
            let gen_el = stack#pop in
            match gen_el with
            |RpcInt el ->
               stack#push (RpcInt (abs_big_int el))
            |RpcFloat el ->
               stack#push (RpcFloat (abs_float el))
            |RpcComplex el ->
               stack#push (RpcFloat (Complex.norm el))
            |_ ->
               (stack#push gen_el;
               raise (Invalid_argument "invalid argument"))
         else
            raise (Invalid_argument "empty stack")

      method arg =
         if stack#length > 0 then
            let gen_el = stack#pop in
            match gen_el with
            |RpcComplex el ->
               stack#push (RpcFloat (Complex.arg el))
            |_ ->
               (stack#push gen_el;
               raise (Invalid_argument "invalid argument"))
         else
            raise (Invalid_argument "empty stack")


      method exp =
         if stack#length > 0 then
            let gen_el = stack#pop in
            match gen_el with
            |RpcInt el ->
               stack#push (RpcFloat (exp (float_of_big_int el)))
            |RpcFloat el ->
               stack#push (RpcFloat (exp el))
            |RpcComplex el ->
               stack#push (RpcComplex (Complex.exp el))
            |_ ->
               (stack#push gen_el;
               raise (Invalid_argument "invalid argument"))
         else
            raise (Invalid_argument "empty stack")


      method ln =
         if stack#length > 0 then
            let gen_el = stack#pop in
            match gen_el with
            |RpcInt el ->
               stack#push (RpcFloat (log (float_of_big_int el)))
            |RpcFloat el ->
               stack#push (RpcFloat (log el))
            |RpcComplex el ->
               stack#push (RpcComplex (Complex.log el))
            |_ ->
               (stack#push gen_el;
               raise (Invalid_argument "invalid argument"))
         else
            raise (Invalid_argument "empty stack")


      method conj =
         if stack#length > 0 then
            let gen_el = stack#pop in
            match gen_el with
            |RpcInt el ->
               stack#push (RpcInt el)
            |RpcFloat el ->
               stack#push (RpcFloat el)
            |RpcComplex el ->
               stack#push (RpcComplex (Complex.conj el))
            |RpcFloatMatrix el ->
               stack#push (RpcFloatMatrix el)
            |RpcComplexMatrix el ->
               (* element-by-element conjugation *)
               let rows, cols = Gsl_matrix_complex.dims el and
               arr = Gsl_matrix_complex.to_array el in
               let conj_arr = Array.map Complex.conj arr in
               let conj_mat = Gsl_matrix_complex.of_array conj_arr rows cols in
               stack#push (RpcComplexMatrix conj_mat)


      method drop = stack#pop

      method enter_int i =
         stack#push (RpcInt i)

      method enter_float f =
         stack#push (RpcFloat f)

      method enter_cmpx f =
         stack#push (RpcComplex f)

      method enter_fmat fm =
         stack#push (RpcFloatMatrix fm)

      method enter_cmat cm =
         stack#push (RpcComplexMatrix cm)

      method print_stack =
         let print_el el = Printf.printf "%s\n" el in
         List.iter print_el stack#get_display_lines

   end;;








(* arch-tag: DO_NOT_CHANGE_548916d4-da42-49b4-8941-c0d42306f1b7 *)
