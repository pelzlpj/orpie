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
            |`Int el ->
               stack#push (`Int (minus_big_int el))
            |`Float el ->
               stack#push (`Float (0.0 -. el))
            |`Complex el ->
               stack#push (`Complex (Complex.neg el))
            |`FloatMatrix el ->
               (Gsl_matrix.scale el (-1.0);
               stack#push (`FloatMatrix el))
            |`ComplexMatrix el ->
               (Gsl_matrix_complex.scale el {Complex.re=(-1.0); Complex.im=0.0};
               stack#push (`ComplexMatrix el))
         else
            raise (Invalid_argument "empty stack")

      method sqrt =
         if stack#length > 0 then
            let gen_el = stack#pop in
            match gen_el with
            |`Float el ->
               stack#push (`Float (sqrt el))
            |`Complex el ->
               stack#push (`Complex (Complex.sqrt el))
            |_ ->
               (stack#push gen_el;
               raise (Invalid_argument "invalid argument"))
         else
            raise (Invalid_argument "empty stack")

      method abs =
         if stack#length > 0 then
            let gen_el = stack#pop in
            match gen_el with
            |`Int el ->
               stack#push (`Int (abs_big_int el))
            |`Float el ->
               stack#push (`Float (abs_float el))
            |`Complex el ->
               stack#push (`Float (Complex.norm el))
            |_ ->
               (stack#push gen_el;
               raise (Invalid_argument "invalid argument"))
         else
            raise (Invalid_argument "empty stack")

      method arg =
         if stack#length > 0 then
            let gen_el = stack#pop in
            match gen_el with
            |`Complex el ->
               stack#push (`Float (Complex.arg el))
            |_ ->
               (stack#push gen_el;
               raise (Invalid_argument "invalid argument"))
         else
            raise (Invalid_argument "empty stack")


      method exp =
         if stack#length > 0 then
            let gen_el = stack#pop in
            match gen_el with
            |`Int el ->
               stack#push (`Float (exp (float_of_big_int el)))
            |`Float el ->
               stack#push (`Float (exp el))
            |`Complex el ->
               stack#push (`Complex (Complex.exp el))
            |_ ->
               (stack#push gen_el;
               raise (Invalid_argument "invalid argument"))
         else
            raise (Invalid_argument "empty stack")


      method ln =
         if stack#length > 0 then
            let gen_el = stack#pop in
            match gen_el with
            |`Int el ->
               stack#push (`Float (log (float_of_big_int el)))
            |`Float el ->
               stack#push (`Float (log el))
            |`Complex el ->
               stack#push (`Complex (Complex.log el))
            |_ ->
               (stack#push gen_el;
               raise (Invalid_argument "invalid argument"))
         else
            raise (Invalid_argument "empty stack")


      method conj =
         if stack#length > 0 then
            let gen_el = stack#pop in
            match gen_el with
            |`Int el ->
               stack#push (`Int el)
            |`Float el ->
               stack#push (`Float el)
            |`Complex el ->
               stack#push (`Complex (Complex.conj el))
            |`FloatMatrix el ->
               stack#push (`FloatMatrix el)
            |`ComplexMatrix el ->
               (* element-by-element conjugation *)
               let rows, cols = Gsl_matrix_complex.dims el and
               arr = Gsl_matrix_complex.to_array el in
               let conj_arr = Array.map Complex.conj arr in
               let conj_mat = Gsl_matrix_complex.of_array conj_arr rows cols in
               stack#push (`ComplexMatrix conj_mat)


      method drop = stack#pop

      method enter_int i =
         stack#push (`Int i)

      method enter_float f =
         stack#push (`Float f)

      method enter_cmpx f =
         stack#push (`Complex f)

      method enter_fmat fm =
         stack#push (`FloatMatrix fm)

      method enter_cmat cm =
         stack#push (`ComplexMatrix cm)

      method print_stack =
         let print_el el = Printf.printf "%s\n" el in
         List.iter print_el stack#get_display_lines

   end;;








(* arch-tag: DO_NOT_CHANGE_548916d4-da42-49b4-8941-c0d42306f1b7 *)
