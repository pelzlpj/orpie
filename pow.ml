open Rpc_stack
open Utility
open Big_int

let pow (stack : rpc_stack) =
   if stack#length > 1 then
      let gen_el2 = stack#pop in
      let gen_el1 = stack#pop in
      match gen_el1 with
      |`Int el1 -> (
         match gen_el2 with
         |`Int el2 ->
            if (sign_big_int el2) != (-1) then
               stack#push (`Int 
                  (power_big_int_positive_big_int el1 el2))
            else
               (stack#push gen_el2;
               stack#push gen_el1;
               raise (Invalid_argument "invalid argument"))
         |`Float el2 ->
            let f_el1 = float_of_big_int el1 in
            stack#push (`Float (f_el1 ** el2))
         |`Complex el2 ->
            let c_el1 = cmpx_of_int el1 in
            stack#push (`Complex (Complex.pow c_el1 el2))
         |_ ->
            (stack#push gen_el2;
            stack#push gen_el1;
            raise (Invalid_argument "incompatible types"))
         )
      |`Float el1 -> (
         match gen_el2 with
         |`Int el2 ->
            stack#push (`Float (el1 ** (float_of_big_int el2)))
         |`Float el2 ->
            stack#push (`Float (el1 ** el2))
         |`Complex el2 ->
            stack#push (`Complex (Complex.pow (cmpx_of_float el1) el2))
         |_ ->
            (stack#push gen_el2;
            stack#push gen_el1;
            raise (Invalid_argument "incompatible types"))
         )
      |`Complex el1 -> (
         match gen_el2 with
         |`Int el2 ->
            stack#push (`Complex (Complex.pow el1 (cmpx_of_int el2)))
         |`Float el2 ->
            stack#push (`Complex (Complex.pow el1 (cmpx_of_float el2)))
         |`Complex el2 ->
            stack#push (`Complex (Complex.pow el1 el2))
         |_ ->
            (stack#push gen_el2;
            stack#push gen_el1;
            raise (Invalid_argument "incompatible types"))
         )
      |_ ->
         (stack#push gen_el2;
         stack#push gen_el1;
         raise (Invalid_argument "invalid argument"))
   else
      raise (Invalid_argument "insufficient arguments")



(* arch-tag: DO_NOT_CHANGE_55f98700-eb1e-4457-ae73-18170a816984 *)
