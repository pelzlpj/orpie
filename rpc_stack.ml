open Big_int
type rpc_data = [ `Int of Big_int.big_int
                | `Float of float
                | `Complex of Complex.t
                | `FloatMatrix of Gsl_matrix.matrix 
                | `ComplexMatrix of Gsl_matrix_complex.matrix ]


class rpc_stack =
   object(self)
      val stack = Stack.create ()

      (* generate a list of strings, one representing each stack entry *)
      method get_display_lines =
         let display_lines = ref [] and
         count = ref 0 in
         let print_el gen_el =
            begin
               count := !count + 1;
               match gen_el with
               |`Int el ->
                  let line = Printf.sprintf "%2d: # %sd" !count
                     (string_of_big_int el) in
                  display_lines := line :: !display_lines
               |`Float el ->
                  let line = Printf.sprintf "%2d: %g" !count el in
                  display_lines := line :: !display_lines
               |`Complex el ->
                  let line = Printf.sprintf "%2d: (%g, %g)" !count
                  el.Complex.re el.Complex.im in
                  display_lines := line :: !display_lines
               |`FloatMatrix el ->
                  (* looks like [[ a11, a12 ][ a21, a22 ]] *)
                  let mat = el in
                  let rows, cols = (Gsl_matrix.dims mat) in
                  let initial_string = Printf.sprintf "%2d: [" !count in
                  let line = ref initial_string in
                  for n = 0 to rows - 1 do
                     line := !line ^ "[ ";
                     for m = 0 to cols - 2 do
                        line := !line ^ (Printf.sprintf "%g, " mat.{n, m})
                     done;
                     line := !line ^ (Printf.sprintf "%g ]" mat.{n, cols-1})
                  done;
                  line := !line ^ "]";
                  display_lines := !line :: !display_lines
               |`ComplexMatrix el ->
                  (* looks like [[ (a11re, a11im), (a12re, a12im) ][ (a21re,
                     a21im), (a22re, a22im) ] *)
                  let mat = el in
                  let rows, cols = (Gsl_matrix_complex.dims mat) in
                  let initial_string = Printf.sprintf "%2d: [" !count in
                  let line = ref initial_string in
                  for n = 0 to rows - 1 do
                     line := !line ^ "[ ";
                     for m = 0 to cols - 2 do
                        line := !line ^ (Printf.sprintf "(%g, %g), " 
                           mat.{n, m}.Complex.re mat.{n, m}.Complex.im)
                     done;
                     line := !line ^ (Printf.sprintf "(%g, %g) ]" 
                        mat.{n, cols-1}.Complex.re mat.{n, cols-1}.Complex.im)
                  done;
                  line := !line ^ "]";
                  display_lines := !line :: !display_lines
            end
         in
         (Stack.iter print_el stack;
         !display_lines)

      method length = Stack.length stack

      method push v =
         Stack.push v stack

      method pop : rpc_data =
         Stack.pop stack

   end;;



(* arch-tag: DO_NOT_CHANGE_59b80e87-dfde-4203-a7a2-8e1f95813151 *)
