open Curses

let () = ripoffline true
let w = initscr ()
let (wd, ncol) = get_ripoff ()

(*let () = assert (start_color ())
let () = assert (init_pair 1 Color.red Color.white)*)

let () = assert (cbreak ())
let () = assert (noecho ())
let () = assert (intrflush w false)
let () = assert (keypad w true)

let () = for i = 0 to 10 do
  assert (mvaddch i (i * 2) (A.color_pair 1 + 111))
done

let () = border 0 0 0 0 0 0 0 0
let () = wborder w 0 0 0 0 0 0 0 0

let () = assert (refresh ())

let (c1, c2) = mousemask (-1)

let () = assert (mvaddstr 3 1 "Bonjour")
let () = assert (mvaddstr 4 2 (string_of_int c1))
let () = assert (mvaddstr 5 2 (string_of_int c2))

let t = Array.init 50 (fun x -> 64 + x)
let () = assert (addchnstr t 10 3)
let () = assert (mvaddnstr 8 40 "Bonjour" 1 3)
let () = assert (mvinsstr 8 40 "toto     ")

let t = [|0; 0; 0; 0 |]

let () = assert (inchnstr t 0 3)

let () = winch_handler_on ()

let kup = tigetstr "kcuu1"
let () = assert (addstr kup)

let acs = get_acs_codes ()
let () = assert (addch acs.Acs.ulcorner)

let i = getch ()

let (nc, np, can) = (colors (), color_pairs (), can_change_color ())

let (c1, c2) = pair_content 1

let l = ref []
let () = assert (tputs "totoping" 1 (fun c -> l := (int_of_char c) :: !l))

let (tr, tc) = get_size ()

let () = endwin ()

let () = Array.iter (fun x -> print_int x; print_newline ()) t

let () = print_string "key="; print_int i; print_newline ()
let () = print_int tr; print_string " "; print_int tc; print_newline ()
let () = print_int nc; print_string " "
let () = print_int np; print_string " "
let () = print_string (if can then "oui" else "non"); print_newline ()
let () = print_int c1; print_string " "
let () = print_int c2; print_newline ()
let () = print_int ncol; print_newline ()
let () = List.iter (fun x -> print_int x; print_string " ") !l;
  print_newline ()

(*let i = ref 0
let () = while
  let (a, b, c) = str_terminfo_variable !i in
  (a <> "") && (print_string (a ^ "\t" ^ b ^ "\t" ^ c);
    print_newline (); true) do
  i := !i + 1
done*)
(*let () = Hashtbl.iter (fun a (b,c) ->
  print_string (a ^ "\t" ^ b ^ "\t" ^ c); print_newline ())
  str_terminfo_variables*)

