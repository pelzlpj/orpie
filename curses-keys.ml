open Curses;;


begin
   let std = initscr () in
   assert (keypad std true);
   assert (cbreak ());
   assert (noecho ());
   assert (curs_set 0);
   assert (mvaddstr 1 0 "Type some keys to see the corresponding octal and string representations:");
   assert (refresh ())
end;
let string_of_chtype ch =
   let str = keyname ch in
   match str with
   |"KEY_LEFT" -> "<left>"
   |"KEY_RIGHT" -> "<right>"
   |"KEY_UP" -> "<up>"
   |"KEY_DOWN" -> "<down>"
   |"KEY_BACKSPACE" -> "<backspace>"
   |"KEY_IC" -> "<insert>"
   |"KEY_DC" -> "<delete>"
   |"KEY_HOME" -> "<home>"
   |"KEY_END" -> "<end>"
   |"KEY_PPAGE" -> "<pageup>"
   |"KEY_NPAGE" -> "<pagedown>"
   |"^J" -> "<return>"
   |" " -> "<space>"
   |"KEY_F(1)" -> "<f1>"
   |"KEY_F(2)" -> "<f2>"
   |"KEY_F(3)" -> "<f3>"
   |"KEY_F(4)" -> "<f4>"
   |"KEY_F(5)" -> "<f5>"
   |"KEY_F(6)" -> "<f6>"
   |"KEY_F(7)" -> "<f7>"
   |"KEY_F(8)" -> "<f8>"
   |"KEY_F(9)" -> "<f9>"
   |"KEY_F(10)" -> "<f10>"
   |"KEY_F(11)" -> "<f11>"
   |"KEY_F(12)" -> "<f12>"
   |"KEY_ENTER" -> "<enter>"
   |"^I" -> "<tab>"
   |"^[" -> "<esc>"
   |_ ->  
      if str.[0] = '^' then
         "\\C" ^ (String.sub str 1 ((String.length str) - 1))
      else
         str
in
while true do
   let k = getch () in
   let s1 = Printf.sprintf " octal: \\%.3o" k and
   s2 = Printf.sprintf     "string: %s" (string_of_chtype k) in
   (assert (move 2 0);
   clrtoeol ();
   assert (addstr s1);
   assert (move 3 0);
   clrtoeol ();
   assert (addstr s2);
   assert (refresh ()))
done;
endwin ();;




(* arch-tag: DO_NOT_CHANGE_31cbf03e-49f9-4f66-844c-ceb584edb920 *)
