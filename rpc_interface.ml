(*  rpc2 -- a stack-based RPN calculator for the console
 *  Copyright (C) 2003  Paul Pelzl
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Please send bug reports, patches, etc. to Paul Pelzl at 
 *  <pelzlpj@eecs.umich.edu>.
 *)


open Curses;;
open Printf;;
open Rpc_calc;;
open Rpc_stack;;
open Complex;;
open Big_int;;


(* help_win is provided as an option, because it may be dropped if
 * the screen width is too small *)
type rpc_interface_screen = {stdscr:window; lines:int; cols:int;
   help_win:window option; hw_lines:int; hw_cols:int;
   stack_win:window; sw_lines:int; sw_cols:int; 
   entry_win:window; ew_lines:int; ew_cols:int};;

type rpc_interface_help_mode = | Standard | Extended;;
type rpc_entry_type          = | IntEntry | FloatEntry | ComplexEntry 
                               | FloatMatrixEntry | ComplexMatrixEntry;;

type complex_entry_element = 
   {mutable re_mantissa : string; mutable re_exponent : string; mutable re_has_dot : bool; 
    mutable im_mantissa : string; mutable im_exponent : string; mutable im_has_dot : bool};;


type rpc_bindings = {
   begin_int           : int;
   begin_complex       : int;
   begin_matrix        : int;
   separator           : int;
   enter               : int;
   scientific_notation : int;
   neg                 : int;
   add                 : int;
   sub                 : int;
   mult                : int;
   div                 : int;
   drop                : int;
   swap                : int;
   clear               : int
};;


let bindings = {
   begin_int           = int_of_char '#';
   begin_complex       = int_of_char '(';
   begin_matrix        = int_of_char '[';
   separator           = int_of_char ',';
   enter               = 10;   (* standard enter key *)
   scientific_notation = int_of_char ' ';
   neg                 = int_of_char 'n';
   add                 = int_of_char '+';
   sub                 = int_of_char '-';
   mult                = int_of_char '*';
   div                 = int_of_char '/';
   drop                = int_of_char '\\';
   swap                = Key.ppage;
   clear               = int_of_char '|'
};;


let max_matrix_size = 1000;;

class rpc_interface (c : rpc_calc) (std : rpc_interface_screen) =
object(self)
   val version = "0.10"
   val calc = c
   val mutable scr = std                      (* curses screen with two or three subwindows *)
   val mutable stack_bottom_row = 1           (* controls what portion of the stack is viewable *)
   val mutable help_mode = Extended           (* controls the mode of context-sensitive help *)

   val mutable has_entry = false              (* whether or not the entry buffer has anything in it *)
   val mutable is_extended_entry = false      (* is the current entry "extended" or not? *)
   val mutable entry_type = FloatEntry        (* the current type of data being entered *)
   val mutable int_entry_buffer = ""          (* holds characters entered for int data type *)
   val mutable is_entering_base = false       (* whether or not the user is entering a base *)
   val mutable int_base_string = ""           (* one-character representation of the base *)
   val mutable is_entering_exponent = false   (* whether or not the user is entering a scientific notation exponent *)

   (* Holds a list of complex_entry_elements used for float, complex, and matrix
      types.  Each element has string storage (and other bits) that can hold the
      state of a single complex number. *)
   val mutable gen_buffer = Array.make max_matrix_size
      {re_mantissa = ""; re_exponent = ""; re_has_dot = false; 
      im_mantissa = ""; im_exponent = ""; im_has_dot = false}
   val mutable current_buffer = 0
   val mutable is_entering_imag = false
   val mutable matrix_cols = max_matrix_size
                                               

   method run () =
      wclear scr.stack_win;
      wclear scr.entry_win;
      calc#enter_int (big_int_of_string "5");
      calc#enter_cmpx {Complex.re=1.5; Complex.im=2.4};
      calc#enter_cmpx {Complex.re=(-3.5); Complex.im=1.0};
      calc#enter_float 12.5;
      calc#enter_int (big_int_of_string "2");
      calc#enter_fmat (Gsl_matrix.of_array [|1.0; 2.0; 3.0; 4.0; 5.0; 6.0|] 3 2);
      calc#enter_fmat (Gsl_matrix.of_array [|7.0; 8.0; 9.0; 10.0; 11.0; 12.0|] 2 3);

      calc#enter_cmat (Gsl_matrix_complex.of_array [| {re=1.0; im=1.0}; {re=2.0;
         im=2.0}; {re=3.0; im=3.0}; {re=4.0; im=4.0}; {re=5.0; im=5.0}; {re=6.0;
         im=6.0} |] 3 2);
      calc#enter_cmat (Gsl_matrix_complex.of_array [| {re=2.3; im=3.1}; 
         {re=0.2; im=4.0}; {re=(-3.0); im=10.0}; {re=(-14.0); im=0.0} |] 2 2);


      (* initialize buffers for matrix entry *)
      for i = 1 to pred max_matrix_size do
         gen_buffer.(i) <- 
            {re_mantissa = ""; re_exponent = ""; re_has_dot = false; 
            im_mantissa = ""; im_exponent = ""; im_has_dot = false}
      done;
      self#draw_stack ();
      self#draw_help ();
      self#draw_entry ();
      self#do_main_loop ()
        

   (* display the stack, where the bottom line of the display
    * corresponds to stack level 'stack_bottom_row' *)
   method draw_stack () =
      for line = stack_bottom_row to pred (stack_bottom_row + scr.sw_lines) do
         let s = calc#get_display_line line in
         let len = String.length s in
         assert (wmove scr.stack_win (scr.sw_lines - line) 0);
         wclrtoeol scr.stack_win;
         if len > scr.sw_cols - 7 then
            (* need to truncate the string *)
            let sub_s = String.sub s 0 (scr.sw_cols - 11) in 
            let line_string = sprintf "%2d:   %s ..." line sub_s in
            assert (waddstr scr.stack_win line_string)
         else
            let spacer = String.make (scr.sw_cols - 7 - len) ' ' in
            let line_num = sprintf "%2d:   " line in
            let line_string = line_num ^ spacer ^ s in
            assert (waddstr scr.stack_win line_string)
      done;
      assert (wrefresh scr.stack_win)



   (* display the data that the user is in the process of entering *)
   method draw_entry () =
      assert (mvwaddstr scr.entry_win 0 0 (String.make scr.ew_cols '-'));
      assert (wmove scr.entry_win 1 0);
      wclrtoeol scr.entry_win;
      (* safely draw a string into the entry window, with "..." when
       * truncation occurs *)
      let draw_entry_string str =
         let len_str = String.length str in
         begin
            if len_str > scr.ew_cols - 1 then
               let trunc_str = String.sub str (len_str - scr.ew_cols + 5) (scr.ew_cols - 5) in
               assert (mvwaddstr scr.entry_win 1 0 ("... " ^ trunc_str))
            else
               assert (mvwaddstr scr.entry_win 1 (scr.ew_cols - len_str - 1) str)
         end;
         assert (wrefresh scr.entry_win)
      in
      (* draw a string for a single floating-point number *)
      let get_float_str is_current mantissa exponent =
         let sign_space =
            if String.length exponent > 0 then
               match exponent.[0] with
               |'-' -> ""
               |'+' -> ""
               |_ -> " "
            else
               " "
         in
         if (is_current && is_entering_exponent) || String.length exponent > 0 then
            mantissa ^ " x10^" ^ sign_space ^ exponent
         else
            mantissa
      in
      match entry_type with
      |IntEntry ->
         if is_entering_base then
            let s = "# " ^ int_entry_buffer ^ " " ^ int_base_string in
            draw_entry_string s
         else
            let s = "# " ^ int_entry_buffer in
            draw_entry_string s
      |FloatEntry ->
         let mantissa_str = gen_buffer.(0).re_mantissa and
         exponent_str = gen_buffer.(0).re_exponent in
         draw_entry_string (get_float_str true mantissa_str exponent_str)
      |ComplexEntry ->
         let buffer = gen_buffer.(0) in
         if is_entering_imag then
            let temp = get_float_str false buffer.re_mantissa buffer.re_exponent in
            let re_str = 
               if String.length temp > 0 then temp
               else "0"
            in
            let im_str = get_float_str true buffer.im_mantissa buffer.im_exponent in
            draw_entry_string ("(" ^ re_str ^ ", " ^ im_str ^ ")")
         else
            let re_str = get_float_str true buffer.re_mantissa buffer.re_exponent in
            draw_entry_string ("(" ^ re_str ^ ")")
      |FloatMatrixEntry ->
         let ss = ref "[[" in
         let matrix_rows = succ (current_buffer / matrix_cols) in
         begin
            for el = 0 to pred current_buffer do
               (if (el mod matrix_cols) = 0 && el > 0 then
                  ss := !ss ^ "]["
               else
                  ());
               let temp_re = get_float_str false gen_buffer.(el).re_mantissa
               gen_buffer.(el).re_exponent in
               ss := !ss ^ temp_re ^ ", "
            done;
            let temp_re = get_float_str true gen_buffer.(current_buffer).re_mantissa
            gen_buffer.(current_buffer).re_exponent in
            ss := !ss ^ temp_re ^ "]]";
            draw_entry_string !ss
         end
      |ComplexMatrixEntry ->
         let ss = ref "[[" in
         let matrix_rows = succ (current_buffer / matrix_cols) in
         for el = 0 to pred current_buffer do
            (if (el mod matrix_cols) = 0 && el > 0 then
               ss := !ss ^ "]["
            else
               ());
            let temp_re = get_float_str false gen_buffer.(el).re_mantissa
            gen_buffer.(el).re_exponent and
            temp_im = get_float_str false gen_buffer.(el).im_mantissa
            gen_buffer.(el).im_exponent in
            ss := !ss ^ "(" ^ temp_re ^ ", " ^ temp_im ^ "), "
         done;
         (if is_entering_imag then
            let temp_re = get_float_str false gen_buffer.(current_buffer).re_mantissa
            gen_buffer.(current_buffer).re_exponent and
            temp_im = get_float_str true gen_buffer.(current_buffer).im_mantissa
            gen_buffer.(current_buffer).im_exponent in
            ss := !ss ^ "(" ^ temp_re ^ ", " ^ temp_im ^ ")]]"
         else
            let temp_re = get_float_str true gen_buffer.(current_buffer).re_mantissa
            gen_buffer.(current_buffer).re_exponent in
            ss := !ss ^ "(" ^ temp_re ^ ")]]");
         draw_entry_string !ss



   (* display the help window *)
   method draw_help () =
      let modes = calc#get_modes () in
      match scr.help_win with
      |Some win ->
         wclear win;
         wattron win WA.bold;
         let s = sprintf "rpc2 v%s  --  Press '?' for help" version in
         assert (mvwaddstr win 0 0 s);
         wattroff win WA.bold;
         assert (mvwaddstr win 1 0 "--------------------------------------");
         for i = 0 to pred scr.hw_lines do
            assert (mvwaddch win i 38 (int_of_char '|'))
         done;
         wattron win WA.bold;
         assert (mvwaddstr win 2 0 "Calculator Modes:");
         assert (mvwaddstr win 3 2 "angle:      base:      complex:");
         wattroff win WA.bold;
         let angle_str = match modes.angle with
         |Rad -> "RAD"
         |Deg -> "DEG" in
         assert (mvwaddstr win 3 9 angle_str);
         let base_str = match modes.base with
         |Bin -> "BIN"
         |Oct -> "OCT"
         |Hex -> "HEX"
         |Dec -> "DEC" in
         assert (mvwaddstr win 3 20 base_str);
         let complex_str = match modes.complex with
         |Rect -> "REC"
         |Polar -> "POL" in
         assert (mvwaddstr win 3 34 complex_str);
         begin
            match help_mode with
            |Standard ->
               wattron win WA.bold;
               assert (mvwaddstr win 5 0 "Default Hotkeys:");
               wattroff win WA.bold;
               assert (mvwaddstr win 6 2 "<Enter> : enter number on stack");
               assert (mvwaddstr win 7 2 "      \\ : drop last stack item");
               assert (mvwaddstr win 8 2 " <PgDn> : swap last two stack items");
               assert (mvwaddstr win 9 2 "      | : clear stack");
               assert (mvwaddstr win 10 2 "+ : add               - : subtract");
               assert (mvwaddstr win 11 2 "* : multiply          / : divide");
               assert (mvwaddstr win 12 2 "^ : power (x^y)       ! : factorial");
               assert (mvwaddstr win 13 2 "% : mod");
               wattron win WA.bold;
               assert (mvwaddstr win 15 0 "Miscellaneous:");
               wattroff win WA.bold;
               assert (mvwaddstr win 16 2 "<Right> : change sign");
               assert (mvwaddstr win 17 2 "<Space> : scientific notation");
               assert (mvwaddstr win 18 2 "      ' : begin extended command");
               assert (mvwaddstr win 19 2 "   <Up> : enter stack browsing mode");
               assert (mvwaddstr win 20 2 "     ^L : refresh display");
               assert (mvwaddstr win 21 2 "      Q : quit");
               assert (wrefresh win)
            |Extended ->
               wattron win WA.bold;
               assert (mvwaddstr win 5 0 "Extended Commands:");
               wattroff win WA.bold;
               assert (mvwaddstr win 6 1 "Functions:");
               assert (mvwaddstr win 7 2 "sin   asin  cos   acos  tan   atan");
               assert (mvwaddstr win 8 2 "exp   ln    10^x  log10 sq    sqrt");
               assert (mvwaddstr win 9 2 "sinh  asinh cosh  acosh tanh  atanh");
               assert (mvwaddstr win 10 2 "erf   erfc  inv   gamma lngamma");
               assert (mvwaddstr win 11 2 "real  imag  floor ceil  nearint");
               assert (mvwaddstr win 13 1 "Change Modes:");
               assert (mvwaddstr win 14 2 "rad   deg   bin   oct   hex   dec");
               assert (mvwaddstr win 15 2 "rect  polar");
               assert (mvwaddstr win 17 1 "Miscellaneous:");
               assert (mvwaddstr win 18 2 "pi   undo  view");
               assert (mvwaddstr win 20 1 "<Backspace> : exit extended entry");
               assert (wrefresh win)
         end
      |None ->
         ()


   (* write an error message to the stack window *)
   method draw_error msg =
      self#draw_stack ();
      let error_lines = Utility.wordwrap ("Error: " ^ msg) (scr.sw_cols-2) in
      let trunc_error_lines = 
         if List.length error_lines > 4 then
            (List.nth error_lines 0) :: (List.nth error_lines 1) ::
            (List.nth error_lines 2) :: (List.nth error_lines 3) :: [] 
         else
            error_lines 
      in
      for i = 0 to pred (List.length trunc_error_lines) do
         assert (wmove scr.stack_win i 0);
         wclrtoeol scr.stack_win;
         assert (mvwaddstr scr.stack_win i 1 (List.nth trunc_error_lines i))
      done;
      let s = String.make scr.sw_cols '-' in
      assert (mvwaddstr scr.stack_win (List.length trunc_error_lines) 0 s);
      assert (wrefresh scr.stack_win)



   (* parse the entry buffers to obtain an rpc object *)
   method private get_entry_from_buffer () =
      match entry_type with
      |IntEntry ->
         let base_mode = 
            if is_entering_base then
               match int_base_string with
               |"b" -> Bin
               |"o" -> Oct
               |"d" -> Dec
               |"h" -> Hex
               |_ -> (calc#get_modes ()).base
            else
               (calc#get_modes ()).base
         in
         let base = match base_mode with
         |Bin -> 2
         |Oct -> 8
         |Dec -> 10
         |Hex -> 16 in
         begin
            try
               let ival = Big_int_str.big_int_of_string_base int_entry_buffer base in
               RpcInt ival
            with Big_int_str.Big_int_string_failure error_msg ->
               raise (Invalid_argument error_msg)
         end
      |FloatEntry ->
         begin
            let buffer = gen_buffer.(0) in
            try
               if is_entering_exponent && String.length buffer.re_exponent > 0 then
                  let ff = float_of_string (buffer.re_mantissa ^ "e" ^
                  buffer.re_exponent) in
                  RpcFloat ff
               else
                  let ff = float_of_string buffer.re_mantissa in
                  RpcFloat ff
            with Failure "float_of_string" ->
               raise (Invalid_argument "improperly formatted floating point data")
         end
      |ComplexEntry ->
         begin
            let buffer = gen_buffer.(0) in
            try
               let real_part =
                  if String.length buffer.re_mantissa > 0 then
                     if String.length buffer.re_exponent > 0 then
                        float_of_string (buffer.re_mantissa ^ "e" ^
                        buffer.re_exponent)
                     else
                        float_of_string buffer.re_mantissa
                  else 0.0
               and imag_part =
                  if String.length buffer.im_mantissa > 0 then
                     if String.length buffer.im_exponent > 0 then
                        float_of_string (buffer.im_mantissa ^ "e" ^
                        buffer.im_exponent)
                     else
                        float_of_string buffer.im_mantissa
                  else
                     0.0
               in
               RpcComplex {re = real_part; im = imag_part}
            with Failure "float_of_string" ->
               raise (Invalid_argument "improperly formatted floating point data")
         end
      |_ ->
         (* handle entry of other data types *)
         failwith "entry of unhandled data type"


   (* parse the entry buffers to obtain an rpc object, then stack#push it *)
   method push_entry () =
      (* perform this cleanup routine after every entry, so we are prepared
         to receive a new object. *)
      let post_entry_cleanup () =
         has_entry <- false;
         entry_type <- FloatEntry;
         int_entry_buffer <- "";
         int_base_string <- "";
         is_entering_base <- false;
         is_entering_exponent <- false;
         for i = 0 to pred max_matrix_size do
            gen_buffer.(i) <- 
               {re_mantissa = ""; re_exponent = ""; re_has_dot = false; 
               im_mantissa = ""; im_exponent = ""; im_has_dot = false}
         done;
         current_buffer <- 0;
         is_entering_imag <- false;
         matrix_cols <- max_matrix_size;
         self#draw_entry ()
      in
      begin
         try
            calc#push (self#get_entry_from_buffer ())
         with
            Invalid_argument error_msg ->
               (post_entry_cleanup ();
               (* raise the EXN again, so it can be caught within do_main_loop *)
               raise (Invalid_argument error_msg))
      end;
      post_entry_cleanup ()



   (* accept and process input *)
   method do_main_loop () =
      while true do
         let key = getch () in
         if is_extended_entry then
            (* do something here *)
            ()
         else
            begin
(*               Printf.fprintf stdout "key = %d\n" key;
               flush stdout; *)
               if key = bindings.enter then
                  try
                     (if has_entry then
                        self#push_entry ()
                     else
                        calc#dup ());
                     self#draw_stack ()
                  with Invalid_argument error_msg ->
                     self#draw_error error_msg
               else if key = bindings.begin_int then
                  if entry_type = FloatEntry then
                     (entry_type <- IntEntry;
                     int_entry_buffer <- "";
                     self#draw_entry ())
                  else
                     () 
               else if key = bindings.begin_complex then
                  (has_entry <- true;
                  match entry_type with
                  |FloatEntry ->
                     (entry_type <- ComplexEntry;
                     self#draw_entry ())
                  |FloatMatrixEntry ->
                     (entry_type <- ComplexMatrixEntry;
                     self#draw_entry ())
                  |_ ->
                     ())
               else if key = bindings.begin_matrix then
                  (has_entry <- true;
                  match entry_type with
                  |FloatEntry ->
                     (entry_type <- FloatMatrixEntry;
                     self#draw_entry ())
                  |ComplexEntry ->
                     (entry_type <- ComplexMatrixEntry;
                     self#draw_entry ())
                  (* FIXME: |FloatMatrixEntry -> (next row) *)
                  |_ ->
                     ())
               else if key = bindings.separator then
                  match entry_type with
                  |ComplexEntry ->
                     if not is_entering_imag then
                        (is_entering_imag <- true;
                        is_entering_exponent <- false;
                        self#draw_entry ())
                     else
                        ()
                  |FloatMatrixEntry ->
                     if current_buffer < pred max_matrix_size then
                        (current_buffer <- succ current_buffer;
                        is_entering_exponent <- false;
                        (*FIXME: any other items to reset here?*)
                        self#draw_entry ())
                     else
                        ()
                  |ComplexMatrixEntry ->
                     if is_entering_imag then
                        if current_buffer < pred max_matrix_size then
                           (current_buffer <- succ current_buffer;
                           is_entering_exponent <- false;
                           is_entering_imag <- false;
                           (*FIXME: any other items to reset here?*)
                           self#draw_entry ())
                        else
                           ()
                     else
                        (is_entering_imag <- true;
                        is_entering_exponent <- false;
                        self#draw_entry ())
                  |_ ->
                     ()
               else if key = bindings.scientific_notation then
                  match entry_type with 
                  |IntEntry ->
                     if String.length int_entry_buffer > 0 then
                        (is_entering_base <- true;
                        self#draw_entry ())
                     else
                        ()
                  |FloatEntry | FloatMatrixEntry ->
                     if String.length gen_buffer.(current_buffer).re_mantissa > 0 then
                        (is_entering_exponent <- true;
                        self#draw_entry ())
                     else
                        ()
                  |ComplexEntry | ComplexMatrixEntry ->
                     if is_entering_imag then
                        if String.length gen_buffer.(current_buffer).im_mantissa > 0 then
                           (is_entering_exponent <- true;
                           self#draw_entry ())
                        else
                           ()
                     else
                        if String.length gen_buffer.(0).re_mantissa > 0 then
                           (is_entering_exponent <- true;
                           self#draw_entry ())
                        else
                           ()
               else if key = bindings.neg then
                  if has_entry then
                     match entry_type with
                     |IntEntry ->
                        (begin
                           match int_entry_buffer.[0] with
                           |'-' -> int_entry_buffer.[0] <- '+'
                           |'+' -> int_entry_buffer.[0] <- '-'
                           |_ -> int_entry_buffer <- "-" ^ int_entry_buffer
                        end;
                        self#draw_entry ())
                     |FloatEntry | FloatMatrixEntry ->
                        begin
                           let buffer = gen_buffer.(current_buffer) in
                           if is_entering_exponent then
                              if String.length buffer.re_exponent > 0 then
                                 match buffer.re_exponent.[0] with
                                 |'-' -> buffer.re_exponent.[0] <- '+'
                                 |'+' -> buffer.re_exponent.[0] <- '-'
                                 |_ -> buffer.re_exponent <- "-" ^ buffer.re_exponent
                              else
                                 ()
                           else
                              match buffer.re_mantissa.[0] with
                              |'-' -> buffer.re_mantissa.[0] <- '+'
                              |'+' -> buffer.re_mantissa.[0] <- '-'
                              |_ -> buffer.re_mantissa <- "-" ^ buffer.re_mantissa
                        end;
                        self#draw_entry ()
                     |ComplexEntry | ComplexMatrixEntry ->
                        begin
                           let buffer = gen_buffer.(current_buffer) in
                           let mantissa = 
                              if is_entering_imag then
                                 buffer.im_mantissa
                              else
                                 buffer.re_mantissa and
                           exponent = 
                              if is_entering_imag then
                                 buffer.im_exponent
                              else
                                 buffer.re_exponent
                           in
                           if is_entering_exponent then
                              if String.length exponent > 0 then
                                 match exponent.[0] with
                                 |'-' -> exponent.[0] <- '+'
                                 |'+' -> exponent.[0] <- '-'
                                 |_ -> 
                                    if is_entering_imag then
                                       buffer.im_exponent <- "-" ^ buffer.im_exponent
                                    else
                                       buffer.re_exponent <- "-" ^ buffer.re_exponent
                              else
                                 ()
                           else
                              if String.length mantissa > 0 then
                                 match mantissa.[0] with
                                 |'-' -> mantissa.[0] <- '+'
                                 |'+' -> mantissa.[0] <- '-'
                                 |_ -> 
                                    if is_entering_imag then
                                       buffer.im_mantissa <- "-" ^ buffer.im_mantissa
                                    else
                                       buffer.re_mantissa <- "-" ^ buffer.re_mantissa
                              else
                                 ()
                        end;
                        self#draw_entry ()
                  else
                     try calc#neg (); self#draw_stack ()
                     with Invalid_argument error_msg ->
                        self#draw_error error_msg
               else if key = bindings.add then
                  try 
                     (if has_entry then
                        self#push_entry ()
                     else
                        ());
                     calc#add (); 
                     self#draw_stack ()
                  with 
                     Invalid_argument error_msg ->
                        self#draw_error error_msg
               else if key = bindings.sub then
                  try 
                     (if has_entry then
                        self#push_entry ()
                     else
                        ());
                     calc#sub (); 
                     self#draw_stack ()
                  with 
                     Invalid_argument error_msg ->
                        self#draw_error error_msg
               else if key = bindings.mult then
                  try 
                     (if has_entry then
                        self#push_entry ()
                     else
                        ());
                     calc#mult (); 
                     self#draw_stack ()
                  with 
                     Invalid_argument error_msg ->
                        self#draw_error error_msg
               else if key = bindings.div then
                  try 
                     (if has_entry then
                        self#push_entry ()
                     else
                        ());
                     calc#div (); 
                     self#draw_stack ()
                  with 
                     Invalid_argument error_msg ->
                        self#draw_error error_msg
               else if key = bindings.drop then
                  try calc#drop (); self#draw_stack ()
                  with Invalid_argument error_msg ->
                     self#draw_error error_msg
               else if key = bindings.swap then
                  try calc#swap (); self#draw_stack ()
                  with Invalid_argument error_msg ->
                     self#draw_error error_msg
               else if key = bindings.clear then
                  try calc#clear (); self#draw_stack ()
                  with Invalid_argument error_msg ->
                     self#draw_error error_msg
               else (* handle entry of digits *)
                  let process_float_digit buffer is_imag =
                     let exponent_buffer =
                        if is_imag then buffer.im_exponent
                        else buffer.re_exponent
                     and mantissa_buffer =
                        if is_imag then buffer.im_mantissa
                        else buffer.re_mantissa
                     and has_dot =
                        if is_imag then buffer.im_has_dot
                        else buffer.re_has_dot
                     in
                     begin
                        if is_entering_exponent then
                           let explen =
                              (if String.length exponent_buffer > 0 then
                                 match exponent_buffer.[0] with
                                 |'-' | '+'-> 4
                                 |_ -> 3
                              else
                                 3)
                           in
                           if String.length exponent_buffer < explen then
                              let digits = "0123456789" in
                              try
                                 let c = char_of_int key in
                                 if String.contains digits c then
                                    (* need this hack to be able to perform
                                     * the mutable field assignment... *)
                                    (if is_imag then
                                       buffer.im_exponent <- exponent_buffer ^
                                       (String.make 1 c)
                                    else
                                       buffer.re_exponent <- exponent_buffer ^
                                       (String.make 1 c);
                                    self#draw_entry ())
                                 else
                                    ()
                              with Invalid_argument "char_of_int" ->
                                 ()
                           else
                              ()
                        else if String.length mantissa_buffer < 17 then
                           let digits =
                              if has_dot then "0123456789"
                              else "0123456789."
                           in
                           try
                              let c = char_of_int key in
                              if String.contains digits c then
                                 begin
                                    (if is_imag then
                                       (buffer.im_mantissa <- mantissa_buffer ^
                                       (String.make 1 c);
                                       if c = '.' then buffer.im_has_dot <- true
                                       else ())
                                    else
                                       (buffer.re_mantissa <- mantissa_buffer ^
                                       (String.make 1 c);
                                       if c = '.' then buffer.re_has_dot <- true
                                       else ()));
                                    has_entry <- true;
                                    self#draw_entry ()
                                 end
                              else
                                 ()
                           with Invalid_argument "char_of_int" ->
                              ()
                        else
                           ()
                     end
                  in
                  match entry_type with
                  |IntEntry ->
                     begin
                        if is_entering_base then
                           let base_chars = "bodh" in
                           try
                              let c = char_of_int key in
                              if String.contains base_chars c then
                                 (int_base_string <- String.make 1 c;
                                 self#draw_entry ())
                              else
                                 ()
                           with Invalid_argument "char_of_int" ->
                              ()
                        else
                           let digits = "0123456789abcdefABCDEF" in
                           try
                              let c = char_of_int key in
                              if String.contains digits c then
                                 (int_entry_buffer <- int_entry_buffer ^ (String.make 1 c);
                                 has_entry <- true;
                                 self#draw_entry ())
                              else
                                 ()
                           with Invalid_argument "char_of_int" ->
                              ()
                     end (* IntEntry *)
                  |FloatEntry | FloatMatrixEntry ->
                     let buffer = gen_buffer.(current_buffer) in
                     process_float_digit buffer false
                  |ComplexEntry | ComplexMatrixEntry ->
                     begin
                        let buffer = gen_buffer.(current_buffer) in
                        if is_entering_imag then
                           process_float_digit buffer true
                        else
                           process_float_digit buffer false
                     end
            end
      done


end;;




(* arch-tag: DO_NOT_CHANGE_b4519dd2-7e94-4cbf-931a-bb5f97445cbf *)
