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
open Operations;;

exception Not_handled;;

(* help_win is provided as an option, because it may be dropped if
 * the screen width is too small *)
type rpc_interface_screen = {stdscr:window; lines:int; cols:int;
   help_win:window option; hw_lines:int; hw_cols:int;
   stack_win:window; sw_lines:int; sw_cols:int; 
   entry_win:window; ew_lines:int; ew_cols:int};;

type rpc_interface_help_mode = | Standard | Extended;;
type rpc_entry_type          = | IntEntry | FloatEntry | ComplexEntry 
                               | FloatMatrixEntry | ComplexMatrixEntry;;

type rpc_interface_mode = | StandardEntryMode | ExtendedEntryMode | BrowsingMode;;

type complex_entry_element = 
   {mutable re_mantissa : string; mutable re_exponent : string;
    mutable im_mantissa : string; mutable im_exponent : string};;

let extended_commands =
   ("add\nsub\nmult\ndiv\nneg\ninv\npow\nsq\nsqrt\nabs\narg\nexp\nln\n" ^
    "10^\nlog10\nconj\nsin\ncos\ntan\nasin\nacos\natan\nsinh\ncosh\ntanh\n" ^
    "drop\nclear\nswap\ndup\nundo\nquit\nrad\ndeg\nrect\npolar\n");;

(* abbreviations used in extended entry mode *)
let command_abbrev_table = Hashtbl.create 30;;
Hashtbl.add command_abbrev_table "add" (Function Add);;
Hashtbl.add command_abbrev_table "sub" (Function Sub);;
Hashtbl.add command_abbrev_table "mult" (Function Mult);;
Hashtbl.add command_abbrev_table "div" (Function Div);;
Hashtbl.add command_abbrev_table "neg" (Function Neg);;
Hashtbl.add command_abbrev_table "inv" (Function Inv);;
Hashtbl.add command_abbrev_table "pow" (Function Pow);;
Hashtbl.add command_abbrev_table "sq" (Function Sq);;
Hashtbl.add command_abbrev_table "sqrt" (Function Sqrt);;
Hashtbl.add command_abbrev_table "abs" (Function Abs);;
Hashtbl.add command_abbrev_table "arg" (Function Arg);;
Hashtbl.add command_abbrev_table "exp" (Function Exp);;
Hashtbl.add command_abbrev_table "ln" (Function Ln);;
Hashtbl.add command_abbrev_table "10^" (Function Ten_x);;
Hashtbl.add command_abbrev_table "log10" (Function Log10);;
Hashtbl.add command_abbrev_table "conj" (Function Conj);;
Hashtbl.add command_abbrev_table "sin" (Function Sin);;
Hashtbl.add command_abbrev_table "cos" (Function Cos);;
Hashtbl.add command_abbrev_table "tan" (Function Tan);;
Hashtbl.add command_abbrev_table "sinh" (Function Sinh);;
Hashtbl.add command_abbrev_table "cosh" (Function Cosh);;
Hashtbl.add command_abbrev_table "tanh" (Function Tanh);;
Hashtbl.add command_abbrev_table "asin" (Function Asin);;
Hashtbl.add command_abbrev_table "acos" (Function Acos);;
Hashtbl.add command_abbrev_table "atan" (Function Atan);;
Hashtbl.add command_abbrev_table "drop" (Command Drop);;
Hashtbl.add command_abbrev_table "clear" (Command Clear);;
Hashtbl.add command_abbrev_table "swap" (Command Swap);;
Hashtbl.add command_abbrev_table "dup" (Command Dup);;
Hashtbl.add command_abbrev_table "undo" (Command Undo);;
Hashtbl.add command_abbrev_table "quit" (Command Quit);;
Hashtbl.add command_abbrev_table "rad" (Command SetRadians);;
Hashtbl.add command_abbrev_table "deg" (Command SetDegrees);;
Hashtbl.add command_abbrev_table "rect" (Command SetRect);;
Hashtbl.add command_abbrev_table "polar" (Command SetPolar);;
let translate_extended_abbrev abb =
   Hashtbl.find command_abbrev_table abb;;


let max_matrix_size = 1000;;

class rpc_interface (c : rpc_calc) (std : rpc_interface_screen) =
object(self)
   val version = "0.10"
   val calc = c
   val mutable scr = std                      (* curses screen with two or three subwindows *)
   val mutable run_calc = true                (* exit when run_true becomes false *)
   val mutable stack_bottom_row = 1           (* controls what portion of the stack is viewable *)
   val mutable stack_selection = 1            (* in stack browsing mode, this item is selected *)
   val mutable interface_mode = StandardEntryMode  (* standard mode or stack browsing mode *)
   val mutable horiz_scroll = 0
   val mutable help_mode = Extended           (* controls the mode of context-sensitive help *)

   val mutable has_entry = false              (* whether or not the entry buffer has anything in it *)
   val mutable is_extended_entry = false      (* is the current entry "extended" or not? *)
   val mutable entry_type = FloatEntry        (* the current type of data being entered *)
   val mutable int_entry_buffer = ""          (* holds characters entered for int data type *)
   val mutable is_entering_base = false       (* whether or not the user is entering a base *)
   val mutable int_base_string = ""           (* one-character representation of the base *)
   val mutable is_entering_exponent = false   (* whether or not the user is entering a scientific notation exponent *)
   val mutable extended_entry_buffer = ""     (* stores characters entered in extended entry mode *)
   val mutable matched_extended_entry = ""    (* stores the command-completed extended entry *)

   (* Holds a list of complex_entry_elements used for float, complex, and matrix
      types.  Each element has string storage (and other bits) that can hold the
      state of a single complex number. *)
   val mutable gen_buffer = Array.make max_matrix_size
      {re_mantissa = ""; re_exponent = "";
      im_mantissa = ""; im_exponent = ""}
   val mutable current_buffer = 0
   val mutable is_entering_imag = false
   val mutable matrix_cols = 1
   val mutable has_multiple_rows = false
                                               

   method run () =
      calc#backup ();
      Rcfile.process_rcfile ();
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
            {re_mantissa = ""; re_exponent = "";
            im_mantissa = ""; im_exponent = ""}
      done;
      self#draw_stack ();
      self#draw_help ();
      self#draw_entry ();
      self#do_main_loop ()
        

   (* display the stack, where the bottom line of the display
    * corresponds to stack level 'stack_bottom_row' *)
   method draw_stack () =
      let print_numbered_line l_num =
         let num_len = 
            String.length (string_of_int (pred (stack_bottom_row +
            scr.sw_lines)))
         in
         if num_len <= 2 then
            (sprintf "%2d:   %s" l_num)
         else if num_len = 3 then
            (sprintf "%3d:  %s" l_num)
         else if num_len = 4 then
            (sprintf "%4d: %s" l_num)
         else
            (* if the line number is really huge, truncate to least
             * significant digits *)
            let l_num_str = string_of_int l_num in
            let str_len = String.length l_num_str in
            let trunc_num = Str.string_after l_num_str (str_len - 4) in
            (sprintf "%s: %s" trunc_num)
      in
      for line = stack_bottom_row to pred (stack_bottom_row + scr.sw_lines) do
         fprintf stderr "fetching display line %d\n" line;
         flush stderr;
         let s = calc#get_display_line line in
         let len = String.length s in
         assert (wmove scr.stack_win 
         (scr.sw_lines + stack_bottom_row - 1 - line) 0);
         wclrtoeol scr.stack_win;
         begin
            if line = stack_selection && interface_mode = BrowsingMode then
               wattron scr.stack_win WA.reverse
            else
               ()
         end;
         begin
            if len > scr.sw_cols - 7 then
               (* need to truncate the string *)
               let line_string =
                  if line = stack_selection && interface_mode = BrowsingMode then
                     let sub_s = 
                        if horiz_scroll < len - scr.sw_cols + 7 then
                           String.sub s horiz_scroll (scr.sw_cols - 7)
                        else
                           String.sub s (len - scr.sw_cols + 7) (scr.sw_cols - 7)
                     in
                     print_numbered_line line sub_s
                  else
                     let sub_s = String.sub s 0 (scr.sw_cols - 10) in
                     print_numbered_line line (sub_s ^ "...")
               in
               assert (waddstr scr.stack_win line_string)
            else
               let spacer = String.make (scr.sw_cols - 7 - len) ' ' in
               let line_string = print_numbered_line line (spacer ^ s) in 
               assert (waddstr scr.stack_win line_string)
         end;
         begin
            if line = stack_selection && interface_mode = BrowsingMode then
               wattroff scr.stack_win WA.reverse
            else
               ()
         end
      done;
      assert (wrefresh scr.stack_win)



   (* display the data that the user is in the process of entering *)
   method draw_entry () =
      assert (mvwaddstr scr.entry_win 0 0 (String.make scr.ew_cols '-'));
      assert (wmove scr.entry_win 1 0);
      wclrtoeol scr.entry_win;
      (* Safely draw a string into the entry window, with "..." when
       * truncation occurs.  Highlight the first 'highlight_len'
       * characters. *)
      let draw_entry_string str highlight_len =
         let len_str = String.length str in
         begin
            if len_str > scr.ew_cols - 1 then
               let trunc_str = String.sub str (len_str - scr.ew_cols + 5) (scr.ew_cols - 5) in
               assert (mvwaddstr scr.entry_win 1 0 ("... " ^ trunc_str))
            else
               if highlight_len <= len_str then
                  begin
                     (* highlight the first 'highlight_len' characters *)
                     wattron scr.entry_win WA.bold;
                     assert (mvwaddstr scr.entry_win 1 (scr.ew_cols - len_str - 1)
                        (Str.string_before str (highlight_len)));
                     wattroff scr.entry_win WA.bold;
                     assert (mvwaddstr scr.entry_win 1 (scr.ew_cols - len_str -
                        1 + highlight_len) (Str.string_after str (highlight_len)))
                  end
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
         else if is_current || String.length mantissa > 0 then
            mantissa
         else 
            "0"
      in
      (* get a string representation of the data that is in the entry buffer *)
      let data_string =
         match entry_type with
         |IntEntry ->
            if is_entering_base then
               "# " ^ int_entry_buffer ^ " " ^ int_base_string
            else
               "# " ^ int_entry_buffer
         |FloatEntry ->
            let mantissa_str = gen_buffer.(0).re_mantissa and
            exponent_str = gen_buffer.(0).re_exponent in
            get_float_str true mantissa_str exponent_str
         |ComplexEntry ->
            let buffer = gen_buffer.(0) in
            if is_entering_imag then
               let temp = get_float_str false buffer.re_mantissa buffer.re_exponent in
               let re_str = 
                  if String.length temp > 0 then temp
                  else "0"
               in
               let im_str = get_float_str true buffer.im_mantissa buffer.im_exponent in
               "(" ^ re_str ^ ", " ^ im_str ^ ")"
            else
               let re_str = get_float_str true buffer.re_mantissa buffer.re_exponent in
               "(" ^ re_str ^ ")"
         |FloatMatrixEntry ->
            let ss = ref "[[" in
            begin
               for el = 0 to pred current_buffer do
                  let temp_re = get_float_str false gen_buffer.(el).re_mantissa
                  gen_buffer.(el).re_exponent in
                  (if has_multiple_rows && ((succ el) mod matrix_cols) = 0 then
                     ss := !ss ^ temp_re ^ "]["
                  else
                     ss := !ss ^ temp_re ^ ", ")
               done;
               let temp_re = get_float_str true gen_buffer.(current_buffer).re_mantissa
               gen_buffer.(current_buffer).re_exponent in
               ss := !ss ^ temp_re ^ "]]";
               !ss
            end
         |ComplexMatrixEntry ->
            let ss = ref "[[" in
            for el = 0 to pred current_buffer do
               let temp_re = get_float_str false gen_buffer.(el).re_mantissa
               gen_buffer.(el).re_exponent and
               temp_im = get_float_str false gen_buffer.(el).im_mantissa
               gen_buffer.(el).im_exponent in
               (if has_multiple_rows && ((succ el) mod matrix_cols) = 0 then
                  ss := !ss ^ "(" ^ temp_re ^ ", " ^ temp_im ^ ")]["
               else
                  ss := !ss ^ "(" ^ temp_re ^ ", " ^ temp_im ^ "), ")
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
            !ss
      in
      match interface_mode with
      |StandardEntryMode ->
         draw_entry_string data_string 0
      |ExtendedEntryMode ->
         let highlight_len = String.length extended_entry_buffer in
         if highlight_len = 0 then
            draw_entry_string "<enter extended command>" 0
         else
            let is_function =
               match (translate_extended_abbrev matched_extended_entry) with
               |Function ff -> true
               |_ -> false
            in
            if is_function then
               draw_entry_string (matched_extended_entry ^ 
               "( )") highlight_len
            else
               draw_entry_string matched_extended_entry highlight_len
      |BrowsingMode ->
         ()


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
      (* get a float from strings representing the mantissa and exponent *)
      let get_float_el mantissa exponent =
         if String.length mantissa > 0 then
            if String.length exponent > 0 then
               if exponent = "-" || exponent = "+" then
                  float_of_string mantissa
               else
                  float_of_string (mantissa ^ "e" ^ exponent)
            else
               float_of_string mantissa
         else
            0.0
      in
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
               let ff = get_float_el buffer.re_mantissa buffer.re_exponent in
               RpcFloat ff
            with Failure "float_of_string" ->
               raise (Invalid_argument "improperly formatted floating-point data")
         end
      |ComplexEntry ->
         begin
            let buffer = gen_buffer.(0) in
            try
               let real_part = get_float_el buffer.re_mantissa
               buffer.re_exponent and
               imag_part = get_float_el buffer.im_mantissa
               buffer.im_exponent in
               RpcComplex {re = real_part; im = imag_part}
            with Failure "float_of_string" ->
               raise (Invalid_argument "improperly formatted complex floating-point data")
         end
      |FloatMatrixEntry ->
         begin
            let matrix_rows = 
               if has_multiple_rows then
                  succ (current_buffer / matrix_cols)
               else
                  1
            in
            let temp_arr = Array.make (matrix_rows * matrix_cols) 0.0 in
            try
               for i = 0 to current_buffer do
                  temp_arr.(i) <- (get_float_el gen_buffer.(i).re_mantissa
                     gen_buffer.(i).re_exponent)
               done;
               RpcFloatMatrix (Gsl_matrix.of_array temp_arr matrix_rows matrix_cols)
            with Failure "float_of_string" ->
               raise (Invalid_argument "improperly formatted floating-point matrix data")
         end
      |ComplexMatrixEntry ->
         begin
            let matrix_rows = 
               if has_multiple_rows then
                  succ (current_buffer / matrix_cols)
               else
                  1
            in
            let temp_arr = Array.make (matrix_rows * matrix_cols) 
            {re = 0.0; im = 0.0} in
            try
               for i = 0 to current_buffer do
                  let re_part = get_float_el gen_buffer.(i).re_mantissa
                  gen_buffer.(i).re_exponent and
                  im_part = get_float_el gen_buffer.(i).im_mantissa
                  gen_buffer.(i).im_exponent in
                  temp_arr.(i) <- {re = re_part; im = im_part}
               done;
               RpcComplexMatrix (Gsl_matrix_complex.of_array temp_arr matrix_rows matrix_cols)
            with Failure "float_of_string" ->
               raise (Invalid_argument "improperly formatted complex
               floating-point matrix data")
         end



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
               {re_mantissa = ""; re_exponent = "";
               im_mantissa = ""; im_exponent = ""}
         done;
         current_buffer <- 0;
         is_entering_imag <- false;
         matrix_cols <- 1;
         has_multiple_rows <- false;
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


   method private process_function ff =
      begin
         match ff with
         |Add ->
            self#handle_function_call calc#add
         |Sub ->
            self#handle_function_call calc#sub
         |Mult ->
            self#handle_function_call calc#mult
         |Div ->
            self#handle_function_call calc#div
         |Neg ->
            self#handle_function_call calc#neg
         |Inv ->
            self#handle_function_call calc#inv
         |Pow ->
            self#handle_function_call calc#pow
         |Sq ->
            self#handle_function_call calc#sq
         |Sqrt ->
            self#handle_function_call calc#sqrt
         |Abs ->
            self#handle_function_call calc#abs
         |Arg ->
            self#handle_function_call calc#arg
         |Exp ->
            self#handle_function_call calc#exp
         |Ln ->
            self#handle_function_call calc#ln
         |Ten_x ->
            self#handle_function_call calc#ten_pow_x
         |Log10 ->
            self#handle_function_call calc#log10
         |Conj ->
            self#handle_function_call calc#conj
         |Sin ->
            self#handle_function_call calc#sin
         |Cos ->
            self#handle_function_call calc#cos
         |Tan ->
            self#handle_function_call calc#tan
         |Sinh ->
            self#handle_function_call calc#sinh
         |Cosh ->
            self#handle_function_call calc#cosh
         |Tanh ->
            self#handle_function_call calc#tanh
         |Asin ->
            self#handle_function_call calc#asin
         |Acos ->
            self#handle_function_call calc#acos
         |Atan ->
            self#handle_function_call calc#atan
      end


   method private process_command cc =
      begin
         match cc with
         |Drop ->
            self#handle_command_call calc#drop
         |Clear ->
            self#handle_command_call calc#clear
         |Swap ->
            self#handle_command_call calc#swap
         |Dup ->
            self#handle_command_call calc#dup
         |Undo ->
            self#handle_command_call calc#undo
         |BeginBrowse ->
            self#handle_begin_browse ()
         |BeginExtended ->
            self#handle_begin_extended ()
         |Quit ->
            self#handle_quit ()
         |SetRadians ->
            self#handle_command_call calc#mode_rad;
            self#draw_help ();
            self#draw_stack ()
         |SetDegrees ->
            self#handle_command_call calc#mode_deg;
            self#draw_help ();
            self#draw_stack ()
         |SetRect ->
            self#handle_command_call calc#mode_rect;
            self#draw_help ();
            self#draw_stack ()
         |SetPolar ->
            self#handle_command_call calc#mode_polar;
            self#draw_help ();
            self#draw_stack ()

      end


   method do_main_loop () =
      while run_calc do
         let key = getch () in
         match interface_mode with
         |StandardEntryMode ->
            begin
            (* editing operations take priority *)
            try 
               let edit_op = Rcfile.edit_of_key key in
               match edit_op with
               |Edit ee ->
                  begin
                     match ee with
                     |Digit ->
                        self#handle_digit key
                     |Enter ->
                        self#handle_enter ()
                     |Backspace ->
                        self#handle_backspace ()
                     |Minus ->
                        self#handle_minus ()
                     |BeginInteger ->
                        self#handle_begin_int ()
                     |BeginComplex ->
                        self#handle_begin_complex ()
                     |BeginMatrix ->
                        self#handle_begin_matrix ()
                     |Separator ->
                        self#handle_separator ()
                     |SciNotBase ->
                        self#handle_scientific_notation ()
                  end
               |_ ->
                  failwith "Non-Edit operation found in Edit Hashtbl"
            with Not_found | Not_handled ->
               (* next we try to match on functions *)
               try 
                  let function_op = Rcfile.function_of_key key in
                  match function_op with
                  |Function ff ->
                     self#process_function ff
                  |_ ->
                     failwith "Non-Function operation found in Function Hashtbl"
               with Not_found ->
                  if has_entry then
                     (* finally we try entry of digits *)
                     self#handle_digit key
                  else
                     (* commands are only suitable when there is no entry *)
                     try 
                        let command_op = Rcfile.command_of_key key in
                        match command_op with
                        |Command cc ->
                           self#process_command cc
                        |_ ->
                           failwith "Non-Command operation found in Command Hashtbl"
                     with Not_found ->
                        self#handle_digit key
            end
         |ExtendedEntryMode ->
            begin
            (* check to see whether the user is either exiting extended mode or
             * is applying the extended command *)
            try
               let extended_op = Rcfile.extended_of_key key in
               match extended_op with
               |Extend ee ->
                  begin
                     match ee with
                     |ExitExtended ->
                        self#handle_exit_extended ()
                     |EnterExtended ->
                        self#handle_enter_extended ()
                     |ExtBackspace ->
                        self#handle_extended_backspace ()
                  end
               |_ ->
                  failwith "Non-Extended command found in Extended Hashtbl"
            with Not_found ->
               self#handle_extended_character key
            end
         |BrowsingMode ->
            try
               let browse_op = Rcfile.browse_of_key key in
               match browse_op with
               |Browse bb ->
                  begin
                     match bb with
                     |EndBrowse ->
                        self#handle_end_browse ()
                     |ScrollLeft ->
                        self#handle_scroll_left ()
                     |ScrollRight ->
                        self#handle_scroll_right ()
                     |RollDown ->
                        self#handle_rolldown ()
                     |RollUp ->
                        self#handle_rollup ()
                     |PrevLine ->
                        self#handle_prev_line ()
                     |NextLine ->
                        self#handle_next_line ()
                     |Echo ->
                        self#handle_browse_echo ()
                  end
               |_ ->
                  failwith "Non-Browsing operation found in Browse Hashtbl"
            with Not_found | Not_handled ->
               ()
      done


   (* handle an 'enter' keypress *)
   method private handle_enter () =
      try
         (if has_entry then
            self#push_entry ()
         else
            raise Not_handled);
         self#draw_stack ()
      with Invalid_argument error_msg ->
         self#draw_error error_msg


   (* handle a 'begin_int' keypress *)
   method private handle_begin_int () =
      if entry_type = FloatEntry then
         (entry_type <- IntEntry;
         int_entry_buffer <- "";
         self#draw_entry ())
      else
         () 


   (* handle a 'begin_complex' keypress *)
   method private handle_begin_complex () =
      has_entry <- true;
      match entry_type with
      |FloatEntry ->
         (entry_type <- ComplexEntry;
         self#draw_entry ())
      |FloatMatrixEntry ->
         (entry_type <- ComplexMatrixEntry;
         self#draw_entry ())
      |_ ->
         ()


   (* handle a 'begin_matrix' keypress *)
   method private handle_begin_matrix () =
      has_entry <- true;
      match entry_type with
      |FloatEntry ->
         (entry_type <- FloatMatrixEntry;
         self#draw_entry ())
      |ComplexEntry ->
         (entry_type <- ComplexMatrixEntry;
         self#draw_entry ())
      |FloatMatrixEntry ->
         if not has_multiple_rows then
            (has_multiple_rows <- true;
            current_buffer <- succ current_buffer;
            matrix_cols <- current_buffer;
            is_entering_exponent <- false;
            self#draw_entry ())
         else if (succ current_buffer) mod matrix_cols = 0 then
            (current_buffer <- succ current_buffer;
            is_entering_exponent <- false;
            (*FIXME: any other items to reset here?*)
            self#draw_entry ())
         else
            ()
      |ComplexMatrixEntry ->
         if not has_multiple_rows then
            (has_multiple_rows <- true;
            current_buffer <- succ current_buffer;
            matrix_cols <- current_buffer;
            is_entering_exponent <- false;
            is_entering_imag <- false;
            self#draw_entry ())
         else if (succ current_buffer) mod matrix_cols = 0 then
            (current_buffer <- succ current_buffer;
            is_entering_exponent <- false;
            is_entering_imag <- false;
            (*FIXME: any other items to reset here?*)
            self#draw_entry ())
         else
            ()
      |_ ->
         ()
      

   (* handle a 'separator' keypress *)
   method private handle_separator () =
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
            (if not has_multiple_rows then
               matrix_cols <- succ current_buffer
            else
               ());
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
               (if not has_multiple_rows then
                  matrix_cols <- succ current_buffer
               else
                  ());
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


   (* handle a 'backspace' keypress *)
   method private handle_backspace () =
      let buffer = gen_buffer.(current_buffer) in
      match entry_type with
      |IntEntry ->
         (if is_entering_base then
            (is_entering_base <- false;
            int_base_string <- "")
         else if String.length int_entry_buffer > 0 then
            (let len = String.length int_entry_buffer in
            int_entry_buffer <- String.sub int_entry_buffer 0 (len - 1))
         else
            (entry_type <- FloatEntry;
            has_entry <- false));
         self#draw_entry ()
      |FloatEntry ->
         if is_entering_exponent then
            if String.length buffer.re_exponent > 0 then
               (let len = String.length buffer.re_exponent in
               buffer.re_exponent <- String.sub buffer.re_exponent 0 (len - 1);
               self#draw_entry ())
            else
               (is_entering_exponent <- false;
               self#draw_entry ())
         else if String.length buffer.re_mantissa > 1 then
               (let len = String.length buffer.re_mantissa in
               buffer.re_mantissa <- String.sub buffer.re_mantissa 0 (len - 1);
               self#draw_entry ())
         else
            (has_entry <- false;
            buffer.re_mantissa <- "";
            self#draw_entry ())
      |ComplexEntry ->
         if is_entering_imag then
            if is_entering_exponent then
               if String.length buffer.im_exponent > 0 then
                  (let len = String.length buffer.im_exponent in
                  buffer.im_exponent <- String.sub buffer.im_exponent 0 (len - 1);
                  self#draw_entry ())
               else
                  (is_entering_exponent <- false;
                  self#draw_entry ())
            else if String.length buffer.im_mantissa > 0 then
                  (let len = String.length buffer.im_mantissa in
                  buffer.im_mantissa <- String.sub buffer.im_mantissa 0 (len - 1);
                  self#draw_entry ())
            else
               begin
                  is_entering_imag <- false;
                  (if String.length buffer.re_exponent > 0 then
                     is_entering_exponent <- true
                  else
                     ());
                  self#draw_entry ()
               end
         (* not entering imag *)
         else if is_entering_exponent then
            if String.length buffer.re_exponent > 0 then
               (let len = String.length buffer.re_exponent in
               buffer.re_exponent <- String.sub buffer.re_exponent 0 (len - 1);
               self#draw_entry ())
            else
               (is_entering_exponent <- false;
               self#draw_entry ())
         else if String.length buffer.re_mantissa > 0 then
               (let len = String.length buffer.re_mantissa in
               buffer.re_mantissa <- String.sub buffer.re_mantissa 0 (len - 1);
               self#draw_entry ())
         else
            (entry_type <- FloatEntry;
            has_entry <- false;
            self#draw_entry ())
      |FloatMatrixEntry ->
         if is_entering_exponent then
            if String.length buffer.re_exponent > 0 then
               (let len = String.length buffer.re_exponent in
               buffer.re_exponent <- String.sub buffer.re_exponent 0 (len - 1);
               self#draw_entry ())
            else
               (is_entering_exponent <- false;
               self#draw_entry ())
         else if String.length buffer.re_mantissa > 0 then
               (let len = String.length buffer.re_mantissa in
               buffer.re_mantissa <- String.sub buffer.re_mantissa 0 (len - 1);
               self#draw_entry ())
         else if current_buffer > 0 then
            begin
               current_buffer <- pred current_buffer;
               (if String.length gen_buffer.(current_buffer).re_exponent > 0 then
                  is_entering_exponent <- true
               else
                  ());
               (if succ current_buffer <= matrix_cols then
                  (matrix_cols <- succ current_buffer;
                  has_multiple_rows <- false)
               else
                  ());
               self#draw_entry ()
            end
         else
            (entry_type <- FloatEntry;
            has_entry <- false;
            self#draw_entry ())
      |ComplexMatrixEntry ->
         if is_entering_imag then
            if is_entering_exponent then
               if String.length buffer.im_exponent > 0 then
                  (let len = String.length buffer.im_exponent in
                  buffer.im_exponent <- String.sub buffer.im_exponent 0 (len - 1);
                  self#draw_entry ())
               else
                  (is_entering_exponent <- false;
                  self#draw_entry ())
            else if String.length buffer.im_mantissa > 0 then
                  (let len = String.length buffer.im_mantissa in
                  buffer.im_mantissa <- String.sub buffer.im_mantissa 0 (len - 1);
                  self#draw_entry ())
            else
               begin
                  is_entering_imag <- false;
                  (if String.length buffer.re_exponent > 0 then
                     is_entering_exponent <- true
                  else
                     () );
                  self#draw_entry ()
               end
         (* not entering imag *)
         else if is_entering_exponent then
            if String.length buffer.re_exponent > 0 then
               (let len = String.length buffer.re_exponent in
               buffer.re_exponent <- String.sub buffer.re_exponent 0 (len - 1);
               self#draw_entry ())
            else
               (is_entering_exponent <- false;
               self#draw_entry ())
         else if String.length buffer.re_mantissa > 0 then
               (let len = String.length buffer.re_mantissa in
               buffer.re_mantissa <- String.sub buffer.re_mantissa 0 (len - 1);
               self#draw_entry ())
         else if current_buffer > 0 then
            begin
               current_buffer <- pred current_buffer;
               is_entering_imag <- true;
               (if String.length gen_buffer.(current_buffer).im_exponent > 0 then
                  is_entering_exponent <- true
               else
                  ());
               (if succ current_buffer <= matrix_cols then
                  (matrix_cols <- succ current_buffer;
                  has_multiple_rows <- false)
               else
                  ());
               self#draw_entry ();
            end
         else
            entry_type <- FloatEntry;
            has_entry <- false;
            self#draw_entry ()


   (* handle a 'scientific_notation' (or base change) keypress *)
   method private handle_scientific_notation () =
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


   (* handle a 'minus' keypress *)
   method private handle_minus () =
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
         raise Not_handled


   (* begin stack browsing *)
   method private handle_begin_browse () =
      if calc#get_stack_size () > 0 then
         (fprintf stderr "beginning browse\n";
         flush stderr;
         interface_mode <- BrowsingMode;
         calc#backup ();
         self#draw_stack ())
      else
         ()


   (* handle exit of browsing mode *)
   method private handle_end_browse () =
      horiz_scroll <- 0;
      stack_selection <- 1;
      stack_bottom_row <- 1;
      interface_mode <- StandardEntryMode;
      self#draw_stack ()
      

   (* handle scrolling left in browsing mode *)
   method private handle_scroll_left () =
      (if horiz_scroll > 0 then
         horiz_scroll <- pred horiz_scroll
      else
         ());
      self#draw_stack ()


   (* handle scrolling right in browsing mode *)
   method private handle_scroll_right () =
      let s = calc#get_display_line stack_selection in
      let len = String.length s in
      (if horiz_scroll < len - scr.sw_cols + 7 then
         horiz_scroll <- succ horiz_scroll
      else
         ());
      self#draw_stack ()


   (* handle cyclic rolldown in browsing mode *)
   method private handle_rolldown () =
      calc#rolldown stack_selection;
      self#draw_stack ()


   (* handle cyclic rollup in browsing mode *)
   method private handle_rollup () =
      calc#rollup stack_selection;
      self#draw_stack ()


   (* handle moving up a line in browsing mode *)
   method private handle_prev_line () =
      if stack_selection < calc#get_stack_size () then
         (stack_selection <- succ stack_selection;
         horiz_scroll <- 0;
         (if stack_selection > pred (stack_bottom_row + scr.sw_lines) then
            stack_bottom_row <- stack_selection - scr.sw_lines + 1
         else
            ());
         self#draw_stack ())
      else
         ()


   (* handle moving down a line in browsing mode *)
   method private handle_next_line () =
      if stack_selection > 1 then
         (stack_selection <- pred stack_selection;
         horiz_scroll <- 0;
         (if stack_selection < stack_bottom_row then
            stack_bottom_row <- stack_selection
         else
            ());
         self#draw_stack ())
      else
         ()


   (* handle echoing stack selection (browsing mode) *)
   method private handle_browse_echo () =
      calc#echo stack_selection;
      self#handle_prev_line ();
      self#draw_stack ()
      

   (* quit the calculator *)
   method private handle_quit () =
      run_calc <- false


   (* begin extended entry *)
   method private handle_begin_extended () =
      if interface_mode != ExtendedEntryMode then
         (interface_mode <- ExtendedEntryMode;
         help_mode <- Extended;
         self#draw_entry ())
         (* do other cleanup stuff *)
      else
         ()


   (* exit extended entry *)
   method private handle_exit_extended () =
      if interface_mode = ExtendedEntryMode then
         (interface_mode <- StandardEntryMode;
         help_mode <- Standard;
         extended_entry_buffer <- "";
         matched_extended_entry <- "";
         self#draw_entry ())
      else
         ()


   (* enter an extended entry *)
   method private handle_enter_extended () =
      (* FIXME: obviously this needs stuff added *)
      if interface_mode = ExtendedEntryMode then
         (interface_mode <- StandardEntryMode;
         help_mode <- Standard;
         (try
            self#match_extended_buffer extended_entry_buffer;
            match (translate_extended_abbrev matched_extended_entry) with
            |Function ff -> self#process_function ff
            |Command cc  -> self#process_command cc
            |_ -> failwith 
               "found extended command that is neither Function nor Command"
         with
            Not_found -> ());
         extended_entry_buffer <- "";
         matched_extended_entry <- "";
         self#draw_entry ())
      else
         ()


   (* search through the list of commands for the first one that matches
    * extended_entry_buffer *)
   method private match_extended_buffer buf =
      if String.length buf > 0 then
         (let regex_str = "^" ^ buf ^ ".*$" in
         let regex = Str.regexp regex_str in
         let dummy = Str.search_forward regex extended_commands 0 in
         let matched_command = Str.matched_string extended_commands in
         matched_extended_entry <- matched_command)
      else
         (matched_extended_entry <- "";
         raise Not_found)


   (* backspace during extended entry *)
   method private handle_extended_backspace () =
      let len = String.length extended_entry_buffer in
      if len > 0 then
         (extended_entry_buffer <- Str.string_before extended_entry_buffer 
         (pred len);
         (try self#match_extended_buffer extended_entry_buffer
         with Not_found -> ());
         (if len = 1 then
            matched_extended_entry <- ""
         else
            ());
         self#draw_entry ())
      else
         ()


   (* handle entry of an arbitrary character in extended mode *)
   method private handle_extended_character key =
      let ch = char_of_int key in
      let test_buffer = extended_entry_buffer ^ (String.make 1 ch) in
      (* search through the list of commands for the first one that matches
       * extended_entry_buffer *)
      try
         self#match_extended_buffer test_buffer;
         extended_entry_buffer <- test_buffer;
         self#draw_entry ()
      with
         Not_found -> let err = beep () in ()
      

   (* handle a call to a function (which first pushes the item in the
    * entry buffer)  *)
   method private handle_function_call calc_function =
      try 
         (if has_entry then
            self#push_entry ()
         else
            ());
         calc_function ();
         self#draw_stack ()
      with 
         Invalid_argument error_msg ->
            self#draw_error error_msg


   (* handle a call to the simple commands that require no argument *)
   method private handle_command_call calc_command =
      try
         calc_command ();
         self#draw_stack ()
      with
         Invalid_argument error_msg ->
            self#draw_error error_msg



   (* handle entry of a digit *)
   method private handle_digit key =
      let process_float_digit buffer is_imag =
         let exponent_buffer =
            if is_imag then buffer.im_exponent
            else buffer.re_exponent
         and mantissa_buffer =
            if is_imag then buffer.im_mantissa
            else buffer.re_mantissa
         in 
         let has_decimal =
            let dot_regex = Str.regexp "^.*\..*$" in
            Str.string_match dot_regex mantissa_buffer 0
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
                  if has_decimal then "0123456789"
                  else "0123456789."
               in
               try
                  let c = char_of_int key in
                  if String.contains digits c then
                     begin
                        (if is_imag then
                           buffer.im_mantissa <- mantissa_buffer ^
                           (String.make 1 c)
                        else
                           buffer.re_mantissa <- mantissa_buffer ^
                           (String.make 1 c));
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
         end
      |FloatEntry | FloatMatrixEntry ->
         let buffer = gen_buffer.(current_buffer) in
         process_float_digit buffer false
      |ComplexEntry | ComplexMatrixEntry ->
         let buffer = gen_buffer.(current_buffer) in
         if is_entering_imag then
            process_float_digit buffer true
         else
            process_float_digit buffer false


end;;  (* class rpc_interface *)




(* arch-tag: DO_NOT_CHANGE_b4519dd2-7e94-4cbf-931a-bb5f97445cbf *)
