(*  Orpie -- a fullscreen RPN calculator for the console
 *  Copyright (C) 2003-2004  Paul Pelzl
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License, Version 2,
 *  as published by the Free Software Foundation.
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
open Interface;;
open Interface_draw;;


exception Interrupt_exception;;



(*******************************************************************)
(* RESIZE HANDLER                                                  *)
(*******************************************************************)


(* create the (new) windows corresponding to the different areas of the screen *)
let create_windows screen =
   let height, width = get_size () in
   if height >= 24 then 
      if width >= 80 && not !Rcfile.hide_help then
         (* full two-pane window provided *)
         let left_win   = Some (newwin (height - 2) 40 0 0) and
         right_win  = newwin (height - 2) 40 0 40 and
         bottom_win = newwin 2 80 (height - 2) 0 in
         {stdscr = screen; lines = height; cols = width; 
         help_win = left_win; hw_lines = (height - 2); hw_cols = 40;
         stack_win = right_win; sw_lines = (height - 2); sw_cols = 40;
         entry_win = bottom_win; ew_lines = 2; ew_cols = 80}
      else if width >= 40 then
         (* only the stack window is provided *)
         let right_win = newwin (height - 2) 40 0 0 and
         bottom_win = newwin 2 width (height - 2) 0 in
         {stdscr = screen; lines = height; cols = width; 
         help_win = None; hw_lines = 0; hw_cols = 0;
         stack_win = right_win; sw_lines = (height - 2); sw_cols = 40;
         entry_win = bottom_win; ew_lines = 2; ew_cols = 40}
      else
         (endwin ();
         failwith "Orpie requires at least a 40 column window.")
   else
      (endwin (); 
      failwith "Orpie requires at least a 24 line window.");;


(* resize the various windows to fit the new terminal size *)
let resize_subwins scr =
   let height, width = get_size () in
   if height >= 24 then 
      if width >= 80 && not !Rcfile.hide_help then
         (* full two-pane window provided *)
         begin
            scr.lines <- height;
            scr.cols <- width;
            begin match scr.help_win with
            |None ->
               scr.help_win <- Some (newwin (height - 2) 40 0 0)
            |Some win ->
               assert (wresize win (height - 2) 40);
            end;
            scr.hw_lines <- height - 2;
            scr.hw_cols <- 40;
            assert (wresize scr.stack_win (height - 2) 40);
            assert (mvwin scr.stack_win 0 40);
            scr.sw_lines <- height - 2;
            scr.sw_cols <- 40;
            assert (wresize scr.entry_win 2 80);
            assert (mvwin scr.entry_win (height - 2) 0);
            scr.ew_lines <- 2;
            scr.ew_cols <- 80
         end
      else if width >= 40 then
         (* only the stack window is provided *)
         begin
            scr.lines <- height;
            scr.cols <- width;
            begin match scr.help_win with
            |None ->
               ()
            |Some win ->
               assert (delwin win);
               scr.help_win <- None;
            end;
            scr.hw_lines <- 0;
            scr.hw_cols <- 0;
            assert (wresize scr.stack_win (height - 2) 40);
            assert (mvwin scr.stack_win 0 0);
            scr.sw_lines <- height - 2;
            scr.sw_cols <- 40;
            assert (wresize scr.entry_win 2 40);
            assert (mvwin scr.entry_win (height - 2) 0);
            scr.ew_lines <- 2;
            scr.ew_cols <- 40
         end
      else
         (endwin ();
         failwith "Orpie requires at least a 40 column window.")
   else
      (endwin (); 
      failwith "Orpie requires at least a 24 line window.");;



(* refresh the screen *)
let handle_refresh (iface : interface_state_t) =
   begin match iface.scr.help_win with
   |None     -> ()
   |Some win -> let _ = touchwin win in ()
   end;
   let _ = touchwin iface.scr.stack_win in
   let _ = touchwin iface.scr.entry_win in
   draw_help iface;
   draw_stack iface;
   draw_update_entry iface;
   draw_update_stack iface



(* handle a terminal resize *)
let handle_resize (iface : interface_state_t) =
   (* reset ncurses *)
   endwin ();
   assert (refresh ());
   let rows, cols = get_size () in
   resize_subwins iface.scr;
   handle_refresh iface;;





(*******************************************************************)
(* OBTAINING AN OBJECT FROM THE ENTRY BUFFER                       *)
(*******************************************************************)

(* parse the entry buffers to obtain a stack object *)
let get_entry_from_buffer (iface : interface_state_t) =
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
   match iface.entry_type with
   |IntEntry ->
      let base_mode = 
         if iface.is_entering_base then
            match iface.int_base_string with
            |"b" -> Bin
            |"o" -> Oct
            |"d" -> Dec
            |"h" -> Hex
            |_ -> (iface.calc#get_modes ()).base
         else
            (iface.calc#get_modes ()).base
      in
      let base = match base_mode with
      |Bin -> 2
      |Oct -> 8
      |Dec -> 10
      |Hex -> 16 in
      begin
         try
            let ival = Big_int_str.big_int_of_string_base 
            iface.int_entry_buffer base in
            RpcInt ival
         with Big_int_str.Big_int_string_failure error_msg ->
            raise (Invalid_argument error_msg)
      end
   |FloatEntry ->
      begin
         let buffer = iface.gen_buffer.(0) in
         try
            let ff = get_float_el buffer.re_mantissa buffer.re_exponent in
            let fu = Units.unit_of_float_string ff iface.units_entry_buffer in
            RpcFloatUnit fu
         with 
            |Failure "float_of_string" ->
               raise (Invalid_argument "improperly formatted floating-point data")
            |Units.Units_error ss ->
               raise (Invalid_argument ss)
      end
   |ComplexEntry ->
      begin
         let buffer = iface.gen_buffer.(0) in
         try
            begin match buffer.is_polar with
            |false ->
               let real_part = get_float_el buffer.re_mantissa 
               buffer.re_exponent
               and imag_part = get_float_el buffer.im_mantissa
               buffer.im_exponent in
               let cu = Units.unit_of_cpx_string 
               {re = real_part; im = imag_part} iface.units_entry_buffer in
               RpcComplexUnit cu
            (* if is_polar==true, then the data in the buffer represents
             * polar data... so convert it to rect notation before storing
             * it as a complex number. *)
            |true ->
               let r = get_float_el buffer.re_mantissa
               buffer.re_exponent
               and theta = 
                  match (iface.calc#get_modes ()).angle with
                  |Rad ->
                     get_float_el buffer.im_mantissa buffer.im_exponent
                  |Deg ->
                     (pi /. 180.0 *. (get_float_el buffer.im_mantissa
                     buffer.im_exponent))
               in
               let cu = Units.unit_of_cpx_string 
               {re = r *. (cos theta); im = r *. (sin theta)}
               iface.units_entry_buffer in
               RpcComplexUnit cu
            end
         with 
            |Failure "float_of_string" ->
               raise (Invalid_argument "improperly formatted complex floating-point data")
            |Units.Units_error ss ->
               raise (Invalid_argument ss)
      end
   |FloatMatrixEntry ->
      begin
         let matrix_rows = 
            if iface.has_multiple_rows then
               succ (iface.curr_buf / iface.matrix_cols)
            else
               1
         in
         let temp_arr = Array.make (matrix_rows * iface.matrix_cols) 0.0 in
         try
            for i = 0 to iface.curr_buf do
               temp_arr.(i) <- (get_float_el iface.gen_buffer.(i).re_mantissa
                  iface.gen_buffer.(i).re_exponent)
            done;
            let uu = Units.unit_of_string iface.units_entry_buffer in
            RpcFloatMatrixUnit (Gsl_matrix.of_array temp_arr matrix_rows
            iface.matrix_cols, uu)
         with Failure "float_of_string" ->
            raise (Invalid_argument "improperly formatted floating-point matrix data")
      end
   |ComplexMatrixEntry ->
      begin
         let matrix_rows = 
            if iface.has_multiple_rows then
               succ (iface.curr_buf / iface.matrix_cols)
            else
               1
         in
         let temp_arr = Array.make (matrix_rows * iface.matrix_cols) 
         {re = 0.0; im = 0.0} in
         try
            for i = 0 to iface.curr_buf do
               begin match iface.gen_buffer.(i).is_polar with
               |false ->
                  let re_part = get_float_el iface.gen_buffer.(i).re_mantissa
                  iface.gen_buffer.(i).re_exponent 
                  and im_part = get_float_el iface.gen_buffer.(i).im_mantissa
                  iface.gen_buffer.(i).im_exponent in
                  temp_arr.(i) <- {re = re_part; im = im_part}
               (* if is_polar==true, then the data in the buffer represents
                * polar data... so convert it to rect notation before storing
                * it as a complex number. *)
               |true ->
                  let r = get_float_el iface.gen_buffer.(i).re_mantissa
                  iface.gen_buffer.(i).re_exponent
                  and theta =
                     match (iface.calc#get_modes ()).angle with
                     |Rad ->
                        get_float_el iface.gen_buffer.(i).im_mantissa
                        iface.gen_buffer.(i).im_exponent
                     |Deg ->
                        (pi /. 180.0 *. (get_float_el
                        iface.gen_buffer.(i).im_mantissa
                        iface.gen_buffer.(i).im_exponent))
                  in
                  temp_arr.(i) <- {re = r *. (cos theta); 
                  im = r *. (sin theta)}
               end
            done;
            let uu = Units.unit_of_string iface.units_entry_buffer in
            RpcComplexMatrixUnit (Gsl_matrix_complex.of_array temp_arr
            matrix_rows iface.matrix_cols, uu)
         with Failure "float_of_string" ->
            raise (Invalid_argument "improperly formatted complex
            floating-point matrix data")
      end
   |VarEntry ->
      RpcVariable iface.variable_entry_buffer




(* parse the entry buffers to obtain a stack object, then stack#push it *)
let push_entry (iface : interface_state_t) =
   (* perform this cleanup routine after every entry, so we are prepared
      to receive a new object. *)
   let post_entry_cleanup () =
      iface.has_entry <- false;
      iface.entry_type <- FloatEntry;
      iface.int_entry_buffer <- "";
      iface.int_base_string <- "";
      iface.is_entering_base <- false;
      iface.is_entering_exponent <- false;
      for i = 0 to pred max_matrix_size do
         iface.gen_buffer.(i) <- 
            {re_mantissa = ""; re_exponent = "";
            im_mantissa = ""; im_exponent = ""; is_polar = false}
      done;
      iface.curr_buf <- 0;
      iface.is_entering_imag <- false;
      iface.matrix_cols <- 1;
      iface.has_multiple_rows <- false;
      iface.variable_entry_buffer <- "";
      iface.units_entry_buffer <- "";
      iface.is_entering_units <- false;
      draw_update_entry iface
   in
   begin
      try
         iface.calc#push (get_entry_from_buffer iface)
      with
         Invalid_argument error_msg ->
            (post_entry_cleanup ();
            (* raise the EXN again, so it can be caught within do_main_loop *)
            raise (Invalid_argument error_msg))
   end;
   post_entry_cleanup ()


(***********************************************************************)
(* HANDLERS FOR EDITING KEYSTROKES                                     *)
(***********************************************************************)

(* handle an 'enter' keypress *)
let handle_enter (iface : interface_state_t) =
   iface.interface_mode <- StandardEditMode;
   draw_help iface;
   try
      if iface.has_entry then
         push_entry iface
      else
         raise Not_handled;
      draw_update_stack iface
   with Invalid_argument error_msg ->
      draw_error iface error_msg;
      assert (doupdate ())


(* handle a 'begin_int' keypress *)
let handle_begin_int (iface : interface_state_t) =
   if iface.entry_type = FloatEntry then
      (iface.entry_type <- IntEntry;
      iface.interface_mode <- IntEditMode;
      iface.int_entry_buffer <- "";
      draw_help iface;
      draw_update_entry iface)
   else
      () 


(* handle a 'begin_complex' keypress *)
let handle_begin_complex (iface : interface_state_t) =
   iface.has_entry <- true;
   match iface.entry_type with
   |FloatEntry ->
      (iface.entry_type <- ComplexEntry;
      draw_update_entry iface)
   |FloatMatrixEntry ->
      (iface.entry_type <- ComplexMatrixEntry;
      draw_update_entry iface)
   |_ ->
      ()


(* handle a 'begin_matrix' keypress *)
let handle_begin_matrix (iface : interface_state_t) =
   iface.has_entry <- true;
   match iface.entry_type with
   |FloatEntry ->
      (iface.entry_type <- FloatMatrixEntry;
      draw_update_entry iface)
   |ComplexEntry ->
      (iface.entry_type <- ComplexMatrixEntry;
      draw_update_entry iface)
   |FloatMatrixEntry ->
      if not iface.has_multiple_rows then
         (iface.has_multiple_rows <- true;
         iface.curr_buf <- succ iface.curr_buf;
         iface.matrix_cols <- iface.curr_buf;
         iface.is_entering_exponent <- false;
         draw_update_entry iface)
      else if (succ iface.curr_buf) mod iface.matrix_cols = 0 then
         (iface.curr_buf <- succ iface.curr_buf;
         iface.is_entering_exponent <- false;
         (*FIXME: any other items to reset here?*)
         draw_update_entry iface)
      else
         ()
   |ComplexMatrixEntry ->
      if not iface.has_multiple_rows then
         (iface.has_multiple_rows <- true;
         iface.curr_buf <- succ iface.curr_buf;
         iface.matrix_cols <- iface.curr_buf;
         iface.is_entering_exponent <- false;
         iface.is_entering_imag <- false;
         draw_update_entry iface)
      else if (succ iface.curr_buf) mod iface.matrix_cols = 0 then
         (iface.curr_buf <- succ iface.curr_buf;
         iface.is_entering_exponent <- false;
         iface.is_entering_imag <- false;
         (*FIXME: any other items to reset here?*)
         draw_update_entry iface)
      else
         ()
   |_ ->
      ()
   

(* handle a 'separator' keypress *)
let handle_separator (iface : interface_state_t) =
   match iface.entry_type with
   |ComplexEntry ->
      if not iface.is_entering_imag then
         (iface.is_entering_imag <- true;
         iface.is_entering_exponent <- false;
         draw_update_entry iface)
      else
         ()
   |FloatMatrixEntry ->
      if iface.curr_buf < pred max_matrix_size then
         (iface.curr_buf <- succ iface.curr_buf;
         iface.is_entering_exponent <- false;
         (if not iface.has_multiple_rows then
            iface.matrix_cols <- succ iface.curr_buf
         else
            ());
         (*FIXME: any other items to reset here?*)
         draw_update_entry iface)
      else
         ()
   |ComplexMatrixEntry ->
      if iface.is_entering_imag then
         if iface.curr_buf < pred max_matrix_size then
            (iface.curr_buf <- succ iface.curr_buf;
            iface.is_entering_exponent <- false;
            iface.is_entering_imag <- false;
            (if not iface.has_multiple_rows then
               iface.matrix_cols <- succ iface.curr_buf
            else
               ());
            (*FIXME: any other items to reset here?*)
            draw_update_entry iface)
         else
            ()
      else
         (iface.is_entering_imag <- true;
         iface.is_entering_exponent <- false;
         draw_update_entry iface)
   |_ ->
      ()



(* handle an 'angle' keypress *)
let handle_angle (iface : interface_state_t) =
   match iface.entry_type with
   |ComplexEntry ->
      if not iface.is_entering_imag then
         (iface.is_entering_imag <- true;
         iface.gen_buffer.(iface.curr_buf).is_polar <- true;
         iface.is_entering_exponent <- false;
         draw_update_entry iface)
      else
         ()
   |ComplexMatrixEntry ->
      if iface.is_entering_imag then
         ()
      else
         (iface.is_entering_imag <- true;
         iface.gen_buffer.(iface.curr_buf).is_polar <- true;
         iface.is_entering_exponent <- false;
         draw_update_entry iface)
   |_ ->
      ()


(* cancel integer entry mode *)
let handle_exit_int (iface : interface_state_t) =
   iface.interface_mode <- StandardEditMode;
   iface.entry_type <- FloatEntry;
   iface.is_entering_base <- false;
   iface.int_base_string <- "";
   if (String.length iface.gen_buffer.(0).re_mantissa > 0) or
      (String.length iface.gen_buffer.(0).re_exponent > 0) or
      (String.length iface.gen_buffer.(0).im_mantissa > 0) or
      (String.length iface.gen_buffer.(0).im_exponent > 0) then
      iface.has_entry <- true
   else
      iface.has_entry <- false;
   draw_help iface;
   draw_update_entry iface


(* handle a 'backspace' keypress *)
let handle_backspace (iface : interface_state_t) =
   let buffer = iface.gen_buffer.(iface.curr_buf) in
   begin match iface.entry_type with
   |IntEntry ->
      if iface.is_entering_base then begin
         iface.is_entering_base <- false;
         iface.int_base_string <- ""
      end else if String.length iface.int_entry_buffer > 0 then begin
         let len = String.length iface.int_entry_buffer in
         iface.int_entry_buffer <- String.sub iface.int_entry_buffer 0 (len - 1)
      end else
         handle_exit_int iface
   |FloatEntry ->
      if iface.is_entering_exponent then
         if String.length buffer.re_exponent > 0 then
            (let len = String.length buffer.re_exponent in
            buffer.re_exponent <- String.sub buffer.re_exponent 0 (len - 1))
         else
            (iface.is_entering_exponent <- false;
            draw_update_entry iface)
      else if String.length buffer.re_mantissa > 1 then
            (let len = String.length buffer.re_mantissa in
            buffer.re_mantissa <- String.sub buffer.re_mantissa 0 (len - 1))
      else
         (iface.has_entry <- false;
         buffer.re_mantissa <- "")
   |ComplexEntry ->
      if iface.is_entering_imag then
         if iface.is_entering_exponent then
            if String.length buffer.im_exponent > 0 then
               (let len = String.length buffer.im_exponent in
               buffer.im_exponent <- String.sub buffer.im_exponent 0 (len - 1))
            else
               iface.is_entering_exponent <- false
         else if String.length buffer.im_mantissa > 0 then
               (let len = String.length buffer.im_mantissa in
               buffer.im_mantissa <- String.sub buffer.im_mantissa 0 (len - 1))
         else
            begin
               iface.is_entering_imag <- false;
               buffer.is_polar <- false;
               (if String.length buffer.re_exponent > 0 then
                  iface.is_entering_exponent <- true
               else
                  ())
            end
      (* not entering imag/angle *)
      else if iface.is_entering_exponent then
         if String.length buffer.re_exponent > 0 then
            (let len = String.length buffer.re_exponent in
            buffer.re_exponent <- String.sub buffer.re_exponent 0 (len - 1))
         else
            iface.is_entering_exponent <- false
      else if String.length buffer.re_mantissa > 0 then
            (let len = String.length buffer.re_mantissa in
            buffer.re_mantissa <- String.sub buffer.re_mantissa 0 (len - 1))
      else
         (iface.entry_type <- FloatEntry;
         iface.has_entry <- false)
   |FloatMatrixEntry ->
      if iface.is_entering_exponent then
         if String.length buffer.re_exponent > 0 then
            (let len = String.length buffer.re_exponent in
            buffer.re_exponent <- String.sub buffer.re_exponent 0 (len - 1))
         else
            iface.is_entering_exponent <- false
      else if String.length buffer.re_mantissa > 0 then
            (let len = String.length buffer.re_mantissa in
            buffer.re_mantissa <- String.sub buffer.re_mantissa 0 (len - 1))
      else if iface.curr_buf > 0 then
         begin
            iface.curr_buf <- pred iface.curr_buf;
            (if String.length iface.gen_buffer.(iface.curr_buf).re_exponent > 0 then
               iface.is_entering_exponent <- true
            else
               ());
            (if succ iface.curr_buf <= iface.matrix_cols then
               (iface.matrix_cols <- succ iface.curr_buf;
               iface.has_multiple_rows <- false)
            else
               ())
         end
      else begin
         iface.entry_type <- FloatEntry;
         iface.has_entry <- false
      end
   |ComplexMatrixEntry ->
      if iface.is_entering_imag then
         if iface.is_entering_exponent then
            if String.length buffer.im_exponent > 0 then
               (let len = String.length buffer.im_exponent in
               buffer.im_exponent <- String.sub buffer.im_exponent 0 (len - 1))
            else
               iface.is_entering_exponent <- false
         else if String.length buffer.im_mantissa > 0 then
               (let len = String.length buffer.im_mantissa in
               buffer.im_mantissa <- String.sub buffer.im_mantissa 0 (len - 1))
         else
            begin
               iface.is_entering_imag <- false;
               buffer.is_polar <- false;
               (if String.length buffer.re_exponent > 0 then
                  iface.is_entering_exponent <- true
               else
                  ())
            end
      (* not entering imag/angle *)
      else if iface.is_entering_exponent then
         if String.length buffer.re_exponent > 0 then
            (let len = String.length buffer.re_exponent in
            buffer.re_exponent <- String.sub buffer.re_exponent 0 (len - 1))
         else
            iface.is_entering_exponent <- false
      else if String.length buffer.re_mantissa > 0 then
            (let len = String.length buffer.re_mantissa in
            buffer.re_mantissa <- String.sub buffer.re_mantissa 0 (len - 1))
      else if iface.curr_buf > 0 then
         begin
            iface.curr_buf <- pred iface.curr_buf;
            iface.is_entering_imag <- true;
            (if String.length iface.gen_buffer.(iface.curr_buf).im_exponent > 0 then
               iface.is_entering_exponent <- true
            else
               ());
            (if succ iface.curr_buf <= iface.matrix_cols then
               (iface.matrix_cols <- succ iface.curr_buf;
               iface.has_multiple_rows <- false)
            else
               ())
         end
      else begin
         iface.entry_type <- FloatEntry;
         iface.has_entry <- false
      end
   |_ ->
      failwith "caught unhandled backspace"
   end;
   draw_update_entry iface


(* handle a 'scientific_notation' (or base change) keypress *)
let handle_scientific_notation (iface : interface_state_t) =
   begin match iface.entry_type with 
   |IntEntry ->
      if String.length iface.int_entry_buffer > 0 then
         (iface.is_entering_base <- true;
         draw_update_entry iface)
      else
         ()
   |FloatEntry | FloatMatrixEntry ->
      if String.length iface.gen_buffer.(iface.curr_buf).re_mantissa > 0 then
         (iface.is_entering_exponent <- true;
         draw_update_entry iface)
      else
         ()
   |ComplexEntry | ComplexMatrixEntry ->
      if iface.is_entering_imag then
         if String.length iface.gen_buffer.(iface.curr_buf).im_mantissa > 0 then
            (iface.is_entering_exponent <- true;
            draw_update_entry iface)
         else
            ()
      else
         if String.length iface.gen_buffer.(0).re_mantissa > 0 then
            (iface.is_entering_exponent <- true;
            draw_update_entry iface)
         else
            ()
   |_ ->
      failwith "caught unhandled scientific notation"
   end;
   draw_update_entry iface


(* handle a 'minus' keypress *)
let handle_minus (iface : interface_state_t) =
   if iface.has_entry then
      match iface.entry_type with
      |IntEntry ->
         (begin
            match iface.int_entry_buffer.[0] with
            |'-' -> iface.int_entry_buffer.[0] <- '+'
            |'+' -> iface.int_entry_buffer.[0] <- '-'
            |_ -> iface.int_entry_buffer <- "-" ^ iface.int_entry_buffer
         end;
         draw_update_entry iface)
      |FloatEntry | FloatMatrixEntry ->
         begin
            let buffer = iface.gen_buffer.(iface.curr_buf) in
            if iface.is_entering_exponent then
               if String.length buffer.re_exponent > 0 then
                  match buffer.re_exponent.[0] with
                  |'-' -> buffer.re_exponent.[0] <- '+'
                  |'+' -> buffer.re_exponent.[0] <- '-'
                  |_ -> buffer.re_exponent <- "-" ^ buffer.re_exponent
               else
                  ()
            else
               if String.length buffer.re_mantissa > 0 then
                  match buffer.re_mantissa.[0] with
                  |'-' -> buffer.re_mantissa.[0] <- '+'
                  |'+' -> buffer.re_mantissa.[0] <- '-'
                  |_ -> buffer.re_mantissa <- "-" ^ buffer.re_mantissa
               else
                  ()
         end;
         draw_update_entry iface
      |ComplexEntry | ComplexMatrixEntry ->
         begin
            let buffer = iface.gen_buffer.(iface.curr_buf) in
            let mantissa = 
               if iface.is_entering_imag then
                  buffer.im_mantissa
               else
                  buffer.re_mantissa
            and exponent = 
               if iface.is_entering_imag then
                  buffer.im_exponent
               else
                  buffer.re_exponent
            in
            if iface.is_entering_exponent then
               if String.length exponent > 0 then
                  match exponent.[0] with
                  |'-' -> exponent.[0] <- '+'
                  |'+' -> exponent.[0] <- '-'
                  |_ -> 
                     if iface.is_entering_imag then
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
                     if iface.is_entering_imag then
                        buffer.im_mantissa <- "-" ^ buffer.im_mantissa
                     else
                        buffer.re_mantissa <- "-" ^ buffer.re_mantissa
               else
                  ()
         end;
         draw_update_entry iface
      |_ ->
         failwith "caught unhandled minus key"
   else
      raise Not_handled


(* handle entry of a digit *)
let handle_digit (iface : interface_state_t) key =
   let process_float_digit buffer is_imag =
      let exponent_buffer =
         if is_imag then buffer.im_exponent
         else buffer.re_exponent
      and mantissa_buffer =
         if is_imag then buffer.im_mantissa
         else buffer.re_mantissa
      in 
      let has_decimal =
         let dot_regex = Str.regexp "^.*\\..*$" in
         Str.string_match dot_regex mantissa_buffer 0
      in
      begin
         if iface.is_entering_exponent then
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
                     draw_update_entry iface)
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
                     iface.has_entry <- true;
                     draw_update_entry iface
                  end
               else
                  ()
            with Invalid_argument "char_of_int" ->
               ()
         else
            ()
      end
   in
   match iface.entry_type with
   |IntEntry ->
      begin
         if iface.is_entering_base then
            let base_chars = "bodh" in
            try
               let c = char_of_int key in
               if String.contains base_chars c then
                  (iface.int_base_string <- String.make 1 c;
                  draw_update_entry iface)
               else
                  ()
            with Invalid_argument "char_of_int" ->
               ()
         else
            let digits = "0123456789abcdefABCDEF" in
            try
               let c = char_of_int key in
               if String.contains digits c then
                  (iface.int_entry_buffer <- iface.int_entry_buffer ^ (String.make 1 c);
                  iface.has_entry <- true;
                  draw_update_entry iface)
               else
                  ()
            with Invalid_argument "char_of_int" ->
               ()
      end
   |FloatEntry | FloatMatrixEntry ->
      let buffer = iface.gen_buffer.(iface.curr_buf) in
      process_float_digit buffer false
   |ComplexEntry | ComplexMatrixEntry ->
      let buffer = iface.gen_buffer.(iface.curr_buf) in
      if iface.is_entering_imag then
         process_float_digit buffer true
      else
         process_float_digit buffer false
   |_ ->
      failwith "caught unhandled digit"



(* begin abbrev entry *)
let handle_begin_abbrev (iface : interface_state_t) =
   if iface.interface_mode <> AbbrevEditMode then begin
      iface.interface_mode <- AbbrevEditMode;
      iface.abbrev_or_const <- IsAbbrev;
      draw_help iface;
      draw_update_entry iface
      (* do other cleanup stuff *)
   end else
      ()

(* begin constant entry *)
let handle_begin_const (iface : interface_state_t) =
   if iface.interface_mode <> AbbrevEditMode then begin
      iface.interface_mode <- AbbrevEditMode;
      iface.abbrev_or_const <- IsConst;
      draw_help iface;
      draw_update_entry iface
      (* do other cleanup stuff *)
   end else
      ()


(* begin entry of a variable name *)
let handle_begin_variable (iface : interface_state_t) =
   if iface.interface_mode <> VarEditMode then begin
      iface.interface_mode <- VarEditMode;
      iface.entry_type <- VarEntry;
      (* FIXME: generation of the sorted variable list can be done far
       * more efficiently *)
      let add_variable var_str var_binding var_list =
         var_str :: var_list
      in
      let all_variables = 
         Hashtbl.fold add_variable (iface.calc#get_variables ()) []
      in
      iface.sorted_variables <- List.stable_sort String.compare all_variables;
      iface.matched_variables <- iface.sorted_variables;
      draw_help iface;
      draw_update_entry iface
   end else
      ()


(* begin entry of units *)
let handle_begin_units (iface : interface_state_t) =
   match iface.entry_type with
   |FloatEntry ->
      if iface.gen_buffer.(0).re_mantissa = "" then
         iface.gen_buffer.(0).re_mantissa <- "1"
      else
         ();
      iface.has_entry         <- true;
      iface.interface_mode    <- UnitEditMode;
      iface.is_entering_units <- true;
      draw_update_entry iface
   |ComplexEntry ->
      if iface.gen_buffer.(0).re_mantissa = "" then
         iface.gen_buffer.(0).re_mantissa <- "0"
      else
         ();
      if iface.gen_buffer.(0).im_mantissa = "" && iface.is_entering_imag then
         iface.gen_buffer.(0).im_mantissa <- "0"
      else
         ();
      iface.has_entry         <- true;
      iface.interface_mode    <- UnitEditMode;
      iface.is_entering_units <- true;
      draw_update_entry iface
   |FloatMatrixEntry | ComplexMatrixEntry ->
      iface.has_entry         <- true;
      iface.interface_mode    <- UnitEditMode;
      iface.is_entering_units <- true;
      draw_update_entry iface
   (* FIXME: add other matches as units code gets filled out *)
   |_ -> ()


(***********************************************************************)
(* HANDLERS FOR BROWSING-RELATED KEYSTROKES                            *)
(***********************************************************************)

(* begin stack browsing *)
let handle_begin_browse (iface : interface_state_t) =
   if iface.calc#get_stack_size () > 0 then
      ( (* fprintf stderr "beginning browse\n";
      flush stderr; *)
      iface.interface_mode <- BrowsingMode;
      iface.calc#backup ();
      draw_help iface;
      draw_update_stack iface)
   else
      ()


(* handle exit of browsing mode *)
let handle_end_browse (iface : interface_state_t) =
   iface.horiz_scroll <- 0;
   iface.stack_selection <- 1;
   iface.stack_bottom_row <- 1;
   iface.interface_mode <- StandardEditMode;
   draw_help iface;
   draw_update_stack iface
   

(* handle scrolling left in browsing mode *)
let handle_scroll_left (iface : interface_state_t) =
   (if iface.horiz_scroll > 0 then
      iface.horiz_scroll <- pred iface.horiz_scroll
   else
      ());
   draw_update_stack iface


(* handle scrolling right in browsing mode *)
let handle_scroll_right (iface : interface_state_t) =
   let s = iface.calc#get_display_line iface.stack_selection in
   let len = String.length s in
   (if iface.horiz_scroll < len - iface.scr.sw_cols + 7 then
      iface.horiz_scroll <- succ iface.horiz_scroll
   else
      ());
   draw_update_stack iface


(* handle cyclic rolldown in browsing mode *)
let handle_rolldown (iface : interface_state_t) =
   iface.calc#rolldown iface.stack_selection;
   draw_update_stack iface


(* handle cyclic rollup in browsing mode *)
let handle_rollup (iface : interface_state_t) =
   iface.calc#rollup iface.stack_selection;
   draw_update_stack iface


(* handle moving up a line in browsing mode *)
let handle_prev_line (iface : interface_state_t) =
   if iface.stack_selection < iface.calc#get_stack_size () then
      (iface.stack_selection <- succ iface.stack_selection;
      iface.horiz_scroll <- 0;
      let num_stack_lines =
         begin match iface.scr.help_win with
         |Some win -> iface.scr.sw_lines
         |None     -> iface.scr.sw_lines - 2
         end
      in
      (if iface.stack_selection > pred (iface.stack_bottom_row + num_stack_lines) then
         iface.stack_bottom_row <- iface.stack_selection - num_stack_lines + 1
      else
         ());
      draw_update_stack iface)
   else
      ()


(* handle moving down a line in browsing mode *)
let handle_next_line (iface : interface_state_t) =
   if iface.stack_selection > 1 then
      (iface.stack_selection <- pred iface.stack_selection;
      iface.horiz_scroll <- 0;
      (if iface.stack_selection < iface.stack_bottom_row then
         iface.stack_bottom_row <- iface.stack_selection
      else
         ());
      draw_update_stack iface)
   else
      ()


(* handle echoing stack selection (browsing mode) *)
let handle_browse_echo (iface : interface_state_t) =
   iface.calc#echo iface.stack_selection;
   handle_prev_line iface
   

(* handle fullscreen viewing of a selected stack element *)
let handle_browse_view (iface : interface_state_t) =
   try
      let fs_string = iface.calc#get_fullscreen_display iface.stack_selection in
      let fs_file = Utility.join_path !(Rcfile.datadir) "fullscreen" in
      let buf = Utility.open_or_create_out_ascii fs_file in
      output_string buf fs_string;
      close_out buf;
      let _ = 
         Sys.command (!(Rcfile.editor) ^ " " ^ fs_file)
      in ();
      draw_help iface;
      draw_stack iface;
      draw_update_entry iface
   with
      Sys_error ss -> 
         draw_error iface ss;
         assert (doupdate ())
            

(* drop a particular stack element *)
let handle_browse_drop1 (iface : interface_state_t) =
   iface.calc#delete iface.stack_selection;
   (if iface.stack_selection > iface.calc#get_stack_size () then
      iface.stack_selection <- iface.calc#get_stack_size ()
   else
      ());
   (if iface.stack_selection < iface.stack_bottom_row then
      iface.stack_bottom_row <- iface.stack_selection
   else
      ());
   if iface.calc#get_stack_size () < 1 then
      handle_end_browse iface
   else
      draw_update_stack iface


(* drop all elements below selected element (inclusive) *)
let handle_browse_dropn (iface : interface_state_t) =
   iface.calc#deleteN iface.stack_selection;
   iface.stack_selection <- 1;
   iface.stack_bottom_row <- 1;
   if iface.calc#get_stack_size () < 1 then
      handle_end_browse iface
   else
      draw_update_stack iface
   

(* keep only the selected stack element *)
let handle_browse_keep (iface : interface_state_t) =
   iface.calc#keep iface.stack_selection;
   iface.stack_selection <- 1;
   iface.stack_bottom_row <- 1;
   draw_update_stack iface


(* keep only stack elements below the current selection (inclusive) *)
let handle_browse_keepn (iface : interface_state_t) =
   iface.calc#keepN iface.stack_selection;
   draw_update_stack iface



(* call !Rcfile.editor to edit the input textfile buffer, then
 * parse it to generate one or more stack elements.
 * If is_browsing = true, then the stack element will be
 * bumped up to the selected stack level. *)
let edit_parse_input_textfile (iface : interface_state_t) (is_browsing) =
   try
      let input_file = Utility.join_path !(Rcfile.datadir) "input" in
      let _ = 
         Sys.command (!(Rcfile.editor) ^ " " ^ input_file)
      in ();
      (* wake up curses again, and check for resize *)
      assert (refresh ());
      let rows, cols = get_size () in
      resize_subwins iface.scr;
      handle_refresh iface;
      let edited_buf = Utility.expand_open_in_ascii input_file in
      let lexbuf = Lexing.from_channel edited_buf in
      let data = 
         (* need to call completely different parsers when using degrees
          * or when using radians *)
         begin match (iface.calc#get_modes ()).angle with
         |Rad ->
            Txtin_parser.decode_data_rad Txtin_lexer.token lexbuf
         |Deg ->
            Txtin_parser.decode_data_deg Txtin_lexer.token lexbuf
         end
      in
      let push_data el =
         if is_browsing then begin
            iface.calc#delete iface.stack_selection;
            iface.calc#push el;
            iface.calc#rolldown iface.stack_selection
         end else
            iface.calc#push el;
      in
      List.iter push_data data;
      close_in edited_buf;
      handle_refresh iface
   with
      |Parsing.Parse_error ->
         draw_help iface;
         draw_stack iface;
         draw_error iface "syntax error in input";
         draw_update_entry iface
      |Utility.Txtin_error ss ->
         draw_help iface;
         draw_stack iface;
         draw_error iface ss;
         draw_update_entry iface
      |Big_int_str.Big_int_string_failure ss ->
         draw_help iface;
         draw_stack iface;
         draw_error iface ss;
         draw_update_entry iface
      |Units.Units_error ss ->
         draw_help iface;
         draw_stack iface;
         draw_error iface ss;
         draw_update_entry iface
      |Failure ss ->
         draw_help iface;
         draw_stack iface;
         draw_error iface "syntax error in input";
         draw_update_entry iface


(* edit the selected stack element with an external editor *)
let handle_browse_edit (iface : interface_state_t) =
   try
      let fs_string = iface.calc#get_fullscreen_display iface.stack_selection in
      let input_file = Utility.join_path !(Rcfile.datadir) "input" in
      let buf = Utility.open_or_create_out_ascii input_file in
      output_string buf fs_string;
      close_out buf;
      edit_parse_input_textfile iface true
   with 
      Sys_error ss -> 
         (draw_help iface;
         draw_stack iface;
         draw_error iface ss;
         draw_update_entry iface)



(* view the last stack element in fullscreen *)
let handle_view (iface : interface_state_t) =
   try
      let fs_string = iface.calc#get_fullscreen_display 1 in
      let fs_file = Utility.join_path !(Rcfile.datadir) "fullscreen" in
      let buf = Utility.open_or_create_out_ascii fs_file in
      output_string buf fs_string;
      close_out buf;
      let _ = 
         Sys.command (!(Rcfile.editor) ^ " " ^ fs_file)
      in ();
      draw_help iface;
      draw_stack iface;
      draw_update_entry iface
   with
      Sys_error ss -> 
         draw_error iface ss;
         assert (doupdate ())

         
(* handle a call to edit a new input buffer *)
let handle_edit_input (iface : interface_state_t) =
   try
      edit_parse_input_textfile iface false
   with
      Sys_error ss -> 
         (draw_help iface;
         draw_stack iface;
         draw_error iface ss;
         draw_update_entry iface)


(* display an "about" screen *)
let handle_about (iface : interface_state_t) =
   draw_about iface;
   let a = getch () in ();
   erase ();
   assert (refresh ());
   handle_refresh iface


(* cycle the help screen *)
let handle_cycle_help (iface : interface_state_t) =
   if iface.help_page < 1 then
      iface.help_page <- succ iface.help_page
   else
      iface.help_page <- 0;
   draw_help iface;
   assert (doupdate ())


(* quit the calculator *)
let handle_quit (iface : interface_state_t) =
   iface.calc#save_state ();
   iface.run_calc <- false




(***********************************************************************)
(* HANDLERS FOR STANDARD CALCULATOR FUNCTIONS AND COMMANDS             *)
(***********************************************************************)


(* register an autobinding.
 * The autobindings are stored both in an array for quick ordered
 * access, and in the standard keybinding hashtables. *)
let register_autobinding (op : operation_t) =
   (* find oldest autobinding *)
   let oldest     = ref 0 in
   let oldest_age = ref (-1) in
   for i = 0 to pred (Array.length !Rcfile.autobind_keys) do
      let (key, key_string, bound_f, age) = !Rcfile.autobind_keys.(i) in
      if age > !oldest_age then begin
         oldest     := i;
         oldest_age := age
      end else
         ();
      (* make all autobindings "older" *)
      !Rcfile.autobind_keys.(i) <- (key, key_string, bound_f, (succ age))
   done;
   if Array.length !Rcfile.autobind_keys > 0 then begin
      let (key, key_string, bound_f, age) = !Rcfile.autobind_keys.(!oldest) in
      begin match bound_f with
      |None         -> ()
      |Some op -> Rcfile.remove_binding key op
      end;
      Rcfile.register_binding_internal key key_string op;
      !Rcfile.autobind_keys.(!oldest) <- (key, key_string, Some op, 0) 
   end else
      ()



(* handle a call to a function (which first pushes the item in the
 * entry buffer)  *)
let handle_function_call (iface : interface_state_t) calc_function =
   try 
      if iface.has_entry then
         push_entry iface
      else
         ();
      calc_function ();
      draw_update_stack iface
   with 
      Invalid_argument error_msg ->
         draw_error iface error_msg;
         assert (doupdate ())



(* handle a call to the simple commands that require no argument *)
let handle_command_call (iface : interface_state_t) calc_command =
   try
      calc_command ();
      draw_update_stack iface
   with
      Invalid_argument error_msg ->
         draw_error iface error_msg;
         assert (doupdate ())


(* handle a call to an interruptible function *)
let handle_interr_function_call (iface : interface_state_t) calc_function =
   try
      if iface.has_entry then
         push_entry iface
      else
         ();
      draw_message iface "Working... (press a key to abort)";
      assert (doupdate ());
      assert (nodelay iface.scr.entry_win true);
      while not (calc_function ()) do
         let key = wgetch iface.scr.entry_win in
         if key <> ~-1 then
            raise Interrupt_exception
         else
            ()
      done;
      assert (nodelay iface.scr.entry_win false);
      draw_update_stack iface
   with
      |Invalid_argument error_msg ->
         assert (nodelay iface.scr.entry_win false);
         draw_error iface error_msg;
         assert (doupdate ())
      |Interrupt_exception ->
         assert (nodelay iface.scr.entry_win false);
         iface.calc#abort_computation ();
         draw_message iface "Computation aborted.";
         assert (doupdate ())



let process_function (iface : interface_state_t) ff =
   begin match ff with
   |Add ->
      handle_function_call iface iface.calc#add
   |Sub ->
      handle_function_call iface iface.calc#sub
   |Mult ->
      handle_function_call iface iface.calc#mult
   |Div ->
      handle_function_call iface iface.calc#div
   |Neg ->
      handle_function_call iface iface.calc#neg
   |Inv ->
      handle_function_call iface iface.calc#inv
   |Pow ->
      handle_function_call iface iface.calc#pow
   |Sq ->
      handle_function_call iface iface.calc#sq
   |Sqrt ->
      handle_function_call iface iface.calc#sqrt
   |Abs ->
      handle_function_call iface iface.calc#abs
   |Arg ->
      handle_function_call iface iface.calc#arg
   |Exp ->
      handle_function_call iface iface.calc#exp
   |Ln ->
      handle_function_call iface iface.calc#ln
   |Ten_x ->
      handle_function_call iface iface.calc#ten_pow_x
   |Log10 ->
      handle_function_call iface iface.calc#log10
   |Conj ->
      handle_function_call iface iface.calc#conj
   |Sin ->
      handle_function_call iface iface.calc#sin
   |Cos ->
      handle_function_call iface iface.calc#cos
   |Tan ->
      handle_function_call iface iface.calc#tan
   |Sinh ->
      handle_function_call iface iface.calc#sinh
   |Cosh ->
      handle_function_call iface iface.calc#cosh
   |Tanh ->
      handle_function_call iface iface.calc#tanh
   |Asin ->
      handle_function_call iface iface.calc#asin
   |Acos ->
      handle_function_call iface iface.calc#acos
   |Atan ->
      handle_function_call iface iface.calc#atan
   |Asinh ->
      handle_function_call iface iface.calc#asinh
   |Acosh ->
      handle_function_call iface iface.calc#acosh
   |Atanh ->
      handle_function_call iface iface.calc#atanh
   |Re ->
      handle_function_call iface iface.calc#re
   |Im ->
      handle_function_call iface iface.calc#im
   |Gamma ->
      handle_function_call iface iface.calc#gamma
   |LnGamma ->
      handle_function_call iface iface.calc#lngamma
   |Erf ->
      handle_function_call iface iface.calc#erf
   |Erfc ->
      handle_function_call iface iface.calc#erfc
   |Fact ->
      handle_interr_function_call iface iface.calc#fact
   |Transpose ->
      handle_function_call iface iface.calc#transpose
   |Mod ->
      handle_function_call iface iface.calc#mod_int
   |Floor ->
      handle_function_call iface iface.calc#floor
   |Ceiling ->
      handle_function_call iface iface.calc#ceiling
   |ToInt ->
      handle_function_call iface iface.calc#to_int
   |ToFloat ->
      handle_function_call iface iface.calc#to_float
   |SolveLin ->
      handle_function_call iface iface.calc#solve_linear
   |Eval ->
      handle_function_call iface iface.calc#eval
   |Store ->
      handle_function_call iface iface.calc#store
   |Purge ->
      handle_function_call iface iface.calc#purge
   |Gcd ->
      handle_interr_function_call iface iface.calc#gcd
   |Lcm ->
      handle_interr_function_call iface iface.calc#lcm
   |Binom ->
      handle_interr_function_call iface iface.calc#binom
   |Perm ->
      handle_interr_function_call iface iface.calc#permutations
   |Total ->
      handle_function_call iface iface.calc#total
   |Mean ->
      handle_function_call iface iface.calc#mean
   |Sumsq ->
      handle_function_call iface iface.calc#sum_squares
   |Var ->
      handle_function_call iface iface.calc#variance_unbiased
   |VarBias ->
      handle_function_call iface iface.calc#variance_biased
   |Stdev ->
      handle_function_call iface iface.calc#standard_deviation_unbiased
   |StdevBias ->
      handle_function_call iface iface.calc#standard_deviation_biased
   |Min ->
      handle_function_call iface iface.calc#minimum
   |Max ->
      handle_function_call iface iface.calc#maximum
   |Utpn ->
      handle_function_call iface iface.calc#upper_tail_prob_normal
   |StandardizeUnits ->
      handle_function_call iface iface.calc#standardize_units
   |ConvertUnits ->
      handle_function_call iface iface.calc#convert_units
   |UnitValue ->
      handle_function_call iface iface.calc#unit_value
   |Trace ->
      handle_function_call iface iface.calc#trace
   end



let process_command (iface : interface_state_t) cc =
   begin match cc with
   |Drop ->
      handle_command_call iface iface.calc#drop
   |Clear ->
      handle_command_call iface iface.calc#clear
   |Swap ->
      handle_command_call iface iface.calc#swap
   |Dup ->
      handle_command_call iface iface.calc#dup
   |Undo ->
      handle_command_call iface iface.calc#undo
   |BeginBrowse ->
      handle_begin_browse iface
   |BeginAbbrev ->
      handle_begin_abbrev iface
   |BeginConst ->
      handle_begin_const iface
   |BeginVar ->
      handle_begin_variable iface
   |Quit ->
      handle_quit iface
   |SetRadians ->
      handle_command_call iface iface.calc#mode_rad;
      draw_help iface;
      draw_update_stack iface
   |SetDegrees ->
      handle_command_call iface iface.calc#mode_deg;
      draw_help iface;
      draw_update_stack iface
   |SetRect ->
      handle_command_call iface iface.calc#mode_rect;
      draw_help iface;
      draw_update_stack iface
   |SetPolar ->
      handle_command_call iface iface.calc#mode_polar;
      draw_help iface;
      draw_update_stack iface
   |SetBin ->
      handle_command_call iface iface.calc#mode_bin;
      draw_help iface;
      draw_update_stack iface
   |SetOct ->
      handle_command_call iface iface.calc#mode_oct;
      draw_help iface;
      draw_update_stack iface
   |SetDec ->
      handle_command_call iface iface.calc#mode_dec;
      draw_help iface;
      draw_update_stack iface
   |SetHex ->
      handle_command_call iface iface.calc#mode_hex;
      draw_help iface;
      draw_update_stack iface
   |ToggleAngleMode ->
      handle_command_call iface iface.calc#toggle_angle_mode;
      draw_help iface;
      draw_update_stack iface
   |ToggleComplexMode ->
      handle_command_call iface iface.calc#toggle_complex_mode;
      draw_help iface;
      draw_update_stack iface
   |CycleBase ->
      handle_command_call iface iface.calc#cycle_base;
      draw_help iface;
      draw_update_stack iface
   |View ->
      handle_view iface 
   |About ->
      handle_about iface 
   |Refresh ->
      handle_refresh iface
   |EnterPi ->
      handle_command_call iface iface.calc#enter_pi
   |Rand ->
      handle_command_call iface iface.calc#rand
   |EditInput ->
      handle_edit_input iface
   |CycleHelp ->
      handle_cycle_help iface
   end



(****************************************************************)
(* IMPLEMENTATION OF ABBREVIATION AND CONSTANT ENTRY SYSTEM     *)
(****************************************************************)



(* exit abbrev entry *)
let handle_exit_abbrev (iface : interface_state_t) =
   if iface.interface_mode = AbbrevEditMode then begin
      iface.interface_mode <- StandardEditMode;
      iface.abbrev_entry_buffer <- "";
      draw_help iface;
      draw_update_entry iface
   end else
      ()


(* search through a list of commands and find all that match
 * iface.abbrev_entry_buffer.
 * The list is built up in reverse order using Str.search_backward,
 * so the head of the list is actually the first match. *)
let match_abbrev_buffer (iface : interface_state_t) buf =
   if String.length buf > 0 then
      let regex = Str.regexp_string buf in
      let find_matches prev_matches el =
         if Str.string_match regex el 0 then
            el :: prev_matches
         else
            prev_matches
      in
      let match_list =
         match iface.abbrev_or_const with
         |IsAbbrev ->
            List.fold_left find_matches [] !Rcfile.abbrev_commands
         |IsConst ->
            List.fold_left find_matches [] Const.constant_symbols
      in
      if List.length match_list = 0 then raise Not_found
      else match_list 
   else 
      raise Not_found


(* backspace during abbrev entry *)
let handle_abbrev_backspace (iface : interface_state_t) =
   let len = String.length iface.abbrev_entry_buffer in
   if len > 0 then begin
      iface.abbrev_entry_buffer <- Str.string_before iface.abbrev_entry_buffer 
      (pred len);
      begin try 
         iface.matched_abbrev_list <- 
         match_abbrev_buffer iface iface.abbrev_entry_buffer
      with 
         Not_found -> ()
      end;
      draw_help iface;
      draw_update_entry iface
   end else
      ()


(* handle entry of an arbitrary character in abbrev mode *)
let handle_abbrev_character (iface : interface_state_t) key =
   let ch = char_of_int key in
   let test_buffer = iface.abbrev_entry_buffer ^ (String.make 1 ch) in
   (* search through the list of commands for the first one that matches
    * iface.abbrev_entry_buffer *)
   try
      iface.matched_abbrev_list <- match_abbrev_buffer iface test_buffer;
      iface.abbrev_entry_buffer <- test_buffer;
      draw_help iface;
      draw_update_entry iface
   with
      Not_found -> let err = beep () in ()


(* enter an abbrev entry *)
let handle_enter_abbrev (iface : interface_state_t) =
   if iface.interface_mode = AbbrevEditMode then begin
      iface.interface_mode <- StandardEditMode;
      begin try
         iface.matched_abbrev_list <- 
            match_abbrev_buffer iface iface.abbrev_entry_buffer;
         let first_abbrev_match = 
            if iface.matched_abbrev_list = [] then ""
            else List.hd iface.matched_abbrev_list
         in
         begin match iface.abbrev_or_const with
         |IsAbbrev ->
            let operation = Rcfile.translate_abbrev first_abbrev_match in
            begin match operation with
            |Function ff -> 
               process_function iface ff
            |Command cc  -> process_command iface cc
            |_ -> failwith 
               "found abbrev command that is neither Function nor Command"
            end;
            (* check whether ff should be autobound *)
            begin try
               let _ = Rcfile.key_of_operation operation in ()
            with Not_found ->
               register_autobinding operation
            end
         |IsConst ->
            let con = Const.translate_symbol first_abbrev_match in
            iface.calc#enter_const con;
         end
      with
         Not_found -> ()
      end;
      iface.abbrev_entry_buffer <- "";
      draw_help iface;
      draw_stack iface;
      draw_update_entry iface
   end else
      ()


(****************************************************************)
(* IMPLEMENTATION OF VARIABLE ENTRY SYSTEM                      *)
(****************************************************************)

(* exit variable entry *)
let handle_exit_variable (iface : interface_state_t) =
   if iface.interface_mode = VarEditMode then begin
      iface.interface_mode <- StandardEditMode;
      iface.entry_type <- FloatEntry;
      iface.variable_entry_buffer <- "";
      draw_help iface;
      draw_update_entry iface
   end else
      ()


(* search through a list of variables and find all that match
 * iface.variable_entry_buffer. *)
let match_variable_buffer (iface : interface_state_t) buf =
   let buf_regex = Str.regexp_string buf in
   let find_matches prev_result el =
      if Str.string_match buf_regex el 0 then
         el :: prev_result
      else
         prev_result
   in
   List.fold_left find_matches [] (List.rev iface.sorted_variables)



(* backspace during variable entry *)
let handle_variable_backspace (iface : interface_state_t) =
   let len = String.length iface.variable_entry_buffer in
   if len > 0 then begin
      iface.completion <- None;
      iface.variable_entry_buffer <- Str.string_before iface.variable_entry_buffer 
      (pred len);
      iface.matched_variables <- 
         match_variable_buffer iface iface.variable_entry_buffer;
      draw_help iface;
      draw_update_entry iface
   end else
      ()


(* handle entry of an arbitrary character in variable entry mode *)
let handle_variable_character (iface : interface_state_t) key =
   (* variables with long strings simply aren't useful, and
    * it's possible that deleting them could become difficult. *)
   if String.length iface.variable_entry_buffer < 25 then
      let allowable_regex = Str.regexp "[-a-zA-Z0-9_]" in
      let ch = char_of_int key in
      let ss = String.make 1 ch in
      if Str.string_match allowable_regex ss 0 then
         let test_buffer = iface.variable_entry_buffer ^ ss in
         (* search through the list of variables for the first one that matches
          * iface.variable_entry_buffer *)
         begin try
            iface.completion <- None;
            iface.matched_variables <- match_variable_buffer iface test_buffer;
            iface.variable_entry_buffer <- test_buffer;
            draw_help iface;
            draw_update_entry iface
         with
            Not_found ->
               iface.matched_variables <- [];
               iface.variable_entry_buffer <- test_buffer;
               draw_help iface;
               draw_update_entry iface
         end
      else
         let _ = beep () in ()
   else
      ()


(* enter an variable entry *)
let handle_enter_variable (iface : interface_state_t) =
   if iface.interface_mode = VarEditMode then begin
      if String.length iface.variable_entry_buffer > 0 then
         push_entry iface
      else
         iface.entry_type <- FloatEntry;
      iface.interface_mode <- StandardEditMode;
      iface.completion <- None;
      iface.variable_entry_buffer <- "";
      draw_help iface;
      draw_stack iface;
      draw_update_entry iface
   end else
      ()


(* autocomplete a variable *)
let handle_complete_variable (iface : interface_state_t) =
   if List.length iface.matched_variables > 0 then begin
      begin match iface.completion with
      |None   -> 
         iface.completion <- Some 0;
         iface.variable_entry_buffer_back <- iface.variable_entry_buffer;
         iface.variable_entry_buffer <- 
            List.nth iface.matched_variables 0
      |Some i -> 
         if i < List.length iface.matched_variables - 1 then begin
            iface.completion <- Some (succ i);
            iface.variable_entry_buffer <- 
               List.nth iface.matched_variables (succ i)
         end else begin
            iface.completion <- None;
            iface.variable_entry_buffer <- iface.variable_entry_buffer_back
         end
      end;
      draw_help iface;
      draw_update_entry iface
   end else
      let _ = beep () in ()



(****************************************************************)
(* IMPLEMENTATION OF UNITS ENTRY SYSTEM                         *)
(****************************************************************)

(* exit units entry *)
let handle_exit_units (iface : interface_state_t) =
   if iface.interface_mode = UnitEditMode then begin
      iface.interface_mode <- StandardEditMode;
      iface.units_entry_buffer <- "";
      iface.is_entering_units <- false;
      draw_update_entry iface
   end else
      ()


(* backspace during units entry *)
let handle_units_backspace (iface : interface_state_t) =
   let len = String.length iface.units_entry_buffer in
   if len > 0 then begin
      iface.units_entry_buffer <- Str.string_before iface.units_entry_buffer 
      (pred len);
      draw_update_entry iface
   end else begin
      iface.interface_mode <- StandardEditMode;
      iface.is_entering_units <- false;
      draw_update_entry iface
   end


(* handle entry of an arbitrary character in units entry mode *)
let handle_units_character (iface : interface_state_t) key =
   let allowable_regex = Str.regexp "[a-zA-Z0-9\\.\\^\\*/-]" in
   let ch = char_of_int key in
   let ss = String.make 1 ch in
   if Str.string_match allowable_regex ss 0 then begin
      iface.units_entry_buffer <- iface.units_entry_buffer ^ ss;
      draw_update_entry iface
   end else
      let _ = beep () in ()



(****************************************************************)
(* MAIN LOOP                                                    *)
(****************************************************************)

(* handle a keypress in standard entry mode *)
let handle_keypress_standard key (iface : interface_state_t) =
   (* check whether this keypress is a macro *)
   begin try
      let ch_list = Rcfile.macro_of_key key in
      let rec push_macro chars =
         match chars with
         | [] -> ()
         | head :: tail ->
            let _ = ungetch head in 
            push_macro tail
      in
      push_macro ch_list
   with Not_found ->
   (* if it's not a macro, go ahead and process it *)
      begin try
      (* editing operations take priority *)
         let edit_op = Rcfile.edit_of_key key in
         begin match edit_op with
         |Digit ->
            handle_digit iface key
         |Enter ->
            handle_enter iface
         |Backspace ->
            handle_backspace iface
         |Minus ->
            handle_minus iface
         |BeginInteger ->
            handle_begin_int iface
         |BeginComplex ->
            handle_begin_complex iface
         |BeginMatrix ->
            handle_begin_matrix iface
         |Separator ->
            handle_separator iface
         |Angle ->
            handle_angle iface
         |SciNotBase ->
            handle_scientific_notation iface
         |BeginUnits ->
            handle_begin_units iface
         end
      with Not_found | Not_handled ->
         (* next we try to match on functions *)
         try 
            let function_op = Rcfile.function_of_key key in
            process_function iface function_op
         with Not_found ->
            if iface.has_entry then
               (* finally we try entry of digits *)
               handle_digit iface key
            else
               (* commands are only suitable when there is no entry *)
               try 
                  let command_op = Rcfile.command_of_key key in
                  process_command iface command_op
               with Not_found ->
                  handle_digit iface key
      end
   end


(* handle a keypress in integer editing mode *)
let handle_keypress_intedit key (iface : interface_state_t) =
   begin try 
      let edit_op = Rcfile.edit_of_key key in
      begin match edit_op with
      |Digit ->
         handle_digit iface key
      |Enter ->
         handle_enter iface
      |Backspace ->
         handle_backspace iface
      |Minus ->
         handle_minus iface
      |SciNotBase ->
         handle_scientific_notation iface
      |_ -> raise Not_handled
      end
   with Not_found | Not_handled ->
      try
         let intedit_op = Rcfile.intedit_of_key key in
         begin match intedit_op with
         |IntEditExit ->
            handle_exit_int iface
         end
      with Not_found | Not_handled ->
         handle_digit iface key
   end


(* handle a keypress in units entry mode *)
let handle_keypress_unitedit key (iface : interface_state_t) =
   begin try
      let edit_op = Rcfile.edit_of_key key in
      begin match edit_op with
      |Enter ->
         handle_enter iface
      |Backspace ->
         handle_units_backspace iface
      |_ -> 
         handle_units_character iface key
      end
   with Not_found | Not_handled ->
      handle_units_character iface key
   end


(* handle a keypress in abbrev entry mode *)
let handle_keypress_abbrev key (iface : interface_state_t) =
   begin try
      let abbrev_op = Rcfile.abbrev_of_key key in
      begin match abbrev_op with
      |AbbrevExit ->
         handle_exit_abbrev iface
      |AbbrevEnter ->
         handle_enter_abbrev iface
      |AbbrevBackspace ->
         handle_abbrev_backspace iface
      end
   with Not_found ->
      handle_abbrev_character iface key
   end


(* handle a keypress in variable editing mode *)
let handle_keypress_varedit key (iface : interface_state_t) =
   begin try
      let varedit_op = Rcfile.varedit_of_key key in
      begin match varedit_op with
      |VarEditExit ->
         handle_exit_variable iface
      |VarEditEnter ->
         handle_enter_variable iface
      |VarEditBackspace ->
         handle_variable_backspace iface
      |VarEditComplete ->
         handle_complete_variable iface
      end
   with Not_found ->
      handle_variable_character iface key
   end


(* handle a keypress in stack browsing mode *)
let handle_keypress_browse key (iface : interface_state_t) =
   try
      let browse_op = Rcfile.browse_of_key key in
      begin match browse_op with
      |EndBrowse ->
         handle_end_browse iface
      |ScrollLeft ->
         handle_scroll_left iface
      |ScrollRight ->
         handle_scroll_right iface
      |RollDown ->
         handle_rolldown iface
      |RollUp ->
         handle_rollup iface
      |PrevLine ->
         handle_prev_line iface
      |NextLine ->
         handle_next_line iface
      |Echo ->
         handle_browse_echo iface
      |ViewEntry ->
         handle_browse_view iface
      |Drop1 ->
         handle_browse_drop1 iface
      |DropN ->
         handle_browse_dropn iface
      |Keep ->
         handle_browse_keep iface
      |KeepN ->
         handle_browse_keepn iface
      |EditEntry ->
         handle_browse_edit iface
      end
   with Not_found | Not_handled ->
      ()



let do_main_loop (iface : interface_state_t) =
   while iface.run_calc do
      iface.calc#launch_fill_in_thread ();
      let key = wgetch iface.scr.entry_win in
      (* using the ncurses SIGWINCH handler to catch window resize events *)
      if key = Key.resize then
         handle_resize iface
      else
         match iface.interface_mode with
         |StandardEditMode -> handle_keypress_standard key iface
         |IntEditMode      -> handle_keypress_intedit key iface
         |AbbrevEditMode   -> handle_keypress_abbrev key iface
         |VarEditMode      -> handle_keypress_varedit key iface
         |UnitEditMode     -> handle_keypress_unitedit key iface
         |BrowsingMode     -> handle_keypress_browse key iface
   done


(* initialize the interface and begin the main loop *)
let run (iface : interface_state_t) =
   iface.calc#backup ();
   assert (keypad iface.scr.entry_win true);

   (* initialize buffers for matrix entry *)
   for i = 1 to pred max_matrix_size do
      iface.gen_buffer.(i) <- 
         {re_mantissa = ""; re_exponent = "";
         im_mantissa = ""; im_exponent = ""; is_polar = false}
   done;

   begin
      try
         iface.calc#load_state ();
         draw_stack iface;
         draw_help iface;
         draw_update_entry iface;
      with
         Invalid_argument err ->
            draw_stack iface;
            draw_help iface;
            draw_error iface err;
            draw_update_entry iface
   end;
   do_main_loop iface
        




(* arch-tag: DO_NOT_CHANGE_b4519dd2-7e94-4cbf-931a-bb5f97445cbf *)
