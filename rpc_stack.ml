(*  Orpie -- a stack-based RPN calculator for the console
 *  Copyright (C) 2003-2004  Paul Pelzl
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

(* rpc_stack.ml -- implementation of internal calculator stack that holds
 *                 multiple data types
 *
 * The stack is implemented as a dynamically-allocated array.  This approach
 * enables non-standard stack operations such as random access and cyclic rotation of
 * elements.
 *)


open Big_int
open Big_int_str
open Printf

exception Stack_error of string

type orpie_data = | RpcInt of Big_int.big_int
                  | RpcFloat of float
                  | RpcComplex of Complex.t
                  | RpcFloatMatrix of Gsl_matrix.matrix 
                  | RpcComplexMatrix of Gsl_matrix_complex.matrix
                  | RpcVariable of string

type angle_mode   = | Rad | Deg
type base_mode    = | Bin | Oct | Hex | Dec
type complex_mode = | Rect | Polar
      
type calculator_modes = {angle : angle_mode; base : base_mode; 
                         complex : complex_mode}

let size_inc = 100
let pi = 3.14159265358979323846

class rpc_stack =
   object(self)
      val mutable len = 0
      val mutable stack = Array.make size_inc (RpcFloat 0.0)

      method length = len


      (* save to a datafile using the Marshal module *)
      method save_state (modes : calculator_modes) 
      (variables : (string, orpie_data) Hashtbl.t) =
         try
            let version_file = Utility.join_path !(Rcfile.datadir) "version" in
            let version_channel = Utility.open_or_create_out_bin version_file in
            output_string version_channel Version.version;
            close_out version_channel;
            let save_file = Utility.join_path !(Rcfile.datadir) "calc_state" in
            let save_channel = Utility.open_or_create_out_bin save_file in
            Marshal.to_channel save_channel (modes, variables, stack, len) [];
            close_out save_channel
         with
            |Sys_error ss -> raise (Invalid_argument "can't open data file for writing")
            |Failure ff   -> raise (Invalid_argument "can't serialize calculator data to file")

      (* load from a datafile using the Marshal module *)
      (* FIXME: if the datafile is corrupted, this can segfault... *)
      method load_state () =
         try
            (* check whether the version file exists *)
            let version_file = Utility.join_path !(Rcfile.datadir) "version" in
            if Sys.file_exists (Utility.expand_file version_file) then begin
               (* if it does exist, try loading it *)
               let version_channel = 
                  Utility.expand_open_in_ascii version_file
               in
               let ver_string = input_line version_channel in
               close_in version_channel;
               (* if the version strings match, then assume it's okay to use
                * Marshal. *)
               if ver_string = Version.version then begin
                  (* check whether the state file exists *)
                  let datafile = Utility.join_path !(Rcfile.datadir) "calc_state" in
                  if Sys.file_exists (Utility.expand_file datafile) then begin
                     (* if it does exist, try loading it *)
                     let load_channel = Utility.expand_open_in_bin datafile in
                     let data_modes, data_variables, data_stack, data_len = 
                        (Marshal.from_channel load_channel : calculator_modes * 
                        ((string, orpie_data) Hashtbl.t) * (orpie_data array) * int)
                     in
                     close_in load_channel;
                     stack <- data_stack;
                     len <- data_len;
                     data_modes, data_variables
                  end else
                     (* if the datafile is missing, do nothing as it will be
                      * created later *)
                     ({angle = Rad; base = Dec; complex = Rect}, Hashtbl.create 20)
               end else
                  (* if the version strings don't match, don't try loading anything *)
                  ({angle = Rad; base = Dec; complex = Rect}, Hashtbl.create 20)
            end else
               (* if the version file does not exist, don't try loading anything *)
                  ({angle = Rad; base = Dec; complex = Rect}, Hashtbl.create 20)
         with
            (* this gets raised if, for example, we don't have read permission
             * on the state data file *)
            |Sys_error ss -> raise (Invalid_argument "can't open calculator state data file")
            (* this shouldn't happen unless the data file gets corrupted. *)
            |Failure ff -> raise (Invalid_argument "can't deserialize calculator data from file")

            
      method backup () =
         let b_stack = Array.copy stack and
         b_len = len in
         {< len = b_len; stack = b_stack >}


      method push v =
         (* allocate a new stack if necessary *)
         begin
            if len >= Array.length stack then
               let new_stack = Array.make ((Array.length stack) + size_inc)
               (RpcFloat 0.0) in
               Array.blit stack 0 new_stack 0 (Array.length stack);
               stack <- new_stack
            else
               ();
            (stack.(len) <- v;
            len <- len + 1)
         end


      method pop () =
         (* compact stack memory by size_inc whenever we have 2 * size_inc
          * elements free *)
         begin
            (if len < (Array.length stack) - 2 * size_inc then
               let new_stack = Array.sub stack 0 ((Array.length stack) -
               size_inc) in
               stack <- new_stack
            else
               ()
            );
            if len > 0 then
               (len <- len - 1;
               stack.(len))
            else
               raise (Stack_error "cannot pop empty stack")
         end


      (* cyclically roll all stack elements downward (i.e. towards the top
       * of the stack), starting below element number 'num' (inclusive). *)
      method rolldown num =
         if num <= len then
            let temp = stack.(pred len) in
            for i = pred len downto len - num + 1 do
               stack.(i) <- stack.(pred i)
            done;
            stack.(len - num) <- temp
         else
            raise (Stack_error "insufficient stack elements")


      (* cyclically roll all stack elements upward (i.e. away from the top
       * of the stack), starting below element number 'num' (inclusive). *)
      method rollup num =
         if num <= len then
            let temp = stack.(len - num) in
            for i = len - num to len - 2 do
               stack.(i) <- stack.(succ i)
            done;
            stack.(pred len) <- temp
         else
            raise (Stack_error "insufficient stack elements")


      (* delete a particular element *)
      method delete num =
         if num <= len then
            (for i = (len - num) to len do
               stack.(i) <- stack.(succ i)
            done;
            len <- (pred len))
         else
            raise (Stack_error "insufficient stack elements")


      (* delete all elements below level N *)
      method deleteN num =
         if num <= len then
            len <- len - num
         else
            raise (Stack_error "insufficient stack elements")


      (* keep only a particular stack element *)
      method keep num =
         if num <= len then
            (stack.(0) <- stack.(len - num);
            len <- 1)
         else
            raise (Stack_error "insufficient stack elements")


      (* keep all elements below the selected (inclusive) *)
      method keepN num =
         if num <= len then
            (for i = 0 to num - 1 do
               stack.(i) <- stack.(i + len - num)
            done;
            len <- num)
         else
            raise (Stack_error "insufficient stack elements")


      (* return a particular stack element without removing it from the stack *)
      (* element 1 points to the top of the stack *)
      method peek el_num =
         if el_num <= len then
            let actual_el_num = len - el_num in
            stack.(actual_el_num)
         else
            let s = Printf.sprintf "cannot access nonexistant stack element %d"
            el_num in
            raise (Stack_error s)


      (* generate a string to represent a particular stack element.
       * The top stack element (stack.(len-1)) is defined to be element 
       * number 1. *)
      method get_display_string line_num calc_modes =
         if line_num > 0 then
            if line_num <= len then
               (* this is the actual index into the array *)
               let index = len - line_num
               and make_string gen_el =
                  begin match gen_el with
                  |RpcInt el -> 
                     begin
                        match calc_modes.base with
                        |Bin ->
                           let s = string_of_big_int_base el 2 in
                           "# " ^ s ^ " b"
                        |Oct ->
                           let s = string_of_big_int_base el 8 in
                           "# " ^ s ^ " o"
                        |Hex ->
                           let s = string_of_big_int_base el 16 in
                           "# " ^ s ^ " h"
                        |Dec ->
                           let s = string_of_big_int el in
                           "# " ^ s ^ " d"
                     end
                  |RpcFloat el ->
                     sprintf "%.15g" el
                  |RpcComplex el ->
                     begin
                        match calc_modes.complex with
                        |Rect ->
                           sprintf "(%.15g, %.15g)" el.Complex.re el.Complex.im
                        |Polar ->
                           begin
                              let r = sqrt (el.Complex.re *. el.Complex.re +.
                              el.Complex.im *. el.Complex.im)
                              and theta = atan2 el.Complex.im el.Complex.re in
                              match calc_modes.angle with
                              |Rad ->
                                 sprintf "(%.15g <%.15g)" r theta
                              |Deg ->
                                 sprintf "(%.15g <%.15g)" r (180.0 /. pi *. theta)
                           end
                     end
                  |RpcFloatMatrix el ->
                     (* looks like [[ a11, a12 ][ a21, a22 ]] *)
                     let rows, cols = (Gsl_matrix.dims el) in
                     let initial_string = "[" in
                     let line = ref initial_string in
                     for n = 0 to rows - 1 do
                        line := !line ^ "[ ";
                        for m = 0 to cols - 2 do
                           line := !line ^ (sprintf "%.15g, " el.{n, m})
                        done;
                        line := !line ^ (sprintf "%.15g ]" el.{n, cols-1})
                     done;
                     line := !line ^ "]";
                     !line
                  |RpcComplexMatrix el ->
                     (* looks like [[ (a11re, a11im), (a12re, a12im) ][ (a21re,
                        a21im), (a22re, a22im) ] *)
                     let rows, cols = (Gsl_matrix_complex.dims el) in
                     let initial_string = "[" in
                     let line = ref initial_string in
                     for n = 0 to rows - 1 do
                        line := !line ^ "[ ";
                        for m = 0 to cols - 2 do
                           match calc_modes.complex with
                           |Rect ->
                              line := !line ^ (sprintf "(%.15g, %.15g), " 
                                 el.{n, m}.Complex.re el.{n, m}.Complex.im)
                           |Polar ->
                              begin
                                 let rr = el.{n, m}.Complex.re
                                 and ii = el.{n, m}.Complex.im in
                                 let r = sqrt (rr *. rr +. ii *. ii)
                                 and theta = atan2 ii rr in
                                 match calc_modes.angle with
                                 |Rad ->
                                    line := !line ^ (sprintf "(%.15g <%.15g), " 
                                    r theta)
                                 |Deg ->
                                    line := !line ^ (sprintf "(%.15g <%.15g), " 
                                    r (180.0 /. pi *. theta))
                              end
                        done;
                        match calc_modes.complex with
                        |Rect ->
                           line := !line ^ (sprintf "(%.15g, %.15g) ]" 
                              el.{n, cols-1}.Complex.re el.{n, cols-1}.Complex.im)
                        |Polar ->
                           begin
                              let rr = el.{n, cols-1}.Complex.re
                              and ii = el.{n, cols-1}.Complex.im in
                              let r = sqrt (rr *. rr +. ii *. ii)
                              and theta = atan2 ii rr in
                              match calc_modes.angle with
                              |Rad ->
                                 line := !line ^ (sprintf "(%.15g <%.15g) ]" 
                                 r theta)
                              |Deg ->
                                 line := !line ^ (sprintf "(%.15g <%.15g) ]" 
                                 r (180.0 /. pi *. theta))
                           end
                     done;
                     line := !line ^ "]";
                     !line
                  |RpcVariable s ->
                     "@ " ^ s
                  end
               in
               make_string stack.(index)
            else (* line_num > len *)
               ""
         else (* line_num <= 0 *)
            raise (Stack_error ("cannot display nonexistent stack element " ^
               (string_of_int line_num)))



      (* generate a string to represent a particular stack element.
       * The top stack element (stack.(len-1)) is defined to be element 
       * number 1.  This version contains newlines and spaces matrix
       * elements in a form suitable for fullscreen display. *)
      (* FIXME: this is ugly; maybe it can be refactored somehow. *)
      method get_fullscreen_display_string line_num calc_modes =
         if line_num > 0 then
            if line_num <= len then
               (* this is the actual index into the array *)
               let index = len - line_num in
               match stack.(index) with
               |RpcInt el -> 
                  begin
                     match calc_modes.base with
                     |Bin ->
                        let s = string_of_big_int_base el 2 in
                        "#" ^ s ^ "_b"
                     |Oct ->
                        let s = string_of_big_int_base el 8 in
                        "#" ^ s ^ "_o"
                     |Hex ->
                        let s = string_of_big_int_base el 16 in
                        "#" ^ s ^ "_h"
                     |Dec ->
                        let s = string_of_big_int el in
                        "#" ^ s ^ "_d"
                  end
               |RpcFloatMatrix el ->
                  (* looks like [[ a11, a12 ]
                   *             [ a21, a22 ]] 
                   * and the columns are aligned. *)
                  let rows, cols = (Gsl_matrix.dims el) in
                  (* first get the maximum field width for each column *)
                  let max_width = Array.make cols 0 in
                  for m = 0 to pred cols do
                     for n = 0 to pred rows do
                        let dummy_string = sprintf "%-.15g" el.{n, m} in
                        let ds_len = String.length dummy_string in
                        if ds_len > max_width.(m) then
                           max_width.(m) <- ds_len
                        else
                           ()
                     done
                  done;
                  (* now use the maximum field widths to align the columns
                   * during string creation *)
                  let initial_string = "[" in
                  let line = ref initial_string in
                  for n = 0 to rows - 1 do
                     line := !line ^ "[ ";
                     for m = 0 to cols - 2 do
                        line := !line ^ (sprintf "%*.15g, " max_width.(m) el.{n, m})
                     done;
                     line := !line ^ (sprintf "%*.15g ]" max_width.(cols-1) el.{n, cols-1});
                     if n < pred rows then
                        line := !line ^ "\n "
                     else
                        ()
                  done;
                  line := !line ^ "]";
                  !line
               |RpcComplexMatrix el ->
                  (* looks like [[ (a11re, a11im), (a12re, a12im) ]
                   *             [ (a21re, a21im), (a22re, a22im) ] 
                   * with properly aligned columns *)
                  let rows, cols = (Gsl_matrix_complex.dims el) in
                  (* first get the maximum field width for each column *)
                  let max_width = Array.make_matrix cols 2 0 in
                  for m = 0 to pred cols do
                     for n = 0 to pred rows do
                        match calc_modes.complex with
                        |Rect ->
                           let dummy_re = sprintf "%-.15g" el.{n, m}.Complex.re in
                           let dr_len = String.length dummy_re in
                           if dr_len > max_width.(m).(0) then
                              max_width.(m).(0) <- dr_len
                           else
                              ();
                           let dummy_im = sprintf "%-.15g" el.{n, m}.Complex.im in
                           let di_len = String.length dummy_im in
                           if di_len > max_width.(m).(1) then
                              max_width.(m).(1) <- di_len
                           else
                              ()
                        |Polar ->
                           let rr = el.{n, m}.Complex.re
                           and ii = el.{n, m}.Complex.im in
                           let r = sqrt (rr *. rr +. ii *. ii)
                           and theta = atan2 ii rr in
                           let dummy_r = sprintf "%-.15g" r in
                           let r_len = String.length dummy_r in
                           if r_len > max_width.(m).(0) then
                              max_width.(m).(0) <- r_len
                           else
                              ();
                           let dummy_theta = 
                              match calc_modes.angle with
                              |Rad -> sprintf "%-.15g" theta
                              |Deg -> sprintf "%-.15g" (180.0 /. pi *. theta)
                           in
                           let theta_len = String.length dummy_theta in
                           if theta_len > max_width.(m).(1) then
                              max_width.(m).(1) <- theta_len
                           else
                              ();
                     done
                  done;
                  (* now use the maximum field widths to align the columns
                   * during string creation *)
                  let initial_string = "[" in
                  let line = ref initial_string in
                  for n = 0 to rows - 1 do
                     line := !line ^ "[ ";
                     for m = 0 to cols - 2 do
                        match calc_modes.complex with
                        |Rect ->
                           line := !line ^ (sprintf "(%*.15g, %*.15g), " 
                              max_width.(m).(0) el.{n, m}.Complex.re 
                              max_width.(m).(1) el.{n, m}.Complex.im)
                        |Polar ->
                           begin
                              let rr = el.{n, m}.Complex.re
                              and ii = el.{n, m}.Complex.im in
                              let r = sqrt (rr *. rr +. ii *. ii)
                              and theta = atan2 ii rr in
                              match calc_modes.angle with
                              |Rad ->
                                 line := !line ^ (sprintf "(%*.15g <%*.15g), " 
                                 max_width.(m).(0) r max_width.(m).(1) theta)
                              |Deg ->
                                 line := !line ^ (sprintf "(%*.15g <%*.15g), " 
                                 max_width.(m).(0) r max_width.(m).(1) 
                                 (180.0 /. pi *. theta))
                           end
                     done;
                     begin match calc_modes.complex with
                     |Rect ->
                        line := !line ^ (sprintf "(%*.15g, %*.15g) ]" 
                           max_width.(cols-1).(0) el.{n, cols-1}.Complex.re 
                           max_width.(cols-1).(1) el.{n, cols-1}.Complex.im)
                     |Polar ->
                        begin
                           let rr = el.{n, cols-1}.Complex.re
                           and ii = el.{n, cols-1}.Complex.im in
                           let r = sqrt (rr *. rr +. ii *. ii)
                           and theta = atan2 ii rr in
                           match calc_modes.angle with
                           |Rad ->
                              line := !line ^ (sprintf "(%*.15g <%*.15g) ]" 
                              max_width.(cols-1).(0) r max_width.(cols-1).(1) theta)
                           |Deg ->
                              line := !line ^ (sprintf "(%*.15g <%*.15g) ]" 
                              max_width.(cols-1).(0) r max_width.(cols-1).(1) 
                              (180.0 /. pi *. theta))
                        end
                     end;
                     if n < pred rows then
                        line := !line ^ "\n "
                     else
                        ()
                  done;
                  line := !line ^ "]";
                  !line
               |RpcVariable s ->
                  "@" ^ s
               |_ ->
                  (* fall back on single-line display *)
                  self#get_display_string line_num calc_modes
            else (* line_num > len *)
               ""
         else (* line_num <= 0 *)
            raise (Stack_error ("cannot display nonexistent stack element " ^
               (string_of_int line_num)))

   end



(* arch-tag: DO_NOT_CHANGE_59b80e87-dfde-4203-a7a2-8e1f95813151 *)
