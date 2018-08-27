(*  Orpie -- a fullscreen RPN calculator for the console
 *  Copyright (C) 2003-2004, 2005, 2006-2007, 2010 Paul Pelzl
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
 *  <pelzlpj@gmail.com>.
 *)

(* statefile.ml
 * This file contains code for saving and loading the calculator
 * state. *)


open Rpc_stack


(* save to a datafile using the Marshal module *)
let save_state ((modes : calculator_modes),
(variables : (string, orpie_data_t) Hashtbl.t),
((data_stack : stack_data_t array), (data_len : int))) =
   try
      let version_file = Utility.join_path !(Rcfile.datadir) "version" in
      let version_channel = Utility.open_or_create_out_bin version_file in
      output_string version_channel Version.version;
      close_out version_channel;
      let save_file = Utility.join_path !(Rcfile.datadir) "calc_state" in
      let save_channel = Utility.open_or_create_out_bin save_file in
      Marshal.to_channel save_channel 
      (modes, variables, !Rcfile.autobind_keys, data_stack, data_len) [];
      close_out save_channel
   with
      |Sys_error ss -> raise (Invalid_argument "can't open data file for writing")
      |Failure ff   -> raise (Invalid_argument "can't serialize calculator data to file")


(* load from a datafile using the Marshal module *)
let load_state () =
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
               let data_modes, data_variables, data_autobind_keys, data_stack, data_len = 
                  (Marshal.from_channel load_channel : calculator_modes * 
                  ((string, orpie_data_t) Hashtbl.t) * 
                  (int * string * Operations.operation_t option * int) array *
                  (stack_data_t array) * int)
               in
               close_in load_channel;
               Rcfile.validate_saved_autobindings data_autobind_keys;
               (data_modes, data_variables, Some (data_stack, data_len))
            end else
               (* if the datafile is missing, do nothing as it will be
                * created later *)
               ({angle = Rad; base = Dec; complex = Rect}, Hashtbl.create 20, None)
         end else
            (* if the version strings don't match, don't try loading anything *)
            ({angle = Rad; base = Dec; complex = Rect}, Hashtbl.create 20, None)
      end else
         (* if the version file does not exist, don't try loading anything *)
         ({angle = Rad; base = Dec; complex = Rect}, Hashtbl.create 20, None)
   with
      (* this gets raised if, for example, we don't have read permission
       * on the state data file *)
      |Sys_error ss -> raise (Invalid_argument "can't open calculator state data file")
      (* this shouldn't happen unless the data file gets corrupted. *)
      |Failure ff -> raise (Invalid_argument "can't deserialize calculator data from file")



(* arch-tag: DO_NOT_CHANGE_00dfe125-d9a3-4eca-842d-2a7e29cd29d0 *)
