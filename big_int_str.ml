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

(* string coercion functions for Big_int
 *
 * These functions accept a base parameter, unlike the functions in the Big_int
 * module itself.  The algorithms are simple, but appear to be fast enough.
 *)

open Big_int;;
exception Big_int_string_failure of string;;

let digits = "0123456789abcdefghijklmnopqrstuvwxyz";;

(* 'values' maps characters to integer values;
   '0' = 0, '1' = 0, ... 'a' = 10, etc. *)
let values = Hashtbl.create 36;;
for i = 0 to pred (String.length digits) do
   Hashtbl.add values digits.[i] i
done;;


(* convert a big_int 'num' to a string, assuming a base 'base' *)
(* Algorithm: continually divide 'num' by 'base'.  The remainders
 *            are the digits in the string, right-to-left. 
 *            'zero_str' keeps track of the zeros that would be added
 *            to the front of the string; they are only prepended if there
 *            is a nonzero digit. *)
let string_of_big_int_base (num : big_int) (base : int) =
   if base >= 2 && base <= 36 then
      let big_base = big_int_of_int base in
      (* get_digit recursively adds one more digit on the left end of the string,
       * exiting when the quotient becomes zero. *)
      let rec get_digit quotient zero_str str =
         let test_zero = (compare_big_int quotient zero_big_int) in
         match test_zero with
         |0 ->
            str
         |1 ->
            let quot, rem = quomod_big_int quotient big_base in
            let irem = int_of_big_int rem in
            let digit = digits.[irem] in (
            match irem with 
            |0 ->
               get_digit quot ((String.make 1 digit) ^ zero_str) str
            |_ ->
               get_digit quot "" ((String.make 1 digit) ^ zero_str ^ str)
            )
         |_ ->
            raise (Big_int_string_failure "negative quotient")
      in
      let s = get_digit (abs_big_int num) "" "" in
      match sign_big_int num with
      |0 ->
         "0"
      |1 ->
         s
      |(-1) ->
         "-" ^ s
      |x ->
         raise (Big_int_string_failure ("unmatched sign: " ^ (string_of_int x)))
   else
      raise (Big_int_string_failure ("unsupported base: " ^ (string_of_int
             base)));;



(* convert a string to a big_int, assuming base 'base' *)
(* The algorithm is the simple... add up the values of the digits. *)
let big_int_of_string_base (str : string) (base : int) =
   let multiplier = ref unit_big_int and
   sum = ref zero_big_int in
   for i = pred (String.length str) downto 0 do
      match str.[i] with
      |'-' -> 
         sum := minus_big_int !sum
      |'+' ->
         ()
      |_ ->
         try
            let digit_value = (Hashtbl.find values str.[i]) in
            if digit_value < base then
               (let diff = mult_int_big_int digit_value !multiplier in
               sum        := add_big_int !sum diff;
               multiplier := mult_int_big_int base !multiplier)
            else
               raise (Big_int_string_failure ("invalid digits for base " ^
               (string_of_int base) ^ " integer data" ))
         with Not_found ->
            raise (Big_int_string_failure ("invalid digits for base " ^
            (string_of_int base) ^ " integer data" ))
   done;
   !sum;;
      
      



(* arch-tag: DO_NOT_CHANGE_16f12562-9499-46c4-8e3a-519da76c1622 *)
