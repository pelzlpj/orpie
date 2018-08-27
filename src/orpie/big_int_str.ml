(*  Orpie -- a fullscreen RPN calculator for the console
 *  Copyright (C) 2003-2004, 2005, 2006-2007, 2010, 2018 Paul Pelzl
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License, Version 3,
 *  as published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  Please send bug reports, patches, etc. to Paul Pelzl at
 *  <pelzlpj@gmail.com>.
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



(* Represent an integer in a given base.  (Continually divide the integer
 * by the base until the remainder is zero.)  This uses ordinary integer
 * division, so it can be done in constant time (< Sys.word_size divisions).
 * This is not designed to be used externally; no error checking is performed
 * in order to keep it fast. *)
let string_of_positive_int_base (num : int) (base : int) =
   let rec get_digit quotient zero_str str =
      if quotient = 0 then
         str
      else
         let quot = quotient / base
         and rem  = quotient mod base in
         let digit = digits.[rem] in
         begin match rem with
         |0 ->
            get_digit quot ((String.make 1 digit) ^ zero_str) str
         |_ ->
            get_digit quot "" ((String.make 1 digit) ^ zero_str ^ str)
         end
   in
   get_digit num "" ""


(* Divide a bignum by a power of 2, yielding a quotient and a remainder.
 * Faster than general case division because it can be done using bit blitting 
 * and shifting. *)
let quomod_power_2 bignum bits =
   let shift_words = bits / Sys.word_size
   and shift_bits  = bits mod Sys.word_size in
   let quot        = nat_of_big_int bignum in
   let quot_len    = Nat.length_nat quot in
   let rem         = Nat.create_nat (succ shift_words) in
   (* copy the 'shift_words' least significant words of quot over to rem *)
   Nat.blit_nat rem 0 quot 0 shift_words;
   Nat.set_to_zero_nat rem shift_words 1;
   (* shift over the remaining 'shift_bits' bits from quot to rem *)
   Nat.shift_right_nat quot shift_words (quot_len - shift_words) rem shift_words
   shift_bits;
   (* adjust the most significant word of rem *)
   let dummy = Nat.create_nat 1 in
   Nat.shift_right_nat rem shift_words 1 dummy 0 (Sys.word_size - shift_bits);
   let big_quot = big_int_of_nat (Nat.copy_nat quot shift_words (quot_len -
   shift_words))
   and big_rem  = big_int_of_nat rem in 
   (big_quot, big_rem)
   


(* Divide-and-conquer algorithm for conversion to bases that are powers of two
 * (binary, octal, and hex, for example).  The big_int is divided by a power
 * of two that is also a power of 'base'; the power is chosen to approximately
 * cut the big_int in half.  This division can be performed using only blits
 * and bit shifts, so it is much faster than ordinary big_int division.
 * The two pieces are recursively subdivided and the resulting strings are
 * concatenated together. O(n*log(n)) time.  This function runs significantly
 * faster than string_of_big_int_base_gen. *)
let string_of_big_int_base (num : big_int) (base : int) =
   let log2_base =
      match base with
      |2  -> 1
      |4  -> 2
      |8  -> 3
      |16 -> 4
      |32 -> 5
      |_  ->
         let err_str =
            Printf.sprintf "Error: called string_of_big_int_base() with illegal base %d"
            base
         in
         raise (Big_int_string_failure err_str)
   in
   let rec str_of_big_int_aux (ival : big_int) =
      if is_int_big_int ival then
         string_of_positive_int_base (int_of_big_int ival) base
      else begin
         let num_words = num_digits_big_int ival in
         let half_binary_digits = num_words * Sys.word_size / 2 in
         (* rounded_digits mod log2_base = 0, therefore
          * division by (2^rounded_digits) will split ival
          * perfectly with respect to the base 'base'. *)
         let rounded_digits = half_binary_digits - 
         (half_binary_digits mod log2_base) in
         let upper, lower = quomod_power_2 ival rounded_digits in
         let upper_string = str_of_big_int_aux upper
         and lower_string = str_of_big_int_aux lower in
         (* pad the lower_string with zeros as necessary *)
         let adj_len_lower = rounded_digits / log2_base in
         let zeros = 
            String.make (adj_len_lower - (String.length lower_string)) '0'
         in
         upper_string ^ zeros ^ lower_string
      end
   in
   let s = str_of_big_int_aux (abs_big_int num) in
   match sign_big_int num with
   |0    -> "0"
   |1    -> s
   |(-1) -> "-" ^ s
   |x    -> 
      raise (Big_int_string_failure ("unmatched sign: " ^ (string_of_int x)))





(* Divide-and-conquer algorithm for string representation of big_ints in a
* desired base (general case--not necessarily a power of two).  The big_int 
* is split approximately in half using this divisor:  
* base^(num_words * log_base(2^word_size) / 2) 
* Each half is split recursively, and the pieces are concatenated together.
* Should run in O(n*log(n)) time, a big improvement on the standard O(n^2)
* algorithm that requires one long division for every digit output. *)
(* Note 1: This runs in logarithmic stack space, so we should be able to handle
 * some pretty large big_ints before worrying about blowing the stack. *)
(* Note 2: A faster method for computing a divisor could make this go a
 * lot quicker yet; gprof indicates that most of the time is spent in
 * multiplication ==> the power_int_positive_int_base call. *)
(* Note 3: This routine actually appears to outperform string_of_big_int()
 * for computing decimal representations.  CPU time decrease looks to be around
 * a third. *)
let string_of_big_int_base_gen (num : big_int) (base : int) =
   if base >= 2 && base <= 36 then
      let rec str_of_big_int_aux (ival : big_int) =
         if is_int_big_int ival then
            string_of_positive_int_base (int_of_big_int ival) base
         else begin
            let num_words   = num_digits_big_int ival in
            let size_factor = (log (2.0 ** (float_of_int Sys.word_size))) /.
            (log (float_of_int base)) in
            let log_div        = (num_words * (int_of_float size_factor)) / 2 in
            let divisor        = power_int_positive_int base log_div in
            let (upper, lower) = quomod_big_int ival divisor in
            let upper_string   = str_of_big_int_aux upper
            and lower_string   = str_of_big_int_aux lower in
            (* pad the lower_string with zeros as necessary *)
            let zeros = String.make (log_div - (String.length lower_string)) '0' in
            upper_string ^ zeros ^ lower_string
         end
      in
      let s = str_of_big_int_aux (abs_big_int num) in
      match sign_big_int num with
      |0    -> "0"
      |1    -> s
      |(-1) -> "-" ^ s
      |x    -> 
         raise (Big_int_string_failure ("unmatched sign: " ^ (string_of_int x)))
   else
      raise (Big_int_string_failure ("unsupported base: " ^ (string_of_int base)))





(* convert a string to a big_int, assuming base 'base' *)
(* The algorithm is simple... add up the values of the digits. *)
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
