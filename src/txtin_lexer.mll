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

(* txtin_lexer.mll
 *
 * Orpie can handle input of data from a textfile created by an external editor.
 * editor_lexer.mll generates a lexer that tokenizes this data.
 *)

{
   open Txtin_parser
}

(* space, tab, CR, linefeed, vertical tab *)
let whitespace    = [' ' '\010' '\013' '\009' '\012']
let digit         = ['0'-'9']
let hex_digit     = ['0'-'9' 'a'-'f' 'A'-'F']
let base_ident    = ['b' 'o' 'd' 'h']
let sign          = ['-' '+']
let variable_char = ['a'-'z' 'A'-'Z' '0'-'9' '-' '_']
let units         = ['a'-'z' 'A'-'Z' '0'-'9' '.' '-' '*' '/' '^']

rule token =
   parse whitespace+ {token lexbuf}
   | '#' sign? hex_digit+ '`' base_ident {
      let s = Lexing.lexeme lexbuf in
      let int_str = String.sub s 1 (String.length s - 1) in
      INTEGER int_str}

   | '@' variable_char+ {
      let s = Lexing.lexeme lexbuf in
      let var_str = String.sub s 1 (String.length s - 1) in
      VARIABLE var_str}

   | ((sign? digit+ ('.' digit*)?) | (sign? digit* '.' digit+)) ('e' sign? digit+)? {
      FLOAT (Lexing.lexeme lexbuf)}

   | '_' units* {
      UNITS (Lexing.lexeme lexbuf)}

   | '(' 
      { BEGINCOMPLEX }

   | ')'
      { ENDCOMPLEX }

   | ','
      { SEPARATOR }

   | '<'
      { ANGLE }

   | '['
      { BEGINMATRIX }

   | ']'
      { ENDMATRIX }

   | eof 
      { EOF}


(* arch-tag: DO_NOT_CHANGE_d4979b04-40d1-47e1-aab6-a04c0ded49ff *)
