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


(* A function will push any item in the entry buffer before performing the
 * operation.  A command does not take input, so it is not allowed when data is
 * in the entry buffer.  An edit is an operation that acts on the data in the
 * entry buffer (e.g. backspace). *)
type function_operation = | Add | Sub | Mult | Div | Neg | Inv
                          | Pow | Sqrt | Sq | Abs | Arg | Exp | Ln 
                          | Ten_x | Log10 | Conj | Sin | Cos | Tan 
                          | Asin | Acos | Atan | Sinh | Cosh | Tanh
                          | Asinh | Acosh | Atanh | Re | Im 
                          | Gamma | LnGamma | Erf | Erfc | Fact
                          | Transpose;;

type command_operation  = | Drop | Clear | Swap | Dup | Undo
                          | BeginBrowse | BeginExtended | Quit
                          | SetRadians | SetDegrees | SetRect | SetPolar
                          | SetBin | SetOct | SetDec | SetHex
                          | ToggleAngleMode | ToggleComplexMode | CycleBase
                          | View | About | Refresh | EnterPi;;

type edit_operation     = | Digit | Enter | Backspace | Minus | SciNotBase 
                          | BeginInteger | BeginComplex | BeginMatrix
                          | Separator | Angle;;

type browse_operation   = | EndBrowse
                          | ScrollLeft | ScrollRight | RollDown | RollUp
                          | PrevLine | NextLine | Echo | ViewEntry;;

type extended_operation = | ExitExtended | EnterExtended | ExtBackspace;;

type integer_edit_operation = | ExitIntEdit;;

type operation = | Function of function_operation 
                 | Command of command_operation
                 | Edit of edit_operation
                 | Browse of browse_operation
                 | Extend of extended_operation
                 | IntEdit of integer_edit_operation;;




(* arch-tag: DO_NOT_CHANGE_e761ca10-6bfd-4edf-a3de-53778a07ca21 *)
