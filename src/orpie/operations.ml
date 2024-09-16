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


(* A function will push any item in the entry buffer before performing the
 * operation.  A command does not take input, so it is not allowed when data is
 * in the entry buffer.  An edit is an operation that acts on the data in the
 * entry buffer (e.g. backspace). *)
type function_operation_t = | Add | Sub | Mult | Div | Neg | Inv
                            | Pow | Sqrt | Sq | Abs | Arg | Exp | Ln 
                            | Ten_x | Log10 | Conj | Sin | Cos | Tan 
                            | Asin | Acos | Atan | Sinh | Cosh | Tanh
                            | Asinh | Acosh | Atanh | Re | Im 
                            | Gamma | LnGamma | Erf | Erfc | Fact
                            | Transpose | Mod | Floor | Ceiling
                            | ToInt | ToFloat | SolveLin | Eval
                            | Store | Purge | Gcd | Lcm | Binom | Perm
                            | Total | Mean | Sumsq | Var |VarBias
                            | Stdev | StdevBias | Min | Max 
                            | Utpn | StandardizeUnits | ConvertUnits
                            | UnitValue | Trace;;

type command_operation_t  = | Drop | Clear | Swap | Dup | Undo
                            | BeginBrowse | BeginAbbrev | BeginVar | Quit
                            | SetRadians | SetDegrees | SetRect | SetPolar
                            | SetBin | SetOct | SetDec | SetHex
                            | ToggleAngleMode | ToggleComplexMode | CycleBase
                            | View | About | Refresh | EnterPi | Rand
                            | EditInput | CycleHelp | BeginConst;;

type edit_operation_t     = | Digit | Enter | Backspace | Minus | SciNotBase 
                            | BeginInteger | BeginComplex | BeginMatrix
                            | Separator | Angle | BeginUnits;;

type browse_operation_t   = | EndBrowse
                            | ScrollLeft | ScrollRight
                            | SwapDown | SwapUp | RollDown | RollUp
                            | PrevLine | NextLine | Echo | ViewEntry
                            | Drop1 | DropN | Keep | KeepN
                            | EditEntry;;

type abbrev_operation_t = | AbbrevExit | AbbrevEnter | AbbrevBackspace;;

type integer_edit_operation_t = | IntEditExit;;

type var_edit_operation_t = | VarEditExit | VarEditEnter | VarEditBackspace
                            | VarEditComplete;;

type operation_t = | Function of function_operation_t 
                   | Command  of command_operation_t
                   | Edit     of edit_operation_t
                   | Browse   of browse_operation_t
                   | Abbrev   of abbrev_operation_t
                   | IntEdit  of integer_edit_operation_t
                   | VarEdit  of var_edit_operation_t;;




(* arch-tag: DO_NOT_CHANGE_e761ca10-6bfd-4edf-a3de-53778a07ca21 *)
