
(* A function will push any item in the entry buffer before performing the
 * operation.  A command does not take input, so it is not allowed when data is
 * in the entry buffer.  An edit is an operation that acts on the data in the
 * entry buffer (e.g. backspace). *)
type function_operation = | Add | Sub | Mult | Div | Neg | Inv
                          | Pow | Sqrt | Abs | Arg | Exp | Ln 
                          | Conj ;;
type command_operation  = | Drop | Clear | Swap | Dup | Undo
                          | BeginBrowse | BeginExtended | Quit;;
type edit_operation     = | Digit | Enter | Backspace | Minus | SciNotBase 
                          | BeginInteger | BeginComplex | BeginMatrix
                          | Separator;;
type browse_operation   = | EndBrowse
                          | ScrollLeft | ScrollRight
                          | PrevLine | NextLine | Echo;;
type extended_operation = | ExitExtended | EnterExtended | ExtBackspace;;

type operation = | Function of function_operation 
                 | Command of command_operation
                 | Edit of edit_operation
                 | Browse of browse_operation
                 | Extend of extended_operation;;




(* arch-tag: DO_NOT_CHANGE_e761ca10-6bfd-4edf-a3de-53778a07ca21 *)
