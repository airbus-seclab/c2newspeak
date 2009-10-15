(**
 * Stripped-down language :
 *   - only ints
 *   - only globals
 *   - no function calls
 *)

type t = blk

and blk = stmt list

and stmt = stmtkind * Newspeak.location

and stmtkind =
  | Set      of var * exp
  | Guard    of exp
  | Select   of blk * blk
  | InfLoop  of blk
  | DoWith   of blk * lbl * blk
  | Goto     of lbl

and var = string

and exp =
  | Const of cst
  | Var   of var
  | Not   of exp
  | Op    of binop * exp * exp

and lbl = int

and cst = int

and binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Gt
  | Eq
