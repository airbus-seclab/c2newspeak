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
  | Decl     of string * blk
  | Select   of blk * blk    (* XXX *)
  | InfLoop  of blk
  | DoWith   of blk * lbl * blk
  | Goto     of lbl
  | UserSpec of Newspeak.assertion

and var = string

and exp =
  | Const of cst
  | Var   of var
  | Op    of binop * exp * exp

and lbl = int

and cst = Newspeak.Nat.t

and binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Gt
  | Eq
