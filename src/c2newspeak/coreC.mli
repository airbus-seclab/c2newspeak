(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2009  Charles Hymans
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
*)

type t = ((global * Newspeak.location) list * assertion list)

and assertion = spec_token list

and spec_token = 
    | SymbolToken of char
    | IdentToken of string
    | CstToken of Cir.cst

and global =
    FunctionDef of (string * ftyp * is_static * blk)
  | GlbDecl of (string * decl)

and decl = 
    VDecl of (typ * is_static * is_extern * init option)
  | EDecl of exp
(* struct or union: composite *)
  | CDecl of (is_struct * field_decl list)

(* true for structure, false for union *)
and is_struct = bool

and is_extern = bool

and is_static = bool

and field_decl = (string * typ)

and ftyp = (typ * string) list option * typ

and typ =
    | Void
    | Int of Newspeak.ikind
    | Bitfield of (Newspeak.ikind * exp)
    | Float of int
    | Ptr of typ
    | Array of array_typ
    | Comp of (string * is_struct)
    | Fun of ftyp
    | Va_arg
    | Typeof of string

and array_typ = typ * exp option

and init = 
    | Data of exp
    | Sequence of (string option * init) list

and stmt = (stmtkind * Newspeak.location)

and blk = stmt list

and stmtkind =
  | LocalDecl of (string * decl)
  | If of (exp * blk * blk)
      (* third parameter is the default case *)
  | CSwitch of (exp * (exp * blk * Newspeak.location) list * blk)
  | For of (blk * exp * blk * blk)
  | DoWhile of (blk * exp)
  | Exp of exp
  | Break
  | Continue
  | Return of exp option
  | Block of blk
  | Goto of lbl
  | Label of lbl
  | UserSpec of assertion

and lbl = string

and exp = 
    | Cst of (Cir.cst * typ)
    | Var of string
    | Field of (exp * string)
    | Index of (exp * array_typ * exp)
    | Deref of exp
    | AddrOf of exp
    | Unop of (unop * exp)
    | IfExp of (exp * exp * exp)
    | Binop of ((binop * typ) * typ_exp * typ_exp)
    | Call of (exp * exp list)
    | Sizeof of typ
    | Offsetof of (typ * string)
    | Str of string
    | FunName
    | Cast of typ_exp
(* None is a regular assignment *)
    | Set of (exp * (binop * typ) option * exp)
(* boolean is true if the operation is applied after the evaluation of the 
   expression *)
    | OpExp of ((binop * typ) * exp * bool)
    | BlkExp of (blk * bool)

and typ_exp = (exp * typ)

and unop = Not | BNot of Newspeak.ikind

and binop =
    | Plus
    | Minus
    | Mult
    | Div
    | Mod
    | Gt
    | Eq
    | BAnd
    | BXor
    | BOr
    | Shiftl
    | Shiftr

val char_typ: typ
val uint_typ: typ
val int_typ: typ

val exp_of_char: char -> exp

val exp_of_int: int -> exp

val comp_of_typ: typ -> string

val ftyp_of_typ: typ -> ftyp

val deref_typ: typ -> typ

val min_ftyp: ftyp -> ftyp -> ftyp

val string_of_exp: exp -> string

val string_of_typ: typ -> string

val ftyp_of_typ: typ -> ftyp

val promote: Newspeak.ikind -> Newspeak.ikind
