(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain
  
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
open Newspeak

type prog = (global * location) list

and spec = spec_token list list

and spec_token = 
    | SymbolToken of char
    | IdentToken of string
    | CstToken of cst

and global =
(* true if static *)
    | FunctionDef of (string * typ * bool * blk)
    | GlbEDecl of enumdecl
(* true for extern *)
    | GlbVDecl of (vardecl * extern)

and extern = bool

and vardecl = string option * typ * static * init option

and enumdecl = string * exp

and declaration = (typ * string * location)

and ftyp = (typ * string) list * typ

and typ =
    | Void
    | Int of ikind
    | Bitfield of (ikind * exp)
    | Float of int
    | Ptr of typ
    | Array of (typ * exp option)
    | Struct of (string * declaration list option)
    | Union of (string * declaration list option)
    | Fun of ftyp

and init = 
    | Data of exp
    | Sequence of (string option * init) list

and stmt = (stmtkind * location)

and blk = stmt list

and stmtkind =
    | EDecl of enumdecl
    | VDecl of vardecl
    | If of (exp * blk * blk)
    | CSwitch of (exp * (exp * blk * location) list * blk)
    | For of (blk * exp * blk * blk)
    | Exp of exp
    | Break
    | Continue
    | Return of exp option
    | Block of blk
    | Goto of lbl
    | Label of lbl

and lbl = string

and static = bool

and exp = 
    | Cst of cst
    | Var of string
    | Field of (exp * string)
    | Index of (exp * exp)
    | Deref of exp
    | AddrOf of exp
    | Unop of (unop * exp)
    | IfExp of (exp * exp * exp)
    | Binop of (binop * exp * exp)
    | Call of (exp * exp list)
    | Sizeof of typ
    | SizeofE of exp
    | Str of string
    | Cast of (exp * typ)
    | Set of (exp * exp)
    | SetOp of (exp * binop * exp)
(* returns the value and then increment it or decrement it *)
    | ExpIncr of (binop * exp)
    | IncrExp of (binop * exp)

and cst = (Cir.cst * typ)

and unop = Neg | Not | BNot

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

val exp_of_int: int -> exp

val char_typ: typ

val int_typ: typ

val int_cst_of_lexeme: 
  (string option * string * char option * string option) -> cst

val char_cst_of_lexeme: int -> cst

val float_cst_of_lexeme: (string * char option) -> cst

val va_arg: (typ * string)

val comp_of_typ: typ -> string

val normalize_ftyp: ftyp -> (ftyp * string list)
