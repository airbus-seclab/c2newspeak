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

type prog = (compdefs * (global * location) list)

(* true for structure/false for union *)
and compdefs = (string * bool * declaration list) list

and global =
    | FunctionDef of (string * typ * blk)
    | GlbEDecl of enumdecl
(* true for extern *)
    | GlbVDecl of (vardecl * extern)

and extern = bool

and vardecl = string * typ * static * init option

and enumdecl = string * exp

and declaration = (typ * string)

(* true if variable list of arguments *)
and ftyp = (typ * string) list * bool * typ

and typ =
    | Void
    | Int of ikind
    | Bitfield of (ikind * int)
    | Float of int
    | Ptr of typ
    | Array of (typ * exp option)
    | Struct of string
    | Union of string
    | Fun of ftyp

and init = 
    | Data of exp
    | Sequence of init list

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

and static = bool

and exp = 
    | Cst of Cir.cst
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
(* returns the value and then increment it or decrement it *)
    | ExpIncr of (binop * exp)

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
