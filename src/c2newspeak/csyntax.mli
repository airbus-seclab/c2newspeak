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

type t = ((global * location) list * assertion list)

and assertion = spec_token list

and spec_token = 
    | SymbolToken of char
    | IdentToken of string
    | LvalToken of exp
    | CstToken of cst

and global =
    (* true if static *)
  | FunctionDef of (string * ftyp * bool * blk)
  | GlbVDecl of vardecl
  | GlbEDecl of enumdecl
(* struct or union: composite *)
  | GlbCDecl of compdecl

and enumdecl = string * exp

(* true for structure, false for union *)
and compdecl = string * bool * declaration list

and extern = bool

(* true for extern *)
and vardecl = string * typ * static * extern * init option

and declaration = (typ * string * location)

and ftyp = (typ * string) list option * typ

and typ =
    | Void
    | Int of ikind
    | Bitfield of (ikind * exp)
    | Float of int
    | Ptr of typ
    | Array of (typ * exp option)
    | Comp of (string * bool)
    | Fun of ftyp
    | Va_arg
    | Typeof of string

and init = 
    | Data of exp
    | Sequence of (string option * init) list

and stmt = (stmtkind * location)

and blk = stmt list

and stmtkind =
    EDecl of enumdecl
  | CDecl of compdecl
  | VDecl of vardecl
  | If of (exp * blk * blk)
      (* third parameter is the default case *)
  | CSwitch of (exp * (exp * blk * location) list * blk)
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
    | FunName
    | Cast of (exp * typ)
(* None is a regular assignment *)
    | Set of (exp * binop option * exp)
(* boolean is true if the operation is applied after the evaluation of the 
   expression *)
    | OpExp of (binop * exp * bool)
    | BlkExp of blk

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

val exp_of_char: char -> exp

val char_typ: typ

val int_typ: typ

val uint_typ: typ

val long_typ: typ

val int_cst_of_lexeme: 
  (string option * string * char option * string option) -> cst

val char_cst_of_lexeme: int -> cst

val float_cst_of_lexeme: (string * char option) -> cst

val comp_of_typ: typ -> string

val string_of_exp: exp -> string

val string_of_typ: typ -> string

val string_of_ftyp: ftyp -> string

val string_of_blk: blk -> string

val ftyp_of_typ: typ -> ftyp

val min_ftyp: ftyp -> ftyp -> ftyp

val print: t -> unit

val array_of_typ: typ -> (typ * exp option)

(** [size_of prog] counts the number of global definitions and the number
    of instructions in program prog. *)
val size_of: t -> int
