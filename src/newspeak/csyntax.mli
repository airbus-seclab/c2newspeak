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

type prog = (compdefs * glbdecls * fundefs)

and compdefs = (string, (fields_t * int)) Hashtbl.t

(* first None is for extern, second None for no init *)
and glbdecls = (string, typ * location * init option option) Hashtbl.t

and fundefs = (string, ftyp * location * body option) Hashtbl.t

and body = (typ * string * location) list * blk

and init = (int * typ * exp) list

and ftyp = (typ * string) list * typ

and typ =
    | Void
    | Int of ikind
    | Float of int
    | Ptr of typ
    | Array of array_t
    | Struct of string
    | Union of string
    | Fun of ftyp

and fields_t = (string * (int * typ)) list

and array_t = (typ * int option)

and blk = stmt list

and stmt = (stmtkind * location)

and stmtkind =
    | Init of (int * init)
    | Set of (lv * typ * exp)
    | If of (exp * blk * blk)
    | Switch of (exp * (typ_exp option * blk * location) list)
    | Loop of (blk * blk)
    | Return
    | Call of (lv option * (lv * ftyp) * exp list)
    | Break
    | Continue

and typ_exp = (exp * typ)

and typ_lv = (lv * typ)

and lv = 
    | Local of int
    | Global of string
    | Field of (lv * string * int)
    | Index of (lv * array_t * exp)
    | Deref of (exp * typ)

and exp = 
    | Const of cst
    | Lval of typ_lv
    | AddrOf of typ_lv
    | Unop of (unop * exp)
    | Binop of (binop * exp * exp)

and unop = 
    | Not
    | BNot of ikind
    | Cast of (typ * typ)

and binop =
    | Plus of ikind
    | Minus of ikind
    | Div of ikind
    | Mult of ikind
    | BAnd of ikind
    | BXor of ikind
    | BOr of ikind
    | Mod
    | PlusP of typ
    | MinusP
    | Gt of typ
    | Eq of typ
    | Shiftl of ikind
    | Shiftr of ikind
    | PlusF of int
    | MinusF of int
    | DivF of int
    | MultF of int

and cst = 
    | CInt of Int64.t
    | CFloat of string

val ftyp_of_typ: typ -> ftyp

val fields_of_typ: compdefs -> typ -> fields_t

val array_of_typ: typ -> array_t

val deref_typ: typ -> typ

val size_of: compdefs -> typ -> int

val int_kind: ikind

val int_typ: typ

val typ_of_cst: cst -> typ

val promote: ikind -> ikind

val exp_of_int: int -> exp

val exp_of_float: float -> exp

val ftyp_equals: ftyp -> ftyp -> bool

val undefined: string

val cast: (exp * typ) -> typ -> exp
