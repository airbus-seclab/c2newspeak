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


type prog = (compdefs * glbdecls * fundefs)

and compdefs = (string, (field list * int * int)) Hashtbl.t

and glbdecls = (string, typ * Newspeak.location * init option) Hashtbl.t

and init = (int * typ * exp) list option

(** field's name, offset and typ *)
and field = (string * (int * typ))

and fundefs = (string, (ftyp * Newspeak.location * funbody option)) Hashtbl.t

and funbody = ((vid * vid list) * blk)

and vid = int

and typ =
    | Void
    | Int of Newspeak.ikind
    | Float of int
    | Ptr of typ
    | Array of array_t
    | Struct of string
    | Union of string
    | Fun of ftyp

and array_t = (typ * int option)

(* true if variable list of arguments *)
and ftyp = typ list * bool * typ

and blk = stmt list

and stmt = (stmtkind * Newspeak.location)

and stmtkind =
    | Block of (blk * lbl option)
    | Goto of lbl
    | Decl of (typ * string * int)
    | Set of (lv * typ * exp)
    | Loop of blk
    | If of (exp * blk * blk)
    | Switch of (exp * (typ_exp * blk) list * blk)
    | Exp of exp

and lbl = int

and typ_lv = (lv * typ)

and typ_exp = (exp * typ)

and lv =
(* variable identified by its unique id. Use fresh_id () to generate
   a new variable *)
    | Var of vid
    | Global of string
    | Shift of (lv * exp)
    | Deref of (exp * typ)
(* TODO: remove Post by using Pref instead and having some optimization get
   rid of unnecessary temporary variable??? If better *)
    | Post of (lv * stmt)

and exp =
    | Const of cst
    | Lval of typ_lv
    | AddrOf of typ_lv
    | Unop of (unop * exp)
    | Binop of (binop * exp * exp)
    | Call of (ftyp * funexp * exp list)
    | Pref of (blk * exp)

and funexp =
    | Fname of string
    | FunDeref of (exp * ftyp)

and unop = 
    | Belongs_tmp of (Int64.t * Npkil.tmp_int)
    | Not
    | BNot of Newspeak.ikind
    | Cast of (typ * typ)

and binop =
    | Plus of Newspeak.ikind
    | Minus of Newspeak.ikind
    | Div of Newspeak.ikind
    | Mult of Newspeak.ikind
    | BAnd of Newspeak.ikind
    | BXor of Newspeak.ikind
    | BOr of Newspeak.ikind
    | Mod
    | PlusP of typ
    | MinusP
    | Gt of typ
    | Eq of typ
    | Shiftl of Newspeak.ikind
    | Shiftr of Newspeak.ikind
    | PlusF of int
    | MinusF of int
    | DivF of int
    | MultF of int

and cst =
    | CInt of Int64.t
    | CFloat of string

(** kind of C int type *)
val int_kind: Newspeak.ikind

(** type of C char type *)
val char_typ: typ

(** type of C int type *)
val int_typ: typ

val typ_of_cst: cst -> typ

val promote: Newspeak.ikind -> Newspeak.ikind

val cast: typ_exp -> typ -> exp

val exp_of_int: int -> exp

val exp_of_float: float -> exp

val fresh_id: unit -> vid

(** [normalize_exp e] returns (pref, e, post), where e is an expression without
    side effects (no Pref or Post constructs), and pref and post are blocks
    to be executed respectively before and after e. *)
val normalize_exp: exp -> (blk * exp * blk)

val normalize_lv: lv -> (blk * lv * blk)

val normalize: blk -> blk

val align_of: compdefs -> typ -> int

(** [next_aligned o x] returns the smallest integer greater or equal than o,
    which is equal to 0 modulo x *)
val next_aligned: int -> int -> int

val size_of: compdefs -> typ -> int

val fields_of_typ: compdefs -> typ -> field list

val deref: typ_exp -> typ_lv

val funexp_of_lv: typ_lv -> (funexp * ftyp)

val len_of_exp: exp -> int

val len_of_array: int option -> lv -> Npkil.tmp_int
