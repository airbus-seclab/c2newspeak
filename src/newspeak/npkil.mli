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

  Olivier Levillain
  email: olivier.levillain@penjili.org
*)

open Newspeak

(* TODO: extern storage not well handled !!! 
   By default, we accept extern as if they were declared but not defined 
*)
type t = (filenames * (string, ginfo) Hashtbl.t * (fid, funinfo) Hashtbl.t
	   * assertion list)

and filenames = string list

(* None is for extern *)
and ginfo = (typ * location * init_t option * used)

and used = bool

(* TODO: code cleanup, remove everything unecessary for link *)
and funinfo = (ftyp * blk)

and stmtkind =
    Set of (lval * exp * scalar_t)
  | Copy of (lval * lval * size_t)
  | Decl of (string * typ * blk)
  | Guard of exp list
  | Select of blk list
  | InfLoop of blk
  | DoWith of (blk * lbl * blk)
  | Goto of lbl
  | Call of fn

and stmt = stmtkind * location

and blk = stmt list

and lval =
    Local of vid
  | Global of string
  | Deref of (exp * size_t)
  | Shift of (lval * exp)

and exp =
    Const of cte
  | Lval of (lval * scalar_t)
  | AddrOf of (lval * tmp_nat)
  | AddrOfFun of (fid * ftyp)
  | UnOp of (unop * exp)
  | BinOp of (binop * exp * exp)

and fn =
    FunId of fid
  | FunDeref of (exp * ftyp)

and init_t = (size_t * scalar_t * exp) list option

and unop =
      Belongs_tmp of (Nat.t * tmp_nat)
    | Coerce of Newspeak.bounds
    | Not
    | BNot of Newspeak.bounds
    | PtrToInt of ikind
    | IntToPtr of ikind
    | Cast of (scalar_t * scalar_t)

and typ = 
    Scalar of scalar_t
  | Array of (typ * tmp_size_t)
  | Region of (field list * size_t)

and ftyp = typ list * typ option

and field = offset * typ

(* TODO: code cleanup: think about this! *)
and tmp_nat =
      Known of Nat.t
    | Length of string
    | Mult of (tmp_nat * int)

and tmp_size_t = int option

module String_set :
  sig
    type elt = string
    type t
    val empty : t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val union : t -> t -> t
    val iter : (elt -> unit) -> t -> unit
  end


val zero : exp
val zero_f : exp

(** [make_int_coerce t e] wraps e into a coerce expression using
    integer bounds of type t *)
val make_int_coerce : sign_t * size_t -> exp -> exp


(** [make_belongs len e] wraps e into a belongs (0, len - 1) *)
(*val make_belongs : int -> exp -> exp*)

(** [exp_of_int i] wraps i into a Newspeak expression *)
val exp_of_int : int -> exp

val negate : exp -> exp

val dump_npko : t -> unit

val string_of_unop: unop -> string

val string_of_typ : typ -> string

val string_of_tmp_size: tmp_size_t -> string

val string_of_lval: lval -> string

(* TODO: remove this function*)
val compare_typs : typ -> typ -> bool

exception Uncomparable

(* More precise type 
   TODO: change name, not well chosen *)
val is_mp_typ : typ -> typ -> bool

val write: string -> t -> unit

val read_header: string -> (filenames* (string, ginfo) Hashtbl.t * specs)

val read_fundefs: string -> (fid, funinfo) Hashtbl.t

val create_cstr: string -> (string * ginfo)

val string_of_cast: Newspeak.scalar_t -> Newspeak.scalar_t -> string

val cast: Newspeak.scalar_t -> exp -> Newspeak.scalar_t -> exp

(* Generates a Newspeak statement by wrapping a block body with
   declarations decls. The order of the declaration must be carefully
   checked because in Newspeak, the variables are identified by their
   positions in the declaration stacks, not by their names *)
val append_decls: (string * typ * location) list -> blk -> blk
