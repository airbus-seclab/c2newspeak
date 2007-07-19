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


open Cilutils
open Newspeak

type t = (exp list * gdecl list * (fid, fundec) Hashtbl.t)

and gdecl = (string * typ * init_t)

and fundec = ftyp * blk option

and stmtkind =
    Set of (lval * exp * scalar_t)
  | Copy of (lval * lval * size_t)
  | Decl of (string * typ * blk)
  | Label of lbl
  | Goto of lbl
  | Call of fn
  | ChooseAssert of (exp list * blk) list
  | InfLoop of blk

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
  | AddrOf of (lval * tmp_int)
  | AddrOfFun of fid
  | UnOp of (unop * exp)
  | BinOp of (binop * exp * exp)

and fn =
    FunId of fid
  | FunDeref of (exp * ftyp)

and init_t = (size_t * scalar_t * exp) list option

and unop =
      Belongs_tmp of (Int64.t * tmp_int)
    | Coerce of (Int64.t * Int64.t)
    | Not
    | BNot of (Int64.t * Int64.t)
    | PtrToInt of ikind
    | IntToPtr of ikind
    | Cast of (scalar_t * scalar_t)

and typ = 
    Scalar of scalar_t
  | Array of (typ * tmp_size_t)
  | Region of (field list * size_t)

and ftyp = typ list * typ option

and field = offset * typ

and tmp_int =
      Known of int
    | Length of string
    | SizeOf of string

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

(* TODO: extern storage not well handled !!! 
   By default, we accept extern as if they were declared but not defined 
*)
type glb_type = {
  gloc  : Cil.location;
  mutable gtype : typ;
(* None is for extern *)
  mutable ginit : init_t option;
  mutable gused : bool;
}

type fspec_type = {
  ploc  : Newspeak.location;
  prett : typ option;
  mutable pargs : ((int * string * typ) list) option;
  mutable plocs : ((int * string * typ) list) option;
  mutable pbody : blk option;
}

type intermediate = {
  ifilename : string;
  iglobs : (string, glb_type) Hashtbl.t;
  iusedcstr : String_set.t;
}

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

val dump_npko : (intermediate * (Newspeak.fid, fspec_type) Hashtbl.t) -> unit

val string_of_typ : typ -> string

(* TODO: remove this function*)
val compare_typs : typ -> typ -> bool

exception Uncomparable

(* More precise type 
   TODO: change name, not well chosen *)
val is_mp_typ : typ -> typ -> bool
