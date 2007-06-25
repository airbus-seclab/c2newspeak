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


(** The Newspeak module describes the internal language used to analyse
    C code. It is a kind of high-level assembly language.

    The names used in Newspeak are similar to those used in CIL.

    This module also exports some useful functions to manipulate and
    display Newspeak programs. *)


(** {1 Types} *)

(** The type of a program: function definitions and an block
    containing initialisation of the global variables. *)
type t = (gdecl list * (fid, fundec) Hashtbl.t)

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
(** The exp list of ChooseAssert is a list of booleans. The block is applied if and only if each boolean is true (each boolean must be evaluated)*)

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
  | AddrOf of (lval * size_t)
  | AddrOfFun of fid
  | UnOp of (unop * exp)
  | BinOp of (binop * exp * exp)

and cte = 
    CInt64 of Int64.t
  (* TODO: warning floats with more than 64 bits can not be represented *)
  | CFloat of float * string
  | Nil

and unop =
    Belongs of (Int64.t * Int64.t)
  | Coerce of (Int64.t * Int64.t)
  | Not
  | BNot of (Int64.t * Int64.t)
  | PtrToInt of ikind
  | IntToPtr of ikind
  | Cast of (scalar_t * scalar_t)

and binop =
  | PlusI | MinusI | MultI | DivI | Mod
  | PlusF of size_t | MinusF of size_t | MultF of size_t | DivF of size_t
  | BOr of (Int64.t * Int64.t) | BAnd of (Int64.t * Int64.t)
  | BXor of (Int64.t * Int64.t)
  | Shiftlt
  | Shiftrt
  | PlusPI
  | MinusPP
  | Gt of scalar_t
  | Eq of scalar_t

and fn =
    FunId of fid
  | FunDeref of (exp * ftyp)

and typ =
    Scalar of scalar_t
  | Array of (typ * size_t)
  | Region of (field list * size_t)

and field = offset * typ

and scalar_t =
    Int of ikind
  | Float of size_t
  | Ptr
  | FunPtr

and init_t = 
  | Zero
  | Init of (size_t * scalar_t * exp) list

and ftyp = typ list * typ option

and lbl = int
and vid = int
and fid = string

and ikind = sign_t * size_t
and sign_t = Unsigned | Signed
and size_t = int
and offset = int

and location = string * int * int



(** {1 Constants} *)

val zero : exp
val zero_f : exp

val locUnknown : location



(** {1 Manipualtion and Simplifications} *)


(** Given the characteristics of an integer type, [domain_of_typ]
    returns the bounds of the type. *)
val domain_of_typ : sign_t * size_t -> Int64.t * Int64.t

(** Negation of a boolean condition. *)
val negate : exp -> exp

(** [exp_of_int i] wraps i into a Newspeak expression. *)
val exp_of_int : int -> exp

(** Deletion of useless Gotos and Labels. *)
val simplify_gotos : blk -> blk

(** Run all simplifications. *)
val simplify : blk -> blk



(** {1 Display } *)

val string_of_typ : typ -> string
val string_of_ftyp : ftyp -> string
val string_of_exp : exp -> string
val string_of_lval : lval -> string

val string_of_stmt: stmt -> string
val string_of_blk: blk -> string

(** [dump (fundecs, body)] prints the program (fundecs, body) 
    to standard output. *)
val dump : t -> unit

val dump_fundec : string -> fundec -> unit

val dump_as_C : t -> unit

(** [write name (files, prog, ptr_sz) ] write the program prog, with
    the list of its file names and the size of pointers to file name. *)
val write : string -> (string list * t * size_t) -> unit

(** [read name] retrieves the list of file names, program and size of
    pointers from file name. *)
val read : string -> (string list * t * size_t)

(** [write_hdr cout (files, decls, ptr_sz] writes the list of file names,
    global variable declarations and size of pointer to channel cout.
    This is useful when incremental dump of Newspeak is needed because of
    memory constraints.
*)
val write_hdr : out_channel -> (string list * gdecl list * size_t) -> unit

(** [write_hdr cout fid spec] writes the function fid with its specification
    spec to channel cout.
    This is useful when incremental dump of Newspeak is needed because of
    memory constraints. This function must be called after write_hdr in order
    to have a correctly formated Newspeak file.
*)
val write_fun : out_channel -> fid -> fundec -> unit

(** 
    Type of the size_of function.
    [size_of t] returns the size of any value of type t.
*)
type size_of = typ -> size_t

(** 
    Type of the size_of_scalar function.
    [size_of_scalar sc_t] returns the size of any value of scalar type sc_t.
*)
type size_of_scalar = scalar_t -> size_t

(**
   [create_size_of ptr_size] creates functions size_of_scalar and size_of from
   a given size of pointers ptr_size. On most standard machines ptr_size is 4.
*)
val create_size_of : size_t -> (size_of_scalar * size_of)

val build_call: fid -> ftyp -> blk

val build_main_call : size_t -> ftyp -> string list -> (gdecl list * blk)

val create_cstr: string -> string -> gdecl

(* Tries to extract a while construct out of a block
   returns the condition, the body, location and remaining of the block
   if it succeeds *)
val extract_while: blk -> (exp list * blk * location * blk) option 
(** [extract_while InfLoop(blk1)::(Label(l)::blk2 ) ] try to find a while loop. 
If it fails, then it returns None.
Else, it returns the while condition in a exp list. It is a list of booleans which 
are evaluated until some of them is false (further booleans are not evaluated).
It also returns two blk, the blk in the loop and the blk after the loop.   *)

type alt_stmtkind =
    (* the condition is a list of expression separated by && 
       Careful! It behaves like the C operator: 
       if the first expression evaluates to false, evaluation stops. *)
    | While of (exp list * blk)
    | Npk of stmtkind

type alt_blk = (alt_stmtkind * location) list

(* Tries to convert all infinite loops in the blk to while loops *)
val convert_loops: blk -> alt_blk
