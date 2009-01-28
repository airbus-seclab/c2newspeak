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



(** Newspeak is a language designed for the purpose of static analysis. 
    It was designed with these features in mind:
    - precise: its semantics is precisely defined, 
    (see {e Newspeak, Doubleplussimple Minilang for Goodthinkful Static 
    Analysis of C} for a thorough and formal description. Available from
    {{:http://www.penjili.org}www.penjili.org})
    - simple: its primitives are as few, as standard and as concise as 
    possible,
    - minimal: no language primitive or fragment of primitive should be 
    expressible as a combination of other primitives,
    - explicit: primitives are context-free, i.e. all semantic information
    needed to execute it are readily available,
    - analysis-friendly: annotations useless for execution are added to allow
    a static analysis tool to perform correctness checks,
    - architecture-independent: all architecture or compiler dependent features
    (size of types, offsets of structure fields, order of executions, ...)
    are made explicit byt the translation to Newspeak,
    - expressive: it should be possible to translate most C programs into 
    Newspeak.

    Newspeak can be seen as a kind of high-level assembly language with 
    annotations for analysis.
    The type of Newspeak programs, types, statements and expressions are 
    described in this module.
    Additionnally, some functions to create, manipulate, export and display 
    Newspeak programs are provided. 
*)

(** {2 Types} *)

module Nat: sig 
  type t = string
  val zero: t
  val one: t
  val of_string: string -> t
  val to_string: t -> string
  val of_int: int -> t

  (** [to_int x] returns the integer representation of [x], when possible.
      @raise Invalid_argument "Newspeak.Nat.to_int" otherwise. *)
  val to_int: t -> int

  val of_big_int: Big_int.big_int -> t
  val to_big_int: t -> Big_int.big_int

  val add: t -> t -> t
  val mul: t -> t -> t
  val sub: t -> t -> t
  val div: t -> t -> t

  val neg: t -> t

  val add_int: int -> t -> t
  val mul_int: int -> t -> t

  (** [shift_left x n] multiplies [x] by 2 to the power [n]. *)
  val shift_left: t -> int -> t

  val compare: t -> t -> int
end

(* The type of a program: file names, global variable declarations,
    function definitions and the size of pointers. *)
type t = {
  fnames: file list;
  globals: globals;
  fundecs: (fid, fundec) Hashtbl.t;
  specs: specs;
  ptr_sz: size_t;
  mem_zones: mem_zones;
}

and globals = (string, gdecl) Hashtbl.t

and gdecl = typ * init_t

and fundec = ftyp * blk

and specs = assertion list

and mem_zones = (Nat.t * size_t) list

and assertion = spec_token list

and spec_token =
    | SymbolToken of char
    | IdentToken of string
    | LvalToken of lval
    | CstToken of cst

(* The exp list of ChooseAssert is a list of booleans. The block is applied if and only if each boolean is true (each boolean must be evaluated)*)
and stmtkind =
    Set of (lval * exp * scalar_t)
  | Copy of (lval * lval * size_t)
  | Guard of exp
  | Decl of (string * typ * blk)
  | Select of (blk * blk)
  | InfLoop of blk
  | DoWith of (blk * lbl * blk)
  | Goto of lbl
  | Call of fn
  | UserSpec of assertion

and stmt = stmtkind * location

and blk = stmt list

and lval =
    Local of vid
  | Global of string
  | Deref of (exp * size_t)
  | Shift of (lval * exp)

and exp =
    Const of cst
  | Lval of (lval * scalar_t)
  | AddrOf of (lval * size_t)
  | AddrOfFun of (fid * ftyp)
  | UnOp of (unop * exp)
  | BinOp of (binop * exp * exp)

and cst = 
    CInt of Nat.t
  (* TODO: warning floats with more than 64 bits can not be represented *)
  | CFloat of (float * string)
  | Nil

and unop =
    Belongs of bounds
  | Coerce of bounds
  | Not
  | BNot of bounds
  | PtrToInt of ikind
  | IntToPtr of ikind
  | Cast of (scalar_t * scalar_t)

and binop =
(* Integer operations *)
  | PlusI | MinusI | MultI | DivI | Mod
(* floating point operations *)
  | PlusF of size_t | MinusF of size_t | MultF of size_t | DivF of size_t
(* bitwise operations *)
  | BOr of bounds | BAnd of bounds | BXor of bounds
  | Shiftlt | Shiftrt
(* pointer operations *)
  | PlusPI | MinusPP
(* comparisons *)
  | Gt of scalar_t | Eq of scalar_t

and fn =
    FunId of fid
  | FunDeref of (exp * ftyp)

and typ =
    Scalar of scalar_t
  | Array of (typ * length)
  | Region of (field list * size_t)

and field = offset * typ

and scalar_t =
    Int of ikind
  | Float of size_t
  | Ptr
  | FunPtr

and init_t = 
  | Zero
  | Init of (offset * scalar_t * exp) list

and ftyp = typ list * typ option

and lbl = int
and vid = int
and fid = string
and file = string

and ikind = sign_t * size_t
and sign_t = Signed | Unsigned
and size_t = int
and offset = int
and length = int
and bounds = (Nat.t * Nat.t)

and location = string * int * int


(* {1 Constants} *)

val zero : exp
val one : exp
val zero_f : exp

val unknown_loc: location
val dummy_loc: string -> location


(* {1 Manipualtion and Simplifications} *)


(* Given the characteristics of an integer type, [domain_of_typ]
    returns the bounds of the type. *)
val domain_of_typ : sign_t * size_t -> bounds

val belongs: Nat.t -> bounds -> bool

(* TODO: change this to subset! *)
val contains: bounds -> bounds -> bool

(* Negation of a boolean condition. *)
val negate : exp -> exp

(* [exp_of_int i] wraps i into a Newspeak expression. *)
val exp_of_int : int -> exp

(* Deletion of useless Gotos and Labels. *)
val simplify_gotos : blk -> blk

(* Normalization of loops *)
val normalize_loops : blk -> blk

(* Run all simplifications. *)
(* true to remove checks when possible *)
val simplify : bool -> blk -> blk
(* true to remove checks when possible *)
val simplify_exp: bool -> exp -> exp



(** {1 Display} *)

(** @raise Invalid_argument "Newspeak.string_of_loc: unknown location" 
    if the file name is unknown
*)
val string_of_loc : location -> string

(** [string_of_bounds r] returns the string representation of range [r]. *)
val string_of_bounds : bounds -> string

(** [string_of_cst c] returns the string representation of constant [c]. *)
val string_of_cst : cst -> string
val string_of_sign_t: sign_t -> string
val string_of_scalar : scalar_t -> string
val string_of_typ : typ -> string
val string_of_ftyp : ftyp -> string
val string_of_exp : exp -> string
val string_of_fn: fn -> string
val string_of_lval : lval -> string

val string_of_stmt: stmt -> string

(** [string_of_block blk] returns the string representation of block [blk]. *)
val string_of_blk: blk -> string

val dump : t -> unit

(** [dump_globals glbdecls] prints the global definitions [glbdecls] to
    standard output. *)
val dump_globals: globals -> unit

val dump_fundec : string -> fundec -> unit

val string_of_binop: binop -> string


(* Visitor *)
class visitor:
object
  method process_gdecl: string -> gdecl -> bool
  method process_fun: fid -> fundec -> bool
  method process_fun_after: unit -> unit
  method process_stmt: stmt -> bool
  method process_fn: fn -> bool
  method process_exp: exp -> bool
(* called on expressions that are used as guards of choices *)
  method process_bexp: exp -> unit
  method process_lval: lval -> bool
  method process_unop: unop -> unit
  method process_binop: binop -> unit
  method process_size_t: size_t -> unit
  method process_length: length -> unit
  method process_typ: typ -> unit

  (* Sets current location *)
  method set_loc: location -> unit
  (* Gets current location *)
  method get_loc: unit -> location
  method print_warning: string -> unit
  (* Throws an Invalid_argument in a standard way, with the file and line
     number *)
  method raise_error: string -> unit
end

val visit_fun : visitor -> fid -> fundec -> unit
val visit_glb : visitor -> string -> gdecl -> unit
val visit : visitor -> t -> unit

class builder:
object
(* TODO: should have the same name as in the visitor!!! *)
  method set_curloc: location -> unit
  method curloc: location
  method process_global: string -> gdecl -> gdecl
  method process_lval: lval -> lval
  method process_exp: exp -> exp
  method process_blk: blk -> blk
  method enter_stmtkind: stmtkind -> unit
  method process_stmtkind: stmtkind -> stmtkind
  method process_size_t: size_t -> size_t
  method process_offset: offset -> offset
end

val build : builder -> t -> t

val build_gdecl: builder -> gdecl -> gdecl

(* [write name (files, prog, ptr_sz) ] write the program prog, with
    the list of its file names and the size of pointers to file name. *)
val write : string -> t -> unit

(** [read name] retrieves the list of file names, program and size of
    pointers from a .npk file.
    @param name file name of the .npk file to read
    @raise Invalid_argument if the input file is not a valid .npk file, or its
    newspeak version is not the same as this file's.
*)
val read : string -> t

(* [write_hdr cout (files, decls, ptr_sz] writes the list of file names,
    global variable declarations and size of pointer to channel cout.
    This is useful when incremental dump of Newspeak is needed because of
    memory constraints.
*)
val write_hdr : 
  out_channel -> 
  (string list * (string, gdecl) Hashtbl.t * specs * size_t * mem_zones) 
  -> unit

(* [write_hdr cout fid spec] writes the function fid with its specification
    spec to channel cout.
    This is useful when incremental dump of Newspeak is needed because of
    memory constraints. This function must be called after write_hdr in order
    to have a correctly formated Newspeak file.
*)
val write_fun : out_channel -> fid -> fundec -> unit

val size_of_scalar : size_t -> scalar_t -> size_t

(* 
    Type of the size_of function.
    [size_of t] returns the size of any value of type t.
*)
val size_of : size_t -> typ -> size_t

(** [bind_var x t blk] encloses the block [blk] with the declaration of 
    variable [x] of type [t] and then binds all occurances of [x] as a global
    variable in [blk].
*)
val bind_var: string -> typ -> blk -> stmtkind

(** [build_call f ft args] builds the call to function [f] of type [ft] with
    arguments [args].
    @raise Invalid_argument "Newspeak.build_call: non scalar argument" if
    some argument is not of scalar type
*)
val build_call: fid -> ftyp -> exp list -> blk

(** [build_main_call ptr_sz ft params] returns a block
   of newspeak code to call the main function with parameters [params].
   @param ptr_sz the size of pointers
   @param the type of main
   @param the parameters with which [main] is called
*)
val build_main_call: size_t -> ftyp -> string list -> blk

val create_cstr: string -> string -> string * gdecl

val max_ikind: ikind -> ikind -> ikind

(** returns the list of all function identifiers that are stored as function
    pointers in the program. *)
val collect_fid_addrof: t -> fid list

val equal_blk: blk -> blk -> bool
