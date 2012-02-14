(*
  ptrtype: do finer typechecks on C pointers
  Copyright (C) 2007-2011 Charles Hymans, Sarah Zennou
  Copyright (C) 2011-2010 Etienne Millon
  
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

  Sarah Zennou
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: sarah(dot)zennou(at)eads(dot)net
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

(* The type of a program: file names, global variable declarations,
    function definitions and the size of pointers. *)

type 'ty t = {
  globals: 'ty globals;
  init: 'ty blk;
  fundecs: (Newspeak.fid, 'ty fundec) Hashtbl.t;
  ptr_sz: Newspeak.size_t;
  src_lang: Newspeak.src_lang;
  abi: Newspeak.abi_t;
}

and 'ty fundec = {
  args : (string * 'ty) list;
  rets : (string * 'ty) list;
  body : 'ty blk;
  position: Newspeak.location;
}

and 'ty globals = (string, 'ty) Hashtbl.t

and 'ty stmtkind =
    Set	     of ('ty lval * 'ty exp * Newspeak.scalar_t)
  | Copy     of ('ty lval * 'ty lval * Newspeak.size_t)
  | Guard    of 'ty exp
  | Decl     of (string * 'ty * 'ty blk)
  | Select   of ('ty blk * 'ty blk)
  | InfLoop  of 'ty blk
  | DoWith   of ('ty blk * Newspeak.lbl)
  | Goto     of Newspeak.lbl
(* TODO: maybe should use a record rather than a tuple? *)
(* arguments, function type, function expression, return left values *)
  | Call     of (('ty exp * 'ty) list * 'ty funexp * ('ty lval * 'ty) list  )
  | UserSpec of 'ty assertion

and 'ty specs = 'ty assertion list

and 'ty assertion = 'ty spec_token list

and 'ty spec_token =
  | SymbolToken of char
  | IdentToken  of string
  | LvalToken   of ('ty lval * 'ty)
  | CstToken    of Newspeak.cst

and 'ty stmt = 'ty stmtkind * Newspeak.location

and 'ty blk = 'ty stmt list

and 'ty lval =
    Local  of string
  | Global of string
  | Deref  of ('ty exp * Newspeak.size_t)
  | Shift  of ('ty lval * 'ty exp)

and 'ty exp = ('ty bexp * 'ty)

and 'ty bexp =
    Const     of Newspeak.cst
  | Lval      of ('ty lval * 'ty)
  | AddrOf    of 'ty lval
  | AddrOfFun of (Newspeak.fid * 'ty ftyp)
  | UnOp      of (Newspeak.unop * 'ty exp)
  | BinOp     of (Newspeak.binop * 'ty exp * 'ty exp)

(* TODO: try to remove ftyp?? maybe not, it comes in handy *)
and 'ty ftyp = 'ty list * 'ty list

and 'ty field = Newspeak.offset * 'ty

and 'ty funexp =
    FunId of Newspeak.fid
  | FunDeref of 'ty exp

(** {1 Display} *)

val dump : ('ty -> string) -> 'ty t -> unit

val string_of_lval : ('ty -> string) -> 'ty lval -> string

val string_of_exp : ('ty -> string) -> 'ty exp -> string
