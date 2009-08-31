(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans
  
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

(** Simple language.
    All variables have type int.
*)

type t = {
  fnames: Newspeak.file list;           (** list of source file names *)
  globals: globals;                     (** program variables *)
  init: blk;                            (** initialization block of globals *)
  fundecs: (Newspeak.fid, fundec) Hashtbl.t;  
                                        (** table of all declared functions *)
  src_lang: Newspeak.src_lang;          (** source programming language *)
}

and globals = (string, gdecl) Hashtbl.t (** Table of global names to location *)

and gdecl = Newspeak.location

and fundec = blk

and blk = stmt list

and stmt = stmtkind * Newspeak.location

and stmtkind =
    Set of (lval * exp)                 (** assignment *)
  | If                                  (** if then else *)
  | While                               (** while loop *)
  | Call                                (** function call *)
  | Assert                              (** assertion *)

and lval = Global of string             (** global variable *)

and exp =
    Const of cst                        (** integer constant *)
  | Lval of lval                        (** left value *)
  | UnOp of (unop * exp)                (** unary operation *)
  | BinOp of (binop * exp * exp)        (** binary operation *)

and cst = CInt of Int32.t

and unop =
    Belongs of bounds                   (** check within bounds: blocking *)
  | Coerce of bounds                    (** check within bounds: non-blocking *)

and binop = 
    PlusI                               (** addition *)
  | MinusI                              (** substraction *)
  | MultI                               (** multiplication *)
  | DivI                                (** division *)
  | Mod                                 (** modulo *)
  | Gt                                  (** strictly greater than *)
  | Eq                                  (** equality *)

and bounds = Int32.t * Int32.t
