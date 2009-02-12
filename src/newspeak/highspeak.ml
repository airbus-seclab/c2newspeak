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

open Newspeak

module N = Newspeak

type t = {
  fnames: file list;
  globals: globals;
  fundecs: (fid, fundec) Hashtbl.t;
  specs: specs;
  ptr_sz: size_t;
  mem_zones: mem_zones
}

and globals = (string, gdecl) Hashtbl.t

and gdecl = typ * init_t * location

and init_t = 
    Zero
  | Init of (size_t * scalar_t * exp) list

and fundec = (vid list * vid list * ftyp * blk)

and stmtkind =
    Set of (lval * exp * typ)
  | Guard of exp
(* TODO: maybe simplify, use only variable name, no need for vid
   simplifies also the left value, put Local and Global into just Var *)
  | Decl of (string * typ * vid * blk)
  | Select of (blk * blk)
  | InfLoop of blk
  | DoWith of (blk * lbl * blk)
  | Goto of lbl
(* TODO: in case of a funderef, the ftyp is redundant!! *)
(* The ftyp should be in the list of args and the list of lval!! *)
  | Call of (exp list * ftyp * fn * lval list)
  | UserSpec of assertion

and specs = assertion list

and assertion = spec_token list

and spec_token =
  | SymbolToken of char
  | IdentToken of string
  | LvalToken of lval
  | CstToken of cst

and stmt = stmtkind * location

and blk = stmt list

and lval =
(* TODO: maybe should put local and global together by name??? *)
    Local of vid
  | Global of string
  | Deref of (exp * size_t)
  | Shift of (lval * exp)

and exp =
    Const of cst
  | Lval of (lval * typ)
  | AddrOf of (lval * size_t)
  | AddrOfFun of (fid * ftyp)
  | UnOp of (unop * exp)
  | BinOp of (binop * exp * exp)

and fn =
    FunId of fid
  | FunDeref of (exp * ftyp)

let string_of_stmtkind x =
  match x with
      Set _ -> "Set"
    | Guard _ -> "Guard"
    | Decl _ -> "Decl"
    | Select _ -> "Select"
    | InfLoop _ -> "InfLoop"
    | DoWith _ -> "DoWith"
    | Goto _ -> "Goto"
    | Call _ -> "Call"
    | UserSpec _ -> "UserSpec"
