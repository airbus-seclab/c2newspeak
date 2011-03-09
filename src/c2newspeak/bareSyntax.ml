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

type base_typ =
    | Void 
    | Integer of Newspeak.ikind
    | Float of int
    | Composite of (bool * (string * field list option))
    | Name of string
    | Enum of ((string * Csyntax.exp option) list) option
    | Va_arg
    | Typeof of string

and var_modifier = (int * modifier)

and modifier = 
    | Abstract
    | Variable of (string * Newspeak.location)
    | Function of (var_modifier * decl list)
    | Array of (var_modifier * Csyntax.exp option)

and decl = (base_typ * var_modifier)

and field = (base_typ * var_modifier * Csyntax.exp option)

type t = (global * Newspeak.location) list

(* TODO: cleanup, simplify types *)
and global = 
    FunctionDef of (bool * ((base_typ * var_modifier) * blk))
  | GlbDecl of ((bool * bool) * (base_typ * ((var_modifier * Newspeak.size_t list) * Csyntax.init option) list))
  | GlbTypedef of (base_typ * ((var_modifier * Newspeak.size_t list) * Csyntax.init option) list)
  | GlbUserSpec of Csyntax.assertion

and blk = stmt list

and stmt = (Csyntax.stmtkind * Newspeak.location)
