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


(** NpkFirstPass contains every useful function run before the actual
    translation. Before translating a file, Npkfirstpass.translate
    makes the following: 
      - deleting CIL's gotos
      - simplifying some exps (StartOf, pointer equality)
      - collecting const string
      - remove unused local vars *)

val code_to_duplicate : (Cil.label, Cil.stmt list) Hashtbl.t

(* TODO: no need to be mutable *)
type glb_type = {
  mutable gtype : Cil.typ;
  mutable gloc : Cil.location;
  mutable gdefd : bool;
  mutable ginit : Cil.init option;
}
(* TODO: remove *)
type fspec_type = {
  mutable prett : Npkil.typ option;
  mutable pargs : ((int * string * Npkil.typ) list) option;
  mutable plocs : ((int * string * Npkil.typ) list) option;
  mutable ploc  : Newspeak.location;
  mutable pbody : Cil.block option;
}

(** TODO: document and clean this up *)
val first_pass : 
  Cil.file -> 
  (Npkil.String_set.t * Npkil.String_set.t 
    * (Newspeak.fid, 
      (int * string * Npkil.typ) list 
      * (int * string * Npkil.typ) list * Cil.block) Hashtbl.t
    * (string, glb_type) Hashtbl.t)
