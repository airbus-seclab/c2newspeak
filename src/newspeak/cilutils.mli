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


(** Cilutils regroups useful functions that could have been provided
    by Cil *)


(** {1 C types } *)

(* TODO: Handle architecture dependent stuff *)

val size_of : Cil.typ -> int

(** [sizeof_sub_type t] returns the size of the type pointed by t and
    raises an exception if t is not a pointer *)
val size_of_subtyp : Cil.typ -> int

val offset_of : Cil.typ -> Cil.offset -> int

(** Thanks to size_of, we store in the following variables the sizes
    in bytes of the types used. As these sizes are architecture
    dependant, they are computed by Cil when Cilutils is loaded *)

val char_size : int
val short_size : int
val int_size : int
val long_size : int
val pointer_size : int
val float_size : int
val double_size : int
val long_double_size : int



(** {1 Display functions } *)

(** setCilPrinter allows to choose between the "plain" CilPrinter and
    the "default" CilPrinter for display functions *)
val setCilPrinter : string -> unit

val string_of_type : Cil.typ -> string
val string_of_global : Cil.global -> string
val string_of_init : Cil.init -> string
val string_of_exp : Cil.exp -> string
val string_of_lval : Cil.lval -> string
val string_of_instr : Cil.instr -> string
val string_of_attribute : Cil.attribute -> string

val string_of_cast : (Cil.typ * Cil.typ) -> Cil.exp -> string

val dump : Pervasives.out_channel -> Cil.file -> unit



(** {1 Miscellaneous } *)

val stmt_of_stmtkind : Cil.stmtkind -> Cil.stmt

val null : Cil.exp
