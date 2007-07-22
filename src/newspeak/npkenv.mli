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


(** In the Npkenv module are grouped all functions concerning the
    declaration of globals, locals and functions. It also contains the
    status functions to keep track of current interesting labels *)

open Npkil


(** {1 Types} *)

(** The status type allows the translate functions to keep track of
    the current return and switch infos *)
type status = {
  return_lbl : Newspeak.lbl;
  switch_lbls: (Cil.location * Newspeak.lbl) list;
  brk_lbl    : Newspeak.lbl;
}

(** {1 Compilation variables} *)

val glb_decls : (string, Npkil.glb_type) Hashtbl.t
val fun_specs : (Newspeak.fid, Npkil.fspec_type) Hashtbl.t
val glb_cstr : String_set.t ref

val glb_uniquename : Cil.varinfo -> string

val init_env : unit -> unit

val create_npkil : 
  string -> (Npkil.intermediate * (string, Npkil.fspec_type) Hashtbl.t)

(** {1 Locals} *)

(** {2 Functions used in translate_fun} *)

(* returns the arguments of the function,
   None when unknown 
*)
val get_args : string -> ((int * string * typ) list) option

(** [loc_declare v] adds the declaration of v in the local list *)
val loc_declare : bool -> (int * string * Npkil.typ) -> unit

(** [get_loc_decls ()] returns the current list of local declaration,
    and reset the local handler (counter, hashtable and decl list) *)
val get_loc_decls : unit -> (string * Npkil.typ * Newspeak.location) list


(** {2 Functions used in translate_call} *)

(** This function increases the counter for local functions *)
val push_local : unit -> unit

(** The two following functions allow translate_call to save and
    restore the declaration counter. Note that only one value of the
    counter can be saved at a time. *)

val save_loc_cnt : unit -> unit

val restore_loc_cnt : unit -> unit



(** {1 Functions} *)

(* TODO: remove this function ??? strange *)
val extract_ldecl : (int * string * Npkil.typ) -> (string * Npkil.typ)

(** allows to update the specification of a function (prototype) when
    called for example *)

val translate_formals : 
  string -> (string * Cil.typ * Cil.attributes) list option 
  -> (int * string * Npkil.typ) list option

(** declaration of a function from a prototype *)
val update_fun_proto : 
  string -> Npkil.typ option -> (int * string * Npkil.typ) list option -> unit

(** {1 Variable id retrieval} *)

(** returns a Newspeak left value corresponding to a global or a local
    variable *)
val get_var : Cil.varinfo -> Npkil.lval

(** [get_cstr] returns the expression giving access to the
    constant string corresponding to s *)
val get_cstr : string -> Npkil.exp

(** returns the Newspeak left value corresponding to the current return
    value *)
val get_ret_var : status -> Npkil.lval




(** {1 Status "constructors" and label handling} *)

(** creates an empty status *)
val empty_status : unit -> status

(** creates a fresh local variable and creates a status with this
    variable as return value *)
val new_ret_status : unit -> status

(** modify a status with a fresh label break *)
val new_brk_status : status -> status

(** generates a fresh label *)
val new_label : unit -> Newspeak.lbl

(** resets label generator to first label *)
val reset_lbl_gen : unit -> unit

(** add a new switch label in the status *)
val add_switch_label : status -> Cil.location -> Newspeak.lbl -> status

(** retrieves a switch label from the status *)
val retrieve_switch_label : status -> Cil.location -> Newspeak.lbl

(** tells whether a switch label is already in the status *)
val mem_switch_label : status -> Cil.location -> bool


val compare_formals : 
  string -> (int * string * Npkil.typ) list 
  -> (int * string * Npkil.typ) list -> unit
