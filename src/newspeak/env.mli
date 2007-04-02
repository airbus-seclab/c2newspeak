(** In the Env module are grouped all functions concerning the
    declaration of globals, locals and functions. It also contains the
    status functions to keep track of current interesting labels *)



(** {1 Types} *)

(** The status type allows the translate functions to keep track of
    the current return and switch infos *)
type status = {
  return_var : Newspeak.vid;
  return_lbl : Newspeak.lbl;
  switch_lbls: (Cil.location * Newspeak.lbl) list;
  brk_lbl    : Newspeak.lbl;
}

type glb_type = {
  mutable gtype : Cil.typ;
  mutable gloc  : Newspeak.location;
  mutable gdefd : bool;
  mutable ginit : Cil.init option;
}

type fspec_type = {
  mutable prett : Newspeak.typ option;
  mutable pargs : ((int * string * Newspeak.typ) list) option;
  mutable plocs : ((int * string * Newspeak.typ) list) option;
  mutable ploc  : Newspeak.location;
  mutable pbody : Newspeak.blk option;
  mutable pcil_body : Cil.block option
}



(** {1 Compilation variables} *)

val glb_decls : (string, glb_type) Hashtbl.t
val fun_specs : (Newspeak.fid, fspec_type) Hashtbl.t
val glb_used : Npkutils.String_set.t ref
val fun_called : Npkutils.String_set.t ref
val glb_cstr : Npkutils.String_set.t ref

val init_env : unit -> unit


(** {1 Globals} *)

(* [glb_uniquename v] returns the standardized name of global
    variable v: its name if it is a shared variable, its name prefixed
    by the filename if it is a static variable. *)

val update_glob_decl : Cil.varinfo -> unit
val update_glob_def : Cil.varinfo -> Cil.init option -> unit


(*
(** The two following functions are called during the first pass *)

(** [glb_make_cstr s] creates a global variable containing s *)
val glb_make_cstr : string -> unit

(** [get_glb_decls_inits translate_exp] retrives the list of the
    global variables and translates the initializers *)
val get_glb_decls_inits : (Cil.exp -> Newspeak.exp) -> Newspeak.decl list
*)





(** {1 Locals} *)

(** {2 Functions used in translate_fun} *)

(** [loc_declare v] adds the declaration of v in the local list *)
val loc_declare : bool -> (int * string * Newspeak.typ) -> unit

(** [get_loc_decls ()] returns the current list of local declaration,
    and reset the local handler (counter, hashtable and decl list) *)
val get_loc_decls : unit -> (Newspeak.ldecl * Newspeak.location) list


(** {2 Functions used in translate_call} *)

(** This function increases the counter for local functions *)
val push_local : unit -> unit

(** The two following functions allow translate_call to save and
    restore the declaration counter. Note that only one value of the
    counter can be saved at a time. *)

val save_loc_cnt : unit -> unit

val restore_loc_cnt : unit -> unit



(** {1 Functions} *)

val use_fun : Cil.varinfo -> unit

val extract_ldecl : (int * string * Newspeak.typ) -> Newspeak.ldecl

(** allows to update the specification of a function (prototype) when
    called for example *)

(** declaration of a function from a prototype *)
val update_fun_proto : string -> Cil.typ -> (string * Cil.typ * Cil.attributes) list option -> unit

(** declaration of a function from a definition *)
val update_fun_def : Cil.fundec -> unit


(** {1 Variable id retrieval} *)

(** [register_var v] stores the information that v has been seen *)
val register_var : Cil.varinfo -> unit

(** [register_cstr s] stores the information that s has been seen *)
val register_cstr : string -> unit

(** returns a Newspeak left value corresponding to a global or a local
    variable *)
val get_var : Cil.varinfo -> Newspeak.lval

(** [get_cstr] returns the expression giving access to the
    constant string corresponding to s *)
val get_cstr : string -> Newspeak.exp

(** returns the Newspeak left value corresponding to the current return
    value *)
val get_ret_var : status -> Newspeak.lval




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

(** add a new switch label in the status *)
val add_switch_label : status -> Cil.location -> Newspeak.lbl -> status

(** retrieves a switch label from the status *)
val retrieve_switch_label : status -> Cil.location -> Newspeak.lbl

(** tells whether a switch label is already in the status *)
val mem_switch_label : status -> Cil.location -> bool


(** {1 Linking time} *)

val update_glob_link : string -> glb_type -> unit
val update_fun_link : string -> fspec_type -> unit

val handle_real_glob : (Cil.exp -> Newspeak.exp) -> Npkutils.String_set.t -> string -> glb_type -> unit
val handle_cstr : string -> unit
val get_glob_decls : unit -> Newspeak.gdecl list
(*val get_glob_vid : string -> Newspeak.vid
val get_glob_typ : string -> Newspeak.typ *)

val handle_funspec : Npkutils.String_set.t -> string -> fspec_type -> unit
val get_funspecs : unit -> (Newspeak.fid, Newspeak.fundec) Hashtbl.t

