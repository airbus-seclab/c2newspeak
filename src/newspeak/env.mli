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
  gname : string;
  gtype : Cil.typ;
  gloc  : Cil.location;
  gdefd : bool;
  ginit : Cil.init option;
}

type proto_type = {
  pname : string;
  prett : Newspeak.typ option;
  pargs : ((string * Newspeak.typ) list) option;
  ploc  : Cil.location;
}



(** {1 Compilation variables} *)

val glb_decls : (glb_type list) ref
val fun_defs : ((Cil.fundec * Cil.location) list) ref
val proto_decls : (proto_type list) ref
val glb_used : Npkutils.String_set.t ref
val fun_called : Npkutils.String_set.t ref
val glb_cstr : Npkutils.String_set.t ref



(** {1 Globals} *)

(** [glb_uniquename v] returns the standardized name of global
    variable v: its name if it is a shared variable, its name prefixed
    by the filename if it is a static variable. *)
val glb_uniquename : Cil.varinfo -> string



(*
(** This is the type of the declarations. We have to keep them in a
    list before we can translate them in Newspeak.Decl statement,
    because of the structure of Newspeak.Decl : (typ, blk).

    The var_id represents the corresponding cil id *)
type decl_t = {
  var_decl    : Newspeak.decl;
  var_cil_vid : int;
  var_loc     : Newspeak.location;
  mutable var_used : bool;
}

val new_decl : Newspeak.decl -> int -> Newspeak.location -> bool -> decl_t



(** A function specification contains all its local variables (return
    variable, arguments and real local vars), its instructions and the
    location where it is defined or declared *)
type fun_spec_t = {
  mutable ret_type : Newspeak.typ option;
  mutable formals  : decl_t list option;
  mutable locals   : decl_t list;
  mutable body     : Cil.stmt list;
  mutable fun_loc  : Newspeak.location option;
}

*)




(*
(** [glb_declare v defd init] adds the declaration of v in the global
  list, and if the variable is defined, keeps the initializers for
  later, or set to zero the corresponding zone if init=None and
  global_zero_init is set *)
val glb_declare : (Cil.varinfo * bool * Cil.init option) -> unit


(** The two following functions are called during the first pass *)

(** [glb_make_cstr s] creates a global variable containing s *)
val glb_make_cstr : string -> unit

(** [glb_uses filename v] tells the environment that the variable v in
    the file filename is used if v is global, or else does nothing *)
val glb_uses : Cil.varinfo -> unit


(** [get_glb_decls_inits translate_exp] retrives the list of the
    global variables and translates the initializers *)
val get_glb_decls_inits : (Cil.exp -> Newspeak.exp) -> Newspeak.decl list
*)





(** {1 Locals} *)

(** {2 Functions used in translate_fun} *)

(** [loc_declare v] adds the declaration of v in the local list *)
val loc_declare : bool -> Cil.varinfo -> unit

(** [get_loc_decls ()] returns the current list of local declaration,
    and reset the local handler (counter, hashtable and decl list) *)
val get_loc_decls : unit -> (Newspeak.decl * Newspeak.location) list


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

val extract_type : Newspeak.decl * Newspeak.location -> Newspeak.typ

(*
(** allows to update the specification of a function (prototype) when
    called for example *)
val update_fun_spec : string -> Newspeak.typ option -> decl_t list option -> decl_t list ->
  Cil.stmt list -> Newspeak.location option -> unit

(** declaration of a function from a definition *)
val fun_declare : Cil.fundec -> unit

(** declaration of a function from a prototype *)
val fun_declare_prototype : 
  (string * Cil.typ * (string * Cil.typ * Cil.attributes) list option) -> unit

(** [get_fun_spec name] retrieves the specification of function name
    if possible *)
val get_fun_spec : string -> fun_spec_t option

*)



(** {1 Variable id retrieval} *)

(*val get_glb_var : string -> Newspeak.lval

val get_glb_typ : string -> Newspeak.scalar_t*)

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



(*
(* TODO: to be removed *)
val fun_specs : (string, fun_spec_t) Hashtbl.t
type glb_t = {
  gv_name         : string;
  gv_cstr         : string;
  mutable gv_ctyp : Cil.typ;
  mutable gv_cinit: Cil.init option;
  mutable gv_defd : bool;
  mutable gv_cloc : Cil.location;
  mutable gv_used : bool;
}
val glb_tabl : (string, glb_t) Hashtbl.t
*)
