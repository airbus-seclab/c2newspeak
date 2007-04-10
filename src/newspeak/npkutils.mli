(** Npkutils regroups simple translation functions *)


module Int_set :
  sig
    type elt = int
    type t
    val empty : t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
  end

(* TODO: should this name, dangerous *)
val incr : int ref -> int


(** [translate_loc cil_loc] translates a Cil.location cil_loc into a
    Newspeak.location *)
val translate_loc : Cil.location -> Newspeak.location

(** [update_loc cil_loc] translates a Cil.location cil_loc into a
    Newspeak.location and stores it into the cur_loc reference of module
    Npkcontext *)
val update_loc : Cil.location -> unit

val get_cur_file : unit -> string


(** The following functions handle binop translations *)

val translate_arith_binop : Cil.binop -> Newspeak.binop
val translate_float_binop : Newspeak.size_t -> Cil.binop -> Newspeak.binop
val translate_logical_binop : (Newspeak.sign_t * Newspeak.size_t) -> Cil.binop -> Newspeak.binop
val translate_rel_binop : Cil.typ -> Cil.typ -> Cil.binop -> Newspeak.binop


(** [translate_typ cil_typ] returns the translation of [cil_typ] in
    Newspeak, if the transformation is possible *)
val translate_typ : Cil.typ -> Npkil.typ

val isPtr : Cil.exp -> bool
