open Cilutils
open Newspeak

type t = (exp list * gdecl list * (fid, fundec) Hashtbl.t)

and gdecl = (string * typ * init_t)

and fundec = ftyp * blk option

and stmtkind =
    Set of (lval * exp * scalar_t)
  | Copy of (lval * lval * size_t)
  | Decl of (string * typ * blk)
  | Label of lbl
  | Goto of lbl
  | Call of fn
  | ChooseAssert of (exp list * blk) list
  | InfLoop of blk

and stmt = stmtkind * location

and blk = stmt list

and lval =
    Local of vid
  | Global of string
  | Deref of (exp * size_t)
  | Shift of (lval * exp)

and exp =
    Const of cte
  | Lval of (lval * scalar_t)
  | AddrOf of (lval * tmp_int)
  | AddrOfFun of fid
  | UnOp of (unop * exp)
  | BinOp of (binop * exp * exp)

and fn =
    FunId of fid
  | FunDeref of (exp * ftyp)

and init_t = (size_t * scalar_t * exp) list option

and unop =
      Belongs_tmp of (Int64.t * tmp_int)
    | Coerce of (Int64.t * Int64.t)
    | Not
    | BNot of (Int64.t * Int64.t)
    | PtrToInt of ikind
    | Cast of (scalar_t * scalar_t)

and typ = 
    Scalar of scalar_t
  | Array of (typ * tmp_size_t)
  | Region of (field list * size_t)

and ftyp = typ list * typ option

and field = offset * typ

and tmp_int =
      Known of int
    | Length of string
    | SizeOf of string

and tmp_size_t = int option

module String_set :
  sig
    type elt = string
    type t
    val empty : t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val union : t -> t -> t
    val iter : (elt -> unit) -> t -> unit
  end

(* TODO: extern storage not well handled !!! 
   By default, we accept extern as if they were declared but not defined 
*)
type glb_type = {
  mutable gtype : typ;
  mutable gloc  : Newspeak.location;
(* None is for extern *)
  mutable ginit : init_t option;
}

type fspec_type = {
  mutable prett : typ option;
  mutable pargs : ((int * string * typ) list) option;
  mutable plocs : ((int * string * typ) list) option;
  mutable ploc  : Newspeak.location;
  mutable pbody : blk option;
  mutable pcil_body : Cil.block option
}

type intermediate = {
  ifilename : string;
  iglobs : (string, glb_type) Hashtbl.t;
(*  ifuns  : (Newspeak.fid, fspec_type) Hashtbl.t;*)
  iusedglbs : String_set.t;
  iusedcstr : String_set.t;
  iusedfuns : String_set.t;
}

val zero : exp
val zero_f : exp

(** [make_int_coerce t e] wraps e into a coerce expression using
    integer bounds of type t *)
val make_int_coerce : sign_t * size_t -> exp -> exp


(** [make_belongs len e] wraps e into a belongs (0, len - 1) *)
(*val make_belongs : int -> exp -> exp*)

(** [exp_of_int i] wraps i into a Newspeak expression *)
val exp_of_int : int -> exp

val negate : exp -> exp

val dump_npko : (intermediate * (Newspeak.fid, fspec_type) Hashtbl.t) -> unit

val string_of_typ : typ -> string

val compare_typs : typ -> typ -> bool
