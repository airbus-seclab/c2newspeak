(** The Newspeak module describes the internal language used to analyse
    C code. It is a kind of high-level assembly language.

    The names used in Newspeak are similar to those used in CIL.

    This module also exports some useful functions to manipulate and
    display Newspeak programs. *)



(** {1 Types} *)

(** The type of a program: function definitions and an block
    containing initialisation of the global variables *)
type t = (exp list * decl list * (fid, fundec) Hashtbl.t)

and decl = (typ * string * init_t)

and fundec = ftyp * blk option

and stmtkind =
    Set of (lval * exp * scalar_t)
  | Copy of (lval * lval * size_t)
  | Decl of (decl * blk)
  | Label of lbl
  | Goto of lbl
  | Call of fn
  | ChooseAssert of (exp list * blk) list
  | InfLoop of blk

and stmt = stmtkind * location

and blk = stmt list

and lval =
    Local of vid
  | Global of vid
  | Deref of (exp * size_t)
  | Shift of (lval * exp)

and exp =
    Const of cte
  | Lval of (lval * scalar_t)
  | AddrOf of (lval * size_t)
  | AddrOfFun of fid
  | UnOp of (unop * exp)
  | BinOp of (binop * exp * exp)

and cte = 
    CInt64 of Int64.t
  (* TODO: warning floats with more than 64 bits can not be represented *)
  | Float64 of float
  | Nil

and unop =
    Belongs of (Int64.t * Int64.t)
  | Coerce of (Int64.t * Int64.t)
  | Not
  | BNot of (Int64.t * Int64.t)
  | PtrToInt of ikind
  | Cast of (scalar_t * scalar_t)

and binop =
  | PlusI | MinusI | MultI | DivI | Mod
  | PlusF of size_t | MinusF of size_t | MultF of size_t | DivF of size_t
  | BOr of (Int64.t * Int64.t) | BAnd of (Int64.t * Int64.t)
  | BXor of (Int64.t * Int64.t)
  | Shiftlt
  | Shiftrt
  | PlusPI
  | MinusPP
  | Ge of scalar_t | Gt of scalar_t
  | Eq of scalar_t | Ne of scalar_t

and fn =
    FunId of fid
  | FunDeref of (exp * ftyp)

and typ =
    Scalar of scalar_t
  | Array of (typ * size_t)
  | Region of (field list * size_t)

and field = offset * typ

and scalar_t =
    Int of ikind
  | Float of size_t
  | Ptr
  | FunPtr

and init_t = 
  | Zero
  | Init of (size_t * scalar_t * exp) list

and ftyp = typ list * typ option

and lbl = int
and vid = int
and fid = string

and ikind = sign_t * size_t
and sign_t = Unsigned | Signed
and size_t = int
and offset = int

and location = string * int * int



(** {1 Constants} *)

val zero : exp

val locUnknown : location



(** {1 Manipualtion and Simplifications} *)

(** [size_of t] returns the size of a value of type as the number of bytes it
    takes when stored in memory *) 
val size_of : typ -> size_t

val size_of_scalar : scalar_t -> size_t

(** Given the characteristics of an integer type, [domain_of_typ]
    returns the bounds of the type *)
val domain_of_typ : sign_t * size_t -> Int64.t * Int64.t

(** [make_int_coerce t e] wraps e into a coerce expression using
    integer bounds of type t *)
val make_int_coerce : sign_t * size_t -> exp -> exp

(** [make_belongs len e] wraps e into a belongs (0, len - 1) *)
val make_belongs : int -> exp -> exp

(** Simplifications of coerces and belongs in [make_belongs] and [make_int_coerce]:
    - Coerce \[a;b\] Coerce \[c;d\] e -> Coerce \[a;b\] if \[c;d\] contains \[a;b\]
    - Coerce \[a;b\] Coerce \[c;d\] e -> Coerce \[c;d\] if \[a;b\] contains \[c;d\]
    - Coerce/belongs \[a;b\] (const c) becomes const c if c in \[a;b\]

    Precondition: all Coerce (l,u) verify l <= u *)

(** Negation of a boolean condition *)
val negate : exp -> exp

val init_of_string : string -> (int * (size_t * scalar_t * exp) list)

(** [exp_of_int i] wraps i into a Newspeak expression *)
val exp_of_int : int -> exp

(** [exp_of_float f] wraps f into a Newspeak expression *)
val exp_of_float : float -> exp

(** Deletion of useless Gotos and Labels *)
val simplify_gotos : blk -> blk

(** Run all simplifications *)
val simplify : blk -> blk



(** {1 Display } *)

(** when the pretty_print boolean is set, locals and globals are
    displayed in a prettier way if possible (with their names) *)
val pretty_print : bool ref

(** [dump cout (fundecs, body)] prints the program (fundecs, body) to
    cout *)
val dump : t -> unit

val string_of_typ : typ -> string
val string_of_ftyp : ftyp -> string
val string_of_exp : exp -> string

