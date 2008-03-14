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

(* TODO: remove size_of, put in csyntax *)
open Newspeak

let vcnt = ref min_int

let fresh_id () =
  let id = !vcnt in
    if (!vcnt = max_int) then Npkcontext.error "Cir.fresh_id" "no more ids";
    incr vcnt;
    id
  
type prog = (glbdecls * fundefs * Newspeak.specs)

and glbdecls = (string, typ * location * init option) Hashtbl.t

and init = (int * typ * exp) list option

and field = (string * (int * typ))

(* TODO: remove location, unused! *)
and fundefs = (string, (ftyp * Newspeak.location * funbody option)) Hashtbl.t

and funbody = ((vid * vid list) * blk)

and vid = int

and typ =
    | Void
    | Int of ikind
    | Float of int
    | Ptr
    | FunPtr
    | Array of array_t
(* TODO: remove string in Struct and Union 
   merge them as a Region *)
    | Struct of (string * field list * int)
    | Union of (string * field list * int)
    | Fun of ftyp

and array_t = (typ * int option)

and ftyp = typ list * typ

and blk = stmt list

and stmt = (stmtkind * location)

and stmtkind =
    | Block of (blk * lbl option)
    | Goto of lbl
    | Decl of (typ * string * int)
    | Set of (lv * typ * exp)
    | Loop of blk
    | If of (exp * blk * blk)
    | Switch of (exp * (typ_exp * blk) list * blk)
    | Exp of exp

and lbl = int

and typ_lv = (lv * typ)

and typ_exp = (exp * typ)

(* TODO: maybe still keep together lv and exp ?? *)
and lv =
(* variable identified by its unique id. Use fresh_id () to generate
   a new variable *)
    | Var of vid
    | Global of string
    | Shift of (lv * exp)
    | Deref of (exp * typ)
(* TODO: remove Post by using Pref instead and having some optimization get
   rid of unnecessary temporary variable??? If better *)
    | Post_lv of (lv * stmt)
    | Pref_lv of (stmt * lv)

and exp =
    | Const of cst
    | Lval of typ_lv
    | AddrOf of typ_lv
    | Unop of (unop * exp)
    | Binop of (binop * exp * exp)
    | Call of (ftyp * funexp * exp list)
    | Pref of (blk * exp)

and funexp =
    | Fname of string
    | FunDeref of (exp * ftyp)

and unop = 
    | Belongs_tmp of (Int64.t * Npkil.tmp_int)
    | Not
    | BNot of ikind
    | Cast of (typ * typ)

(* TODO: remove ikind for operations (add a coerce operator), be closer to 
   npkil and more low level!! *)
and binop =
    | Plus of ikind
    | Minus of ikind
    | Div of ikind
    | Mult of ikind
    | BAnd of ikind
    | BXor of ikind
    | BOr of ikind
    | Mod
    | PlusP of typ
    | MinusP
    | Gt of typ
    | Eq of typ
    | Shiftl of ikind
    | Shiftr of ikind
    | PlusF of int
    | MinusF of int
    | DivF of int
    | MultF of int

and cst =
    | CInt of Int64.t
    | CFloat of (float * string)


let create_tmp loc t = 
  let id = fresh_id () in
  let x = "!tmp"^(string_of_int id) in
  let decl = (Decl (t, x, id), loc) in
  let v = Var id in
    (decl, v)
	
let exp_of_int i = Const (CInt (Int64.of_int i))

let exp_of_float x = Const (CFloat (x, string_of_float x))

(* TODO: this is a temporary hack, remove this function and align_of 
   put in csyntax *)
let rec size_of t =
  match t with
      Int (_, n) -> n 
    | Float n -> n
    | Ptr | FunPtr -> Config.size_of_ptr
    | Array (t, Some n) -> (size_of t) * n
    | Struct (_, _, n) | Union (_, _, n) -> n
    | Fun _ -> Npkcontext.error "Csyntax.size_of" "Unknown size of function"
    | Array _ -> Npkcontext.error "Csyntax.size_of" "Unknown size of array"
    | Void -> Npkcontext.error "Csyntax.size_of" "Unknown size of void"

let int_of_exp e =
  let rec int_of_exp e =
    match e with
	Const (CInt i) -> Big_int.big_int_of_string (Int64.to_string i)
      | Binop (Plus _, e1, e2) ->
	  let i1 = int_of_exp e1 in
	  let i2 = int_of_exp e2 in
	    Big_int.add_big_int i1 i2
      | Binop (Minus _, e1, e2) ->
	  let i1 = int_of_exp e1 in
	  let i2 = int_of_exp e2 in
	    Big_int.sub_big_int i1 i2
      | Binop (Mult _, e1, e2) ->
	  let i1 = int_of_exp e1 in
	  let i2 = int_of_exp e2 in
	    Big_int.mult_big_int i1 i2
      | Binop (Div _, e1, e2) ->
	  let i1 = int_of_exp e1 in
	  let i2 = int_of_exp e2 in
	    Big_int.div_big_int i1 i2
      | _ -> 
	  Npkcontext.error "Csyntaxt.int_of_exp" 
	    "static expression expected"
  in
  let i = int_of_exp e in
    if not (Big_int.is_int_big_int i) then begin
      Npkcontext.error "Csyntax.len_of_exp" 
	("expression can not be evaluated to an int: "
	 ^(Big_int.string_of_big_int i))
    end;
    Big_int.int_of_big_int i

(* TODO: if possible remove int_kind, int_typ and char_typ, they are
   in csyntax rather *)
let int_kind = (Signed, Config.size_of_int)

let int_typ = Int int_kind

let char_typ = Int (Signed, Config.size_of_char)

let promote k = 
  match k with
      (_, n) when n < Config.size_of_int -> int_kind
    | _ -> k

let concat_effects blk1 blk2 =
  if (blk1 <> []) && (blk2 <> []) then begin
    Npkcontext.print_warning "Cir.concat_effect" 
      ("the order of execution of side-effects in expressions not specified, "
	^"picking a random one, be careful")
  end;
  (* TODO: Could pick randomly this sequence *)
  blk1@blk2

let remove_post loc (pref, e, post) t =
  let (decl, v) = create_tmp loc t in
  let set = (Set (v, t, e), loc) in
    (pref@decl::set::post, v)

(* Removes Pref and Post subexpressions (removes side-effects) 
   pushes calls at top level *)
let rec normalize_exp x =
  match x with
      Const _ -> ([], x, [])
    | Lval (lv, t) ->
	let (pref, lv, post) = normalize_lv lv in
	  (pref, Lval (lv, t), post)
    | AddrOf (lv, t) -> 
	let (pref, lv, post) = normalize_lv lv in
	  (pref, AddrOf (lv, t), post)
    | Unop (op, e) ->
	let (pref, e, post) = normalize_exp e in
	  (pref, Unop (op, e), post)
    | Binop (op, e1, e2) ->
	let (pref1, e1, post1) = normalize_exp e1 in
	let (pref2, e2, post2) = normalize_exp e2 in
	let pref = concat_effects pref1 pref2 in
	let post = concat_effects post1 post2 in
	  (pref, Binop (op, e1, e2), post)

    | Call (ft, f, args) ->
	let loc = Npkcontext.get_loc () in
	let (pref, call) = normalize_call loc (ft, f, args) in
	let (_, t) = ft in
	let (decl, v) = create_tmp loc t in
	let call = (Set (v, t, call), loc) in
	  (pref@decl::call::[], Lval (v, t), [])
	  
    | Pref (blk, e) ->
	let blk = normalize_blk blk in
	let (pref, e, post) = normalize_exp e in
	let pref = concat_effects blk pref in
	  (pref, e, post)
	    
and normalize_lv x =
  match x with
      Var _ | Global _ -> ([], x, [])
    | Shift (lv, e) ->
	let (pref1, lv, post1) = normalize_lv lv in
	let (pref2, e, post2) = normalize_exp e in
	let pref = concat_effects pref1 pref2 in
	let post = concat_effects post1 post2 in
	  (pref, Shift (lv, e), post)
    | Deref (e, t) ->
	let (pref, e, post) = normalize_exp e in
	  (pref, Deref (e, t), post)
    | Pref_lv (stmt, lv) ->
	let (pref, lv, post) = normalize_lv lv in
	let stmt = normalize_stmt stmt in
	let pref = concat_effects stmt pref in
	  (pref, lv, post)
    | Post_lv (lv, stmt) ->
	let (pref, lv, post) = normalize_lv lv in
	let stmt = normalize_stmt stmt in
	let post = concat_effects post stmt in
	  (pref, lv, post)
	    
and normalize_stmt (x, loc) = 
  Npkcontext.set_loc loc;
  match x with
      Block (body, lbl) -> 
	  let body = normalize_blk body in
	    (Block (body, lbl), loc)::[]

    | Goto _ | Decl _ -> (x, loc)::[]
	  

    | Set (lv, t, Call c) ->
	let (pref1, lv) = normalize_lv_post loc lv t in
	let (pref2, e) = normalize_call loc c in
	let pref = concat_effects pref1 pref2 in
	  (Block (pref@(Set (lv, t, e), loc)::[], None), loc)::[]

    | Set (lv, t, e) ->
	let (pref1, lv, post1) = normalize_lv lv in
	let (pref2, e, post2) = normalize_exp e in
	let pref = concat_effects pref1 pref2 in
	let post = concat_effects post1 post2 in
	  (Block (pref@(Set (lv, t, e), loc)::post, None), loc)::[]
	    
    | Loop body -> (Loop (normalize_blk body), loc)::[]
	
    | If (e, body1, body2) ->
	let (pref, e, post) = normalize_exp e in
	let body1 = normalize_blk body1 in
	let body2 = normalize_blk body2 in
	let body = pref@(If (e, post@body1, post@body2), loc)::[] in
	  (* TODO: not good, code duplication!!! 
	     could add a variable instead, if variable elimination later 
	     on is good enough *)
	  (Block (body, None), loc)::[]

    | Switch (e, choices, default) ->
	let (pref, e, post) = normalize_exp e in
	let choices = List.map (normalize_choice post) choices in
	let default = normalize_blk default in
	  pref@(Switch (e, choices, default), loc)::[]

    | Exp (Call call) -> 
	let (pref, call) = normalize_call loc call in
	  pref@(Exp call, loc)::[]

    | Exp e ->
	let (pref, _, post) = normalize_exp e in
	  (Block (concat_effects pref post, None), loc)::[]
	    
and normalize_call loc (ft, f, args) =
  let (pref1, f) = normalize_funexp loc f in
  let (args_t, _) = ft in
  let (pref2, args) = normalize_args loc args args_t in
  let pref = concat_effects pref1 pref2 in
    (pref, Call (ft, f, args))

and normalize_funexp loc f =
  match f with
      Fname _ -> ([], f)
    | FunDeref (e, ft) ->
	let (pref, e) = normalize_exp_post loc e FunPtr in
	  (pref, FunDeref (e, ft))
	      
and normalize_lv_post loc lv t =
  let (pref, lv, post) = normalize_lv lv in
    if (post <> []) then begin
      let (pref, v) = remove_post loc (pref, Lval (lv, t), post) t in
	(pref, v)
    end else (pref, lv)
      
and normalize_exp_post loc e t =
  let (pref, e, post) = normalize_exp e in
    if (post <> []) then begin
      let (pref, v) = remove_post loc (pref, e, post) t in
	(pref, Lval (v, t))
    end else (pref, e)
      
and normalize_rets loc rets t =
  match rets with
      lv::[] -> 
	let (pref, lv) = normalize_lv_post loc lv t in
	  (pref, lv::[])
    | [] -> ([], [])
    | _ -> Npkcontext.error "Cir.normalize_rets" "unreachable statement"
	
and normalize_args loc args args_t =
  match (args, args_t) with
      (e::args, t::args_t) -> 
	let (pref1, args) = normalize_args loc args args_t in
	let (pref2, e) = normalize_exp_post loc e t in
	let pref = concat_effects pref1 pref2 in
	  (pref, e::args)
    | ([], []) -> ([], [])
    | _ -> Npkcontext.error "Cir.normalize_rets" "unreachable statement"
	
and normalize_choice pref ((e, t), body) =
  let (empty_pref, e, empty_post) = normalize_exp e in
  let body = normalize_blk body in
    if (empty_pref <> []) || (empty_post <> []) then begin
      Npkcontext.error "Firstpass.normalize_choice"
	"integer constant expression expected"
    end;
    (* TODO: not good, code duplication!!! 
       could add a variable instead, if variable elimination later on is 
       good enough *)
    ((e, t), pref@body)
      
and normalize_blk x =
  match x with
      hd::tl -> (normalize_stmt hd)@(normalize_blk tl)
    | [] -> []
	
module Int =
struct
  type t = int
  let compare = compare
end
module Set = Set.Make(Int)

let normalize x =
  let stack_height = ref 0 in
  let lbl_tbl = Hashtbl.create 20 in
  let age_tbl = Hashtbl.create 20 in

  let push_lbl lbl =
    Hashtbl.add lbl_tbl lbl [];
    Hashtbl.add age_tbl !stack_height lbl;
    incr stack_height
  in
  let pop_lbl lbl body loc =
    let decls = Hashtbl.find lbl_tbl lbl in
    let body = List.rev_append decls ((Block (body, Some lbl), loc)::[]) in
    let body = (Block (body, None), loc)::[] in
      decr stack_height;
      Hashtbl.remove age_tbl !stack_height;
      Hashtbl.remove lbl_tbl lbl;
      body
  in
  let register_decl lbl x =
    let decls = 
      try Hashtbl.find lbl_tbl lbl 
      with Not_found -> 
	Npkcontext.error "Cir.normalize.register_decl" "unexpected label"
    in
      Hashtbl.replace lbl_tbl lbl (x::decls)
  in

  let rec set_scope_blk x =
    match x with
	((Decl _, _) as decl)::body ->
	  let (body, used_lbls) = set_scope_blk body in
	  let body =
	    if Set.is_empty used_lbls then decl::body
	    else begin
	      let lbl = Set.min_elt used_lbls in
		register_decl lbl decl;
		body
	    end
	  in
	    (body, used_lbls)

      | (Block (body, Some lbl), loc)::tl -> 
	  push_lbl lbl;
	  let (body, used_lbls) = set_scope_blk body in
	  let body = pop_lbl lbl body loc in
	  let used_lbls1 = Set.remove lbl used_lbls in
	  let (tl, used_lbls2) = set_scope_blk tl in
	    (body@tl, Set.union used_lbls1 used_lbls2)
	      
      | (x, loc)::tl -> 
	  let (x, used_lbls1) = set_scope_stmtkind x in
	  let (tl, used_lbls2) = set_scope_blk tl in
	    ((x, loc)::tl, Set.union used_lbls1 used_lbls2)
      | [] -> ([], Set.empty)

  and set_scope_stmtkind x =
    match x with
      | Block (body, lbl) -> 
	  let (body, used_lbls) = set_scope_blk body in
	    (Block (body, lbl), used_lbls)
      | Goto lbl -> (x, Set.singleton lbl)
      | Decl _ | Set _ | Exp _ -> (x, Set.empty)
      | Loop body -> 
	  let (body, used_lbls) = set_scope_blk body in
	    (Loop body, used_lbls)
      | If (e, body1, body2) ->
	  let (body1, used_lbls1) = set_scope_blk body1 in
	  let (body2, used_lbls2) = set_scope_blk body2 in
	    (If (e, body1, body2), Set.union used_lbls1 used_lbls2)
      | Switch (e, choices, default) ->
	  let (choices, used_lbls1) = set_scope_choices choices in
	  let (default, used_lbls2) = set_scope_blk default in
	    (Switch (e, choices, default), Set.union used_lbls1 used_lbls2)

  and set_scope_choices x =
    match x with
	(e, body)::tl ->
	  let (tl, used_lbls1) = set_scope_choices tl in
	  let (body, used_lbls2) = set_scope_blk body in
	    ((e, body)::tl, Set.union used_lbls1 used_lbls2)
      | [] -> ([], Set.empty)
  in
    
  let x = normalize_blk x in
  let (body, _) = set_scope_blk x in
    body

let len_of_array n lv =
  match (n, lv) with
      (Some n, _) -> Npkil.Known n
    | (_, Global x) -> Npkil.Length x
    | _ -> Npkcontext.error "Cir.len_of_array" "unknown array length"

(* TODO: this should be probably put in firstpass *)
let cast (e, t) t' =
  let (e, t) =
    match (t, e, t') with
	(Array _, Lval (lv, Array _), (Ptr|Int _)) -> (AddrOf (lv, t), Ptr)
      | (Fun _, Lval lv, (FunPtr|Ptr|Int _)) -> (AddrOf lv, FunPtr)
      | _ -> (e, t)
  in
    if t = Void then begin
      Npkcontext.error "Cir.cast" "value void not ignored as it ought to be"
    end;
    if t = t' then e
    else Unop (Cast (t, t'), e)

let string_of_typ t =
  match t with
    | Void -> "void"
    | Int _ -> "int"
    | Float _ -> "float"
    | Ptr -> "ptr"
    | FunPtr -> "fptr"
    | Array _ -> "a[i]"
    | Struct _ -> "{}"
    | Union _ -> "{}"
    | Fun _ -> "fun"
