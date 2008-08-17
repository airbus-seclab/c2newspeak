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
open Npkil

let vcnt = ref min_int

let fresh_id () =
  let id = !vcnt in
    if (!vcnt = max_int) then Npkcontext.error "Cir.fresh_id" "no more ids";
    incr vcnt;
    id
  
type prog = (glbdecls * fundefs * Newspeak.specs)

and glbdecls = (string, typ * location * init option) Hashtbl.t

and init = (int * scalar_t * exp) list option

and field = (string * (int * typ))

(* TODO: remove location, unused! *)
and fundefs = (string, (ftyp * Newspeak.location * funbody option)) Hashtbl.t

and funbody = ((vid * vid list) * blk)

and vid = int

and typ =
    | Void
    | Scalar of Newspeak.scalar_t
    | Array of array_t
(* TODO: Struct and Union 
   merge them as a Region *)
    | Struct of (field list * int)
    | Union of (field list * int)
    | Fun of ftyp

and array_t = (typ * int option)

and ftyp = typ list * typ

and blk = stmt list

and stmt = (stmtkind * location)

and stmtkind =
    | Block of (blk * (lbl * blk) option)   (* DoWith construct *)
    | Goto of lbl
    | Decl of (typ * string * int)
    | Set of (lv * typ * exp)
    | Loop of blk
    | If of (exp * blk * blk)
    | Switch of (exp * (typ_exp * blk) list * blk)
    | Exp of exp

and lbl = int

and typ_lv = (lv * typ)

and typ_exp = (exp * scalar_t)

(* TODO: maybe still keep together lv and exp ?? *)
and lv =
(* variable identified by its unique id. Use fresh_id () to generate
   a new variable *)
    | Var of vid
    | Global of string
    | Shift of (lv * exp)
    | Deref of (exp * typ)
(* the boolean is true if stmt is after, false otherwise *)
(* TODO: remove the boolean, by adding temporary variables and then
   having some optimization get
   rid of unnecessary temporary variable??? If better *)
    | Stmt_lv of (stmt * lv * bool)

and exp =
    | Const of cst
    | Lval of typ_lv
    | AddrOf of typ_lv
    | Unop of (Npkil.unop * exp)
    | Binop of (Newspeak.binop * exp * exp)
    | Call of (ftyp * funexp * exp list)
    | Pref of (blk * exp)

and funexp =
    | Fname of string
    | FunDeref of (exp * ftyp)

and cst =
    | CInt of Nat.t
    | CFloat of (float * string)


let rec string_of_exp margin e =
  match e with
      Const (CInt i) -> Big_int.string_of_big_int (Nat.to_big_int i)
    | Const _ -> "cst"
    | Lval _ -> "lv"
    | AddrOf _ -> "&lv"
    | Unop (op, e) -> (Npkil.string_of_unop op)^"("^(string_of_exp margin e)^")"
    | Binop (op, e1, e2) -> 
	(string_of_exp margin e1)
	^" "^(Newspeak.string_of_binop op)^" "
	^(string_of_exp margin e2)
    | Call _ -> "f()"
    | Pref (blk, e) -> 
	"("^(string_of_blk margin blk)^(string_of_exp margin e)^")"

and string_of_blk margin x =
  match x with
      [] -> ""
    | (Block (body, None), _)::tl -> 
	(string_of_blk margin body)^(string_of_blk margin tl)
    | hd::tl -> 
	margin^(string_of_stmt margin hd)^"\n"^(string_of_blk margin tl)

and string_of_stmt margin (x, _) =
  match x with
    | Block (body, Some (lbl, action)) ->
	"{\n"
	^(string_of_blk (margin^"  ") body)
	^margin^"} with lbl"^(string_of_int lbl)^" {\n"
	^(string_of_blk (margin^"  ") action)
	^margin^"}"
    | Goto lbl -> "goto lbl"^(string_of_int lbl)^";"
    | Decl (_, x, _) -> "typ "^x^";"
    | Set (_, _, e) -> "lv = "^(string_of_exp margin e)^";"
    | If (e, br1, br2) -> 
	"if "^(string_of_exp margin e)^" {\n"
	^(string_of_blk (margin^"  ") br1)
	^margin^"} else {\n"
	^(string_of_blk (margin^"  ") br2)
	^margin^"}"
    | Exp e -> string_of_exp margin e
    | _ -> "not implemented yet"

let string_of_exp = string_of_exp ""

let string_of_blk = string_of_blk ""

let create_tmp loc t = 
  let id = fresh_id () in
  let x = "!tmp"^(string_of_int id) in
  let decl = (Decl (t, x, id), loc) in
  let v = Var id in
    (decl, v)
	
let exp_of_int i = Const (CInt (Nat.of_int i))

let exp_of_float x = Const (CFloat (x, string_of_float x))

(* TODO: this is a temporary hack, remove this function and align_of 
   put in csyntax *)
let rec size_of t =
  match t with
      Scalar t -> Newspeak.size_of_scalar Config.size_of_ptr t
    | Array (t, Some n) -> (size_of t) * n
    | Struct (_, n) | Union (_, n) -> n
    | Fun _ -> Npkcontext.error "Csyntax.size_of" "unknown size of function"
    | Array _ -> Npkcontext.error "Csyntax.size_of" "unknown size of array"
    | Void -> Npkcontext.error "Csyntax.size_of" "unknown size of void"

let int_of_exp e =
  let rec int_of_exp e =
    match e with
	Const (CInt i) -> i
      | Binop (PlusI, e1, e2) ->
	  let i1 = int_of_exp e1 in
	  let i2 = int_of_exp e2 in
	    Nat.add i1 i2
      | Binop (MinusI, e1, e2) ->
	  let i1 = int_of_exp e1 in
	  let i2 = int_of_exp e2 in
	    Nat.sub i1 i2
      | Binop (MultI, e1, e2) ->
	  let i1 = int_of_exp e1 in
	  let i2 = int_of_exp e2 in
	    Nat.mul i1 i2
      | Binop (DivI, e1, e2) ->
	  let i1 = int_of_exp e1 in
	  let i2 = int_of_exp e2 in
	    if (Nat.compare i2 Nat.zero = 0) 
	    then Npkcontext.error "Cir.int_of_exp" "division by zero";
	    Nat.div i1 i2
      | Binop (Shiftlt, e1, e2) -> 
	  let i1 = int_of_exp e1 in
	  let i2 = int_of_exp e2 in
	  let i2 = Nat.to_int i2 in
	    Nat.shift_left i1 i2
      | Unop (Coerce b, e) -> 
	  let i = int_of_exp e in
	    if Newspeak.belongs i b then i 
	    else Npkcontext.error "Cir.int_of_exp" "integer overflow"
      | _ -> 
	  Npkcontext.error "Cir.int_of_exp" 
	    "static expression expected"
  in
    Nat.to_int (int_of_exp e)

(* TODO: if possible remove int_kind, int_typ and char_typ, they are
   in csyntax rather *)
let int_kind = (Signed, Config.size_of_int)

let char_typ = Scalar (Int (Signed, Config.size_of_char))

let int_typ = Scalar (Int int_kind)

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
    | Stmt_lv (stmt, lv, is_after) -> 
	let (pref, lv, post) = normalize_lv lv in
	let stmt = normalize_stmt stmt in
	let (pref, post) = 
	  if is_after then (pref, concat_effects post stmt)
	  else (concat_effects stmt pref, post)
	in
	  (pref, lv, post)
	    
and normalize_stmt (x, loc) = 
  Npkcontext.set_loc loc;
  match x with
      Block (body, lbl) -> 
	let body = normalize_blk body in
	let lbl = 
	  match lbl with
	      None -> None
	    | Some (lbl, action) -> Some (lbl, normalize_blk action)
	in
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
	let (pref, e) = normalize_exp_post loc e (Scalar Newspeak.FunPtr) in
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
    (* maps each lbl to the variable that should be declared at this block *)
  let age_tbl = Hashtbl.create 20 in

  let push_lbl lbl =
    Hashtbl.add lbl_tbl lbl [];
    Hashtbl.add age_tbl !stack_height lbl;
    incr stack_height
  in
  let pop_lbl lbl =
    let decls = Hashtbl.find lbl_tbl lbl in
      decr stack_height;
      Hashtbl.remove age_tbl !stack_height;
      Hashtbl.remove lbl_tbl lbl;
      decls
  in
  let register_decl lbl x =
    let decls = 
      try Hashtbl.find lbl_tbl lbl 
      with Not_found -> 
	Npkcontext.error "Cir.normalize.register_decl" 
	  ("unexpected label lbl"^(string_of_int lbl))
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

      | (Block (body, Some (lbl, act_blk)), loc)::tl -> 
	  push_lbl lbl;
	  let (body, used_lbls1) = set_scope_blk body in
	  let used_lbls1 = Set.remove lbl used_lbls1 in
	  let decls = pop_lbl lbl in
	  let (act_blk, used_lbls2) = set_scope_blk act_blk in
	  let used_lbls = Set.union used_lbls1 used_lbls2 in
	  let body = ((Block (body, Some (lbl, act_blk)), loc)::[]) in
	  let body = 
	    if Set.is_empty used_lbls then begin
	      let body = List.rev_append decls body in
		(Block (body, None), loc)::[]
	    end else begin
	      let lbl = Set.min_elt used_lbls in
		List.iter (register_decl lbl) decls;
		body
	    end
	  in
	  let (tl, used_lbls') = set_scope_blk tl in
	    (body@tl, Set.union used_lbls used_lbls')
	      
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
  match (t, e, t') with
    | _ when t = t' -> e
	(* TODO: this should be probably put in firstpass *)
    | (Fun _, Lval lv, Scalar (FunPtr|Ptr|Int _ as t')) -> 
	Unop (Npkil.Cast (FunPtr, t'), AddrOf lv)
    | (Scalar (Int _), _, Scalar (Int k)) -> 
	Unop (Npkil.Coerce (Newspeak.domain_of_typ k), e)
    | (Scalar t, _, Scalar t') -> Unop (Npkil.Cast (t, t'), e)
    | (Void, _, _) -> 
	Npkcontext.error "Cir.cast" 
	  "value void not ignored as it ought to be"
    | _ -> Npkcontext.error "Cir.cast" "scalar type expected for cast"

let string_of_typ t =
  match t with
    | Void -> "void"
    | Scalar t -> Newspeak.string_of_scalar t
    | Array _ -> "a[i]"
    | Struct _ -> "{}"
    | Union _ -> "{}"
    | Fun _ -> "fun"

let rec is_subtyp t1 t2 =
  match (t1, t2) with
      (Array (t1, l1), Array (t2, l2)) -> 
	(is_sublen l1 l2) && (is_subtyp t1 t2)
    | (Struct (f1, n1), Struct (f2, n2)) 
    | (Union (f1, n1), Union (f2, n2)) -> begin
	try (n1 = n2) && (List.for_all2 is_subfield f1 f2)
	with Invalid_argument _ -> false
      end
    | (Fun f1, Fun f2) -> is_subftyp f1 f2
    | _ -> t1 = t2

and is_sublen l1 l2 =
  match (l1, l2) with
      (_, None) -> true
    | (Some i1, Some i2) -> i1 = i2
    | (None, _) -> false

and is_subfield (f1, (o1, t1)) (f2, (o2, t2)) =
  (f1 = f2) && (o1 = o2) && (is_subtyp t1 t2)

and is_subftyp (args1, ret1) (args2, ret2) =
  try (is_subtyp ret1 ret2) && (List.for_all2 is_subtyp args1 args2)
  with Invalid_argument _ -> false

(* a large block has at least 3 instructions or has a call *)
let is_large_blk x =
  let cnt = ref 0 in
  let rec check_blk x =
    match x with
	(hd, _)::tl -> 
	  incr cnt;
	  if !cnt > 2 then raise Exit;
	  check_stmt hd;
	  check_blk tl
      | [] -> ()

  and check_stmt x =
    match x with
	Block (x, None) | Loop x -> check_blk x
      | Block (x, Some (_, y)) -> check_blk x; check_blk y
      | If (e, x, y) -> check_blk x; check_blk y; check_exp e
      | Switch (e, choices, x) -> 
	  check_exp e;
	  List.iter check_choice choices;
	  check_blk x;
      | Set (lv, _, e) -> check_lval lv; check_exp e
      | Exp e -> check_exp e
      | _ -> ()

  and check_choice ((e, _), blk) = check_exp e; check_blk blk

  and check_lval x =
    match x with
      | Shift (lv, e) -> check_lval lv; check_exp e
      | Deref (e, _) -> check_exp e
      | Stmt_lv ((stmt, _), lv, _) -> check_stmt stmt; check_lval lv
      | _ -> ()

  and check_exp e =
    match e with
      | Lval (lv, _) | AddrOf (lv, _) -> check_lval lv
      | Unop (_, e) -> check_exp e
      | Binop (_, e1, e2) -> check_exp e1; check_exp e2
      | Call _ -> raise Exit
      | Pref (blk, e) -> check_blk blk; check_exp e
      | _ -> ()
  in
    try 
      check_blk x;
      false
    with Exit -> true

