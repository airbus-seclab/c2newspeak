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

(* TODO: should count local variables from beginning of function !! *)

open Cilutils
open Newspeak

type t = (filename * (string, ginfo) Hashtbl.t * (fid, funinfo) Hashtbl.t)

and filename = string

(* None is for extern *)
and ginfo = (typ * location * init_t option * used)

and used = bool

and funinfo = (ftyp * blk option)

and stmtkind =
    Set of (lval * exp * scalar_t)
  | Copy of (lval * lval * size_t)
  | Decl of (string * typ * blk)
  | ChooseAssert of (exp list * blk) list
  | InfLoop of blk
  | DoWith of (blk * lbl * blk)
  | Goto of lbl
  | Call of fn

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
    | IntToPtr of ikind
    | Cast of (scalar_t * scalar_t)

and typ = 
    Scalar of scalar_t
  | Array of (typ * tmp_size_t)
  | Region of (field list * size_t)

and ftyp = typ list * typ option

and field = offset * typ

and tmp_int =
      Known of int
    | Length of string (* number of elements in global array named by string *)
    | Mult of (tmp_int * int) (* tmp_int * int *)
    | Decr of tmp_int  (* tmp_int - 1 *)

and tmp_size_t = int option

module String_set = 
  Set.Make (struct type t = string let compare = Pervasives.compare end)

let zero = Const (CInt64 (Int64.zero))
let zero_f = Const (CFloat (0., "0."))

let make_int_coerce int_t e =
  UnOp (Coerce (domain_of_typ int_t), e)


let exp_of_int x = Const (CInt64 (Int64.of_int x))

module Int_map = 
  Map.Make (struct type t = int let compare = Pervasives.compare end)

let rec seq sep f l =
  match l with
    | [] -> ""
    | [e] -> f e
    | e::r -> (f e)^sep^(seq sep f r)

let string_of_size_t = string_of_int

let string_of_sign_t sg =
  match sg with
      Unsigned -> "u"
    | Signed -> ""

let string_of_scalar s =
  match s with
      Int (sg,sz) -> (string_of_sign_t sg)^"int"^(string_of_size_t sz)
    | Float sz -> "float" ^ (string_of_size_t sz)
    | Ptr -> "ptr"
    | FunPtr -> "fptr"

let rec string_of_typ t =
  match t with
      Scalar s -> string_of_scalar s
    | Array (t, Some sz) -> (string_of_typ t)^"["^(string_of_size_t sz)^"]"
    | Array (t, None) -> (string_of_typ t)^"[]"
    | Region (lst, sz) ->
	let string_of_elt (off, t) = 
	  (string_of_typ t)^" "^(string_of_size_t off) 
	in
	  "{"^(seq ";" string_of_elt lst)^"}"^(string_of_size_t sz)

let string_of_fid fid = fid

let rec string_of_tmp_int x =
  match x with
      Known i -> string_of_int i
    | Length v -> "len("^v^")"
    | Mult (v, n) -> "("^(string_of_tmp_int v)^" * "^(string_of_int n)^")"
    | Decr i -> (string_of_tmp_int i)^" - 1"

let string_of_unop op =
  match op with
      Belongs_tmp (l,u) ->
	"belongs["^(Int64.to_string l)^","^(string_of_tmp_int u)^"]"
    | Coerce (l,u) ->
	"coerce["^(Int64.to_string l)^","^(Int64.to_string u)^"]"
    | Cast (typ, typ') ->
	"("^(string_of_scalar typ')^" <= "^(string_of_scalar typ)^")"
    | Not -> "!"
    | BNot _ -> "~"
    | PtrToInt i -> "("^(string_of_scalar (Int i))^")"
    | IntToPtr i -> "(ptr)"
	  
let string_of_binop neg op =
  match op with
    | Gt _ when neg -> ">="
    | Eq _ when neg -> "<>"
    | Gt _ -> ">"
    | Eq _ -> "=="
    | _ when neg ->
	failwith ("Newspeak.string_of_binop: unexpected negation")
    | PlusI -> "+"
    | MinusI -> "-"
    | MultI -> "*"
    | Mod -> "%"
    | DivI -> "/"
    | PlusF _ -> "+."
    | MinusF _ -> "-."
    | MultF _ -> "*."
    | DivF _ -> "/."
    | BAnd _ -> "&"
    | BOr _ -> "|"
    | BXor _ -> "^"
    | Shiftlt -> "<<"
    | Shiftrt -> ">>"
    | PlusPI -> "+"
    | MinusPP -> "-"

let string_of_cte c =
  match c with
      CInt64 c -> Int64.to_string c
    | CFloat (_, s) -> s
    | Nil -> "nil"

let string_of_vid = string_of_int

let string_of_local decls vid =
  try
    List.nth decls vid
  with Failure _ -> (string_of_vid vid) ^ "-"


let rec string_of_lval decls lv =
  match lv with
      Local vid -> string_of_local decls vid
    | Global name -> "Global("^name^")"
    | Deref (e, sz) -> "["^(string_of_exp decls e)^"]"^(string_of_size_t sz)
    | Shift (lv, sh) -> (string_of_lval decls lv)^" + "^(string_of_exp decls sh)

and string_of_exp decls e =
  match e with
      Const c -> string_of_cte c
    | Lval (lv, t) -> (string_of_lval decls lv)^"_"^(string_of_scalar t)
    | AddrOf (lv, sz) -> "&_"^(string_of_tmp_int sz)^"("^(string_of_lval decls lv)^")"
    | AddrOfFun fid -> "&fun"^(string_of_fid fid)

    (* TODO: Check this ! *)
    (* Pretty printing for >= and != *)
    | UnOp (Not, BinOp (op, e1, e2)) (* when !pretty_print *) ->
	"("^(string_of_exp decls e2)^" "^(string_of_binop true op)^
	  " "^(string_of_exp decls e1)^")"

    | BinOp (op, e1, e2) ->
	"("^(string_of_exp decls e1)^" "^(string_of_binop false op)^
	  " "^(string_of_exp decls e2)^")"
	  
    | UnOp (op, exp) -> (string_of_unop op)^" "^(string_of_exp decls exp)

	  
and string_of_fn decls f =
  match f with
      FunId fid -> (string_of_fid fid)^"()"
    | FunDeref (exp, (args_t, Some ret_t)) ->
	"["^(string_of_exp decls exp)^"]("^
	  (seq ", " string_of_typ args_t)^") -> "^(string_of_typ ret_t)
    | FunDeref (exp, (args_t, None)) ->
	"["^(string_of_exp decls exp)^"]("^
	  (seq ", " string_of_typ args_t)^")"

(* TODO: remove pretty option here and Npkcontext *)
let dump_npko (fname, globs, funs) = 
  let cur_fun = ref "" in

  let lbls = ref (Int_map.empty) in
  let lbl_index = ref 0 in

  let string_of_lbl l =
    if not !Npkcontext.pretty_print
    then "lbl"^(string_of_int l)
    else
      let pretty_l = try
	  Int_map.find l !lbls
	with Not_found ->
	  lbl_index := !lbl_index + 1;
	  lbls := Int_map.add l !lbl_index !lbls;
	  !lbl_index
      in !cur_fun^"_"^(string_of_int pretty_l)
  in

  let rec dump_blk align decls b =
    match b with
      | hd::[] -> dump_stmt align decls true hd
      | hd::r ->
	  dump_stmt align decls false hd;
	  List.iter (dump_stmt align decls false) r
      | [] -> ()
  
  and dump_stmt align decls only (sk, _) =
    print_string align;
    match sk with
	Set (lv, e, sc) ->
	  print_endline ((string_of_lval decls lv)^" =("^(string_of_scalar sc)^
			    ") "^(string_of_exp decls e)^";")
      | Copy (lv1, lv2, sz) ->
	  print_endline ((string_of_lval decls lv1)^" ="^(string_of_size_t sz)^
			    " "^(string_of_lval decls lv2)^";")
	    
      | Decl (name, t, body) ->
	  let new_decls = if !Npkcontext.pretty_print then (name::decls) else [] in
	    if only then begin
	      print_endline ((string_of_typ t)^" "^name^";");
	      dump_blk align new_decls body
	    end else begin
	      print_endline "{";
	      let new_align = align^"  " in
		print_string new_align;
		print_endline ((string_of_typ t)^" "^name^";");
		dump_blk new_align new_decls body;
		print_endline (align^"}")
	    end
	     
      | DoWith  (body, lbl, action) ->
	  print_endline "do {";
	  dump_blk (align^"  ") decls body;
	  print_endline (align^"} with lbl"^(string_of_int lbl)^": {");
	  dump_blk (align^"  ") decls action;
	  print_endline (align^"}")

      | Goto l ->
	  print_endline ("goto "^(string_of_lbl l)^";")
	    
      | Call f ->
	  print_endline ((string_of_fn decls f)^";")
	    
      | ChooseAssert elts ->
	  print_endline "choose {";
	  List.iter (dump_assertblk (align^"--> ") (align^"    ") decls) elts;
	  print_endline (align^"}")
	    
      | InfLoop body -> 
	  print_endline "while (1) {";
	  dump_blk (align^"  ") decls body;
	  print_endline (align^"}")
	    
  and dump_assert align decls e =
    print_endline (align^"assert("^(string_of_exp decls e)^");")
      
  and dump_assertblk align1 align2 decls (exps, b) =
    match exps, b with
      | [], [] -> Npkcontext.error "Newspeak.dump_assertblk" "Error in output"
      | [], hd::[] ->
	  dump_stmt align1 decls true hd
      | [], hd::r ->
	  dump_stmt align1 decls false hd;
	  List.iter (dump_stmt align2 decls false) r
	    
      | first::others, _ ->
	  dump_assert align1 decls first;
	  List.iter (dump_assert align2 decls) others;
	  dump_blk align2 decls b
  in

  let dump_init i =
    let dump_elt (o, s, e) =
      print_string ((string_of_size_t o)^": "^(string_of_scalar s)^" "
		     ^(string_of_exp [] e));
    in
    let rec dump_init l =
      match l with
	| [] -> ()
	| [e] -> dump_elt e
	| e::r ->
	    dump_elt e;
	    print_string ";";
	    dump_init r
    in
      match i with
	| None -> ()
	| Some i -> 
	    print_string " = {";
	    dump_init i;
	    print_endline "}"
  in
  
  let dump_fundec name body =
    match body with
	None -> ()
      | Some body ->
	  cur_fun := name;
	  lbl_index := 0;
	  print_endline (name^"() {");
	  dump_blk "  " [] body;
	  print_endline "}";
	  print_newline ()
  in

  let print_usedglbs title globs =
    print_endline title;
    Hashtbl.iter (fun x (_, _, _, used) -> if used then print_endline x) 
      globs;
    print_newline ()
  in

  let print_glob n (t, _, init, _) =
    let str = (string_of_typ t)^" "^n in
      match init with
	  None -> print_endline ("extern "^str^";")
	| Some i -> 
	    print_string str;
	    dump_init i;
	    print_endline ";"
  in

  let print_fundef n (_, pbody) =
    dump_fundec n pbody;
    if pbody <> None then print_newline ()
  in
    print_endline fname;

    print_usedglbs "Global used" globs;

    print_endline "Global variables";
    Hashtbl.iter print_glob globs;
    print_newline ();

    print_endline "Function definitions";
    Hashtbl.iter print_fundef funs

exception Uncomparable

(* TODO: this is no good recode. Careful. *)
let is_mp_typ t1 t2 =
  let rec is_mp_typs_aux t1 t2 =
    match (t1, t2) with
	(Scalar sc1, Scalar sc2) when sc1 = sc2 -> true

      | (Array (t1, None), Array (t2, Some _)) -> 
	  ignore (is_mp_typs_aux t1 t2);
	  false
      | (Array (t1, _), Array (t2, None)) ->
	  is_mp_typs_aux t1 t2

      | (Array (t1, Some l1), Array (t2, Some l2)) when l1 = l2 ->
	  (is_mp_typs_aux t1 t2)
    
      | (Region (f1, n1), Region (f2, n2)) when n1 = n2 ->
	  (is_mp_fields f1 f2)
	    
      | _ -> raise Uncomparable

  and is_mp_fields f1 f2 =
    match (f1, f2) with
	([], []) -> true
      | ((o1, t1)::f1, (o2, t2)::f2) when o1 = o2 ->
	  (is_mp_fields f1 f2) && (is_mp_typs_aux t1 t2)
      | _ -> raise Uncomparable
  in
    
    is_mp_typs_aux t1 t2

let compare_typs t1 t2 =
  let rec compare_typs_aux t1 t2 =
    match (t1, t2) with
	(Scalar sc1, Scalar sc2) -> sc1 = sc2

      | (Array (t1, None), Array (t2, _))
      | (Array (t1, _), Array (t2, None)) ->
	  compare_typs_aux t1 t2

      | (Array (t1, Some l1), Array (t2, Some l2)) ->
	  (compare_typs_aux t1 t2) && (l1 = l2)
    
      | (Region (f1, n1), Region (f2, n2)) ->
	  (compare_fields f1 f2) && (n1 = n2)
	    
      | _ -> false

  and compare_fields f1 f2 =
    match (f1, f2) with
	([], []) -> true
      | ((o1, t1)::f1, (o2, t2)::f2) ->
	  (compare_fields f1 f2)
	  && (o1 = o2) && (compare_typs_aux t1 t2)
      | _ -> false
  in
    
    compare_typs_aux t1 t2

let write out_name prog = 
  Npkcontext.print_debug ("Writing "^(out_name)^"...");
  let ch_out = open_out_bin out_name in
    Marshal.to_channel ch_out "NPKO" [];
    Marshal.to_channel ch_out prog [];
    close_out ch_out;
    Npkcontext.print_debug ("Writing done.")
    

let read_header fname =
  let cin = open_in_bin fname in
    Npkcontext.print_debug ("Importing "^fname^"...");
    let str = Marshal.from_channel cin in
      if str <> "NPKO" then begin 
	close_in cin;
	Npkcontext.error 
	  "Npkil.read_header" (fname^" is an invalid .npko file")
      end;
      let (srcname, globs, _) = Marshal.from_channel cin in
	Npkcontext.print_debug ("Importing done.");
	close_in cin;
	(srcname, globs)

let read_fundefs fname =
  let cin = open_in_bin fname in
    Npkcontext.print_debug ("Importing funs from "^fname^"...");
    let _ = Marshal.from_channel cin in
    let (_, _, funs) = Marshal.from_channel cin in
      Npkcontext.print_debug ("Funs import done.");
      close_in cin;
      funs

(* TODO: architecture dependent ?? *)
(* TODO: probably the best way to deal with this and all size problems
   is to set all these global constants, when a npk file is read ?? 
Some kind of data structure with all the sizes,
then function read returns this data structure too
and there is an init function *)
let char_typ = Int (Signed, Config.size_of_char)

let init_of_string str =
  let len = String.length str in
  let res = ref [(len, char_typ, exp_of_int 0)] in
    for i = len - 1 downto 0 do 
      let c = Char.code str.[i] in
	res := (i, char_typ, exp_of_int c)::!res
    done;
    (len + 1, Some !res)

let create_cstr str =
  (* TODO: see firstpass.ml, maybe this should not be in npkil! *)
  let fname = Npkcontext.get_fname () in
  let name = "!"^fname^".const_str_"^str in
  let (len, init) = init_of_string str in
  let t = Array (Scalar char_typ, Some len) in
    (name, (t, 
    (* TODO: code cleanup: not nice *)
    (fname, -1, -1), Some init, true))

let string_of_cast t1 t2 =
  match (t1, t2) with
      (Int _, Ptr) -> "from integer to pointer"
    | (Ptr, Int _) -> "from pointer to integer"
    | _ -> (string_of_scalar t1)^" -> "^(string_of_scalar t2)

(* TODO: code cleanup: this could be also used by cilcompiler ? *)
let cast t e t' =
    match (t, t') with
      _ when t = t' -> e
    | (Int _, Int k) -> make_int_coerce k e
    | (Int _, Ptr) when e = zero -> Const Nil
    | (Ptr, Int k) when !Npkcontext.castor_allowed -> 
	Npkcontext.print_warning "Compiler.cast"
	  ("Probable invalid cast "^(string_of_cast t t'));
	UnOp (PtrToInt k, e)
    | _ -> 
	Npkcontext.error "Compiler.cast"
	  ("Invalid cast "^(string_of_cast t t'))

let rec append_decls d body =
  match d with
      (x, t, loc)::tl -> (Decl (x, t, append_decls tl body), loc)::[]
    | [] -> body

let rec negate e =
  match e with
    | UnOp (Not, BinOp (Eq t, e1, e2)) -> BinOp (Eq t, e1, e2)
    | UnOp (Not, e) -> e
    | BinOp (Gt t, e1, e2) -> UnOp (Not, BinOp (Gt t, e1, e2))
    | BinOp (Eq t, e1, e2) -> UnOp (Not, BinOp (Eq t, e1, e2))
    | UnOp (Coerce i, e) -> UnOp (Coerce i, negate e)
    | _ -> UnOp (Not, e)
