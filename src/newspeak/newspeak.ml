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


(* TODO: remove global variables as much as possible *)

(*-------*)
(* Types *)
(*-------*)

type t = (gdecl list * (fid, fundec) Hashtbl.t)

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
(* TODO: maybe this was a really bad idea, go back to a number ?? *)
  | Global of string
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
  | CFloat of float * string
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
  | Gt of scalar_t
  | Eq of scalar_t

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

type size_of = typ -> size_t

type size_of_scalar = scalar_t -> size_t


(*-----------*)
(* Constants *)
(*-----------*)

let zero = Const (CInt64 (Int64.zero))
let zero_f = Const (CFloat (0., "0."))

let locUnknown = ("", -1, -1)



(*----------------------------------*)
(* Manipualtion and Simplifications *)
(*----------------------------------*)

let create_size_of ptr_sz =
  let size_of_scalar t =
    match t with
	Int (_, n) -> n
      | Float n -> n
      | Ptr -> ptr_sz
      | FunPtr -> ptr_sz
  in
  let rec size_of t =
    match t with
      | Scalar t -> size_of_scalar t
      | Array (t, n) -> (size_of t) * n
      | Region (_, n) -> n
  in
    (size_of_scalar, size_of)

let domain_of_typ (sign, size) =
    match sign, size with
      (Unsigned, 1) -> (Int64.zero, Int64.of_string "255")
    | (Signed, 1) -> (Int64.of_string "-128", Int64.of_string "127")
    | (Unsigned, 2) -> (Int64.zero, Int64.of_string "65535")
    | (Signed, 2) -> (Int64.of_string "-32768", Int64.of_string "32767")
    | (Unsigned, 4) -> (Int64.zero, Int64.of_string "4294967295")
    | (Signed, 4) -> (Int64.of_string "-2147483648", Int64.of_string "2147483647")
    | _ -> invalid_arg "Newspeak.domain_of_typ"


let rec negate exp =
  match exp with
    | UnOp (Not, BinOp (Eq t, e2, e1)) -> BinOp (Eq t, e1, e2)
    | UnOp (Not, e) -> e
    | BinOp (Gt t, e1, e2) -> UnOp (Not, BinOp (Gt t, e1, e2))
    | BinOp (Eq t, e1, e2) -> UnOp (Not, BinOp (Eq t, e2, e1))
    | UnOp (Coerce i, e) -> UnOp (Coerce i, negate e)
    | _ -> invalid_arg "Newspeak.negate"

let exp_of_int x = Const (CInt64 (Int64.of_int x))

let extract_cond_body lbl x =
  let rec extract x =
    match x with
	(ChooseAssert [(cond1::[], body); (cond2::[], [Goto lbl2, _])], _)::[] 
	  when lbl = lbl2 && cond1 = negate cond2 ->
	    cond1::(extract body)
      | [] -> []
      | _ -> raise Exit
  in
    try match x with
	hd::body -> (extract [hd], body)
      | [] -> ([], [])
    with Exit -> ([], x)
	  
(* TODO: use this in a pretty printer *)
let extract_while blk =
  match blk with
      (InfLoop body, loc)::((Label lbl, _)::_ as tl) -> 
	let (cond, body) = extract_cond_body lbl body in
	  Some (cond, body, loc, tl)
    | _ -> None

(*---------*)
(* Display *)
(*---------*)

(* Useful types, functions and variables *)

module String_map = 
  Map.Make (struct type t = string let compare = Pervasives.compare end)

module Int_map = 
  Map.Make (struct type t = int let compare = Pervasives.compare end)

let rec seq sep f l =
  match l with
    | [] -> ""
    | [e] -> f e
    | e::r -> (f e)^sep^(seq sep f r)
(*
let globals = Hashtbl.create 100
let globals_index = ref 0

let funs = ref (String_map.empty)
let cur_fun = ref ""

let lbls = ref (Int_map.empty)
let lbl_index = ref 0

let clear_tables () =
  funs := String_map.empty;
  lbls := Int_map.empty;
  Hashtbl.clear globals;
  globals_index := 0;
  lbl_index := 0
*)


(* Types *)

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
    | Array (t, sz) -> (string_of_typ t)^"["^(string_of_size_t sz)^"]"
    | Region (lst, sz) ->
	let res = ref "{ " in
	let string_of_elt (off, t) = 
	  res := !res^(string_of_typ t)^" "^(string_of_size_t off)^"; "
	in
	  List.iter string_of_elt lst;
	  !res^"}"^(string_of_size_t sz)

let string_of_ftyp (args, ret) = 
  let res = ref "" in 
    begin match args with
	hd::[] -> res := string_of_typ hd
      | hd::tl -> 
	  res := "("^(string_of_typ hd);
	  List.iter (fun x -> res := !res^", "^(string_of_typ x)) tl;
	  res := !res^")"
      | [] -> res := "void"
    end;
    res := !res^" -> ";
    begin match ret with
	None -> res := !res^"void"
      | Some t -> res := !res^(string_of_typ t)
    end;
    !res



(* Expressions *)
let string_of_cte c =
  match c with
      CInt64 c -> Int64.to_string c
    | CFloat (_, s) -> s
    | Nil -> "nil"
	
let string_of_unop op =
  match op with
      Belongs (l,u) ->
	"belongs["^(Int64.to_string l)^","^(Int64.to_string u)^"]"
    | Coerce (l,u) ->
	"coerce["^(Int64.to_string l)^","^(Int64.to_string u)^"]"
    | Cast (typ, typ') ->
	"("^(string_of_scalar typ')^" <= "^(string_of_scalar typ)^")"
    | Not -> "!"
    | BNot _ -> "~"
    | PtrToInt i -> "("^(string_of_scalar (Int i))^")"
	  
let string_of_binop neg op =
  match op with
    | Gt _ when neg -> ">="
    | Eq _ when neg -> "<>"
    | Gt _ -> ">"
    | Eq _ -> "=="
    | _ when neg ->
	invalid_arg "Newspeak.string_of_binop: unexpected negation"
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

(* TODO: add name of locals, this will simplify this *)
let rec string_of_lval lv =
  match lv with
      Local vid -> (string_of_int vid) ^ "-"
    | Global name -> name
    | Deref (e, sz) -> "["^(string_of_exp e)^"]"^(string_of_size_t sz)
    | Shift (lv, sh) -> (string_of_lval lv)^" + "^(string_of_exp sh)

and string_of_exp e =
  match e with
      Const c -> string_of_cte c
    | Lval (lv, t) -> (string_of_lval lv)^"_"^(string_of_scalar t)
    | AddrOf (lv, sz) -> "&_"^(string_of_size_t sz)^"("^(string_of_lval lv)^")"
    | AddrOfFun fid -> "&fun"^fid

    (* TODO: Check this ! *)
    (* Pretty printing for >= and != *)
    | UnOp (Not, BinOp (op, e1, e2)) (* when !pretty_print *) ->
	"("^(string_of_exp e2)^" "^(string_of_binop true op)^
	  " "^(string_of_exp e1)^")"

    | BinOp (op, e1, e2) ->
	"("^(string_of_exp e1)^" "^(string_of_binop false op)^
	  " "^(string_of_exp e2)^")"

    | UnOp (op, exp) -> (string_of_unop op)^" "^(string_of_exp exp)

	  
let string_of_fn f =
  match f with
      FunId fid -> fid^"()"
    | FunDeref (exp, (args_t, Some ret_t)) ->
	"["^(string_of_exp exp)^"]("^
	  (seq ", " string_of_typ args_t)^") -> "^(string_of_typ ret_t)
    | FunDeref (exp, (args_t, None)) ->
	"["^(string_of_exp exp)^"]("^(seq ", " string_of_typ args_t)^")"

let rec string_of_cond b =
  match b with
      [] -> "1"
    | e::[] -> string_of_exp e
    | e::b -> (string_of_exp e)^" & "^(string_of_cond b)

(* Actual dump *)
let string_of_lbl l = "lbl"^(string_of_int l)

let dump_gdecl (name, t, i) =
  let dump_elt (o, s, e) =
    print_string ((string_of_size_t o)^": "^(string_of_scalar s)^" "^(string_of_exp e));
  in
  let rec dump_init l =
    match l with
      | [] -> ();
      | [e] -> dump_elt e;
      | e::r ->
	  dump_elt e;
	  print_string ";";
	  dump_init r
  in
    print_string ((string_of_typ t)^" "^name);
    match i with
      | Zero -> print_endline " = 0;"
      | Init [] -> print_endline ";"
      | Init i -> 
	  print_string " = {";
	  dump_init i;
	  print_endline "};"

let string_of_blk offset x =
  let buf = Buffer.create 80 in
  let offset = ref offset in
  let incr_margin () = offset := !offset + 2 in
  let decr_margin () = offset := !offset - 2 in
  let dump_line str = 
    let margin = String.make !offset ' ' in
      Buffer.add_string buf (margin^str^"\n") 
  in

  let rec dump_stmt only (sk, _) =
    match sk with
	Set (lv, e, sc) ->
	  dump_line ((string_of_lval lv)^" =("^(string_of_scalar sc)^
			") "^(string_of_exp e)^";")
      | Copy (lv1, lv2, sz) ->
	  dump_line ((string_of_lval lv1)^" ="^(string_of_size_t sz)^
			" "^(string_of_lval lv2)^";")
	    
      | Decl (name, t, body) ->
	  if only then begin
	    dump_line ((string_of_typ t)^" "^name^";");
	    dump_blk body
	  end else begin
	    dump_line "{";
	    incr_margin ();
	    dump_line ((string_of_typ t)^" "^name^";");
	    dump_blk body;
	    decr_margin ();
	    dump_line "}"
	  end
	    
      | Label l -> dump_line ((string_of_lbl l)^":")
      | Goto l -> dump_line ("goto "^(string_of_lbl l)^";")
	  
      | Call f -> dump_line ((string_of_fn f)^";")
	  
      | ChooseAssert elts ->
	  dump_line "choose {";
	  incr_margin ();
	  List.iter dump_assertblk elts;
	  decr_margin ();
	  dump_line "}"

      | InfLoop body -> 
	  dump_line "while (1) {";
	  incr_margin ();
	  dump_blk body;
	  decr_margin ();
	  dump_line "}"

(* TODO: Should change this output!! *)
  and dump_assertblk (exps, body) =
    dump_line ("| "^(string_of_cond exps)^" -->");
    incr_margin ();
    dump_blk body;
    decr_margin ()

  and dump_blk b =
    match b with
      | hd::[] -> dump_stmt true hd
      | hd::r ->
	  dump_stmt false hd;
	  List.iter (dump_stmt false) r
      | [] -> ()
  in
    
    dump_blk x;
    Buffer.contents buf
  
let dump_fundec name body =
  match body with
      None -> ()
    | Some body ->
	print_endline (name^"() {");
	print_string (string_of_blk 2 body);
	print_endline "}";
	print_newline ()


(* Exported print functions *)
let dump (gdecls, fundecs) =
  (* TODO: Clean this mess... String_map *)
  let funs = ref (String_map.empty) in
    
    Hashtbl.iter 
    (fun name (_, body) -> funs := (String_map.add name body !funs))
    fundecs;
    String_map.iter dump_fundec !funs;
    List.iter dump_gdecl gdecls

let dump_fundec name (_, body) = dump_fundec name body

let string_of_blk x = string_of_blk 0 x

let string_of_stmt x = string_of_blk (x::[])

(* TODO: Implement two dumps, a pretty and an bare one *)

(* Input/output functions *)
let write_hdr cout (filenames, decls, ptr_sz) =
  Marshal.to_channel cout "NPK!" [];
  Marshal.to_channel cout filenames [];
  Marshal.to_channel cout ptr_sz [];
  Marshal.to_channel cout decls []
  
let write_fun cout f spec = Marshal.to_channel cout (f, spec) []

let write name (filenames, (decls, funs), ptr_sz) =
  let cout = open_out_bin name in
    write_hdr cout (filenames, decls, ptr_sz);
    Hashtbl.iter (write_fun cout) funs;
    close_out cout

let read name = 
  let cin = open_in_bin name in
  let str = Marshal.from_channel cin in
    if str <> "NPK!" 
    then invalid_arg ("Newspeak.read: "^name^" is not an .npk file");
    let filenames = Marshal.from_channel cin in
    let ptr_sz = Marshal.from_channel cin in
    let decls = Marshal.from_channel cin in
    let funs = Hashtbl.create 100 in
      begin try 
	while true do
	  let (f, spec) = Marshal.from_channel cin in
	    Hashtbl.add funs f spec
	done
      with End_of_file -> ()
      end;
      close_in cin;
      (filenames, (decls, funs), ptr_sz)

(** Simplifications of coerces and belongs in [make_belongs] and [make_int_coerce]:
    - Coerce \[a;b\] Coerce \[c;d\] e -> Coerce \[a;b\] if \[c;d\] contains \[a;b\]
    - Coerce \[a;b\] Coerce \[c;d\] e -> Coerce \[c;d\] if \[a;b\] contains \[c;d\]
    - Coerce/belongs \[a;b\] (const c) becomes const c if c in \[a;b\]

    Precondition: all Coerce (l,u) verify l <= u *)

(* TODO: architecture dependent ?? *)
(* TODO: probably the best way to deal with this and all size problems
   is to set all these global constants, when a npk file is read ?? 
Some kind of data structure with all the sizes,
then function read returns this data structure too
and there is an init function *)
let char_sz = 1
let char_typ = Int (Signed, char_sz)

let init_of_string str =
  let len = String.length str in
  let res = ref [(len, char_typ, exp_of_int 0)] in
    for i = len - 1 downto 0 do 
      let c = Char.code (String.get str i) in
	res := (i, char_typ, exp_of_int c)::!res
    done;
    (len + 1, Init !res)

let create_cstr name str =
  let (len, init) = init_of_string str in
  let t = Array (Scalar char_typ, len) in
    (name, t, init)

let build_call_aux f prolog (args_t, ret_t) =
  let call = ref (prolog@[Call (FunId f), locUnknown]) in
  let i = ref 0 in
  let handle_arg t =
    call := [Decl ("arg"^(string_of_int !i), t, !call), locUnknown];
    incr i
  in
    List.iter handle_arg args_t;
    begin match ret_t with
	None -> ()
      | Some t -> call := [Decl ("value_of_"^f, t, !call), locUnknown]
    end;
    !call

let build_call f ftyp = build_call_aux f [] ftyp

let build_main_call ptr_sz (args_t, ret_t) args =
  let argv_name = "!ptr_array" in
  let n = ref 0 in
  let globs = ref [] in
  let ptr_array_init = ref [] in
  let handle_arg_aux str =
    let name = "!param_str"^(string_of_int (!n + 1)) in
    let (len, init) = init_of_string str in
      globs := (name, Array (Scalar char_typ, len), init)::(!globs);
      ptr_array_init := 
	(!n * ptr_sz, Ptr, AddrOf (Global name, len))::(!ptr_array_init);
      incr n
  in
  let handle_args () =
    List.iter handle_arg_aux args;
    let ptr_array = 
      (argv_name, Array (Scalar Ptr, !n), Init !ptr_array_init) 
    in
    globs := List.rev (ptr_array::(!globs))
  in
  let prolog =
    match args_t with
	[Scalar Int k; Scalar Ptr] -> 
	  handle_args ();
	  let set_argv = 
	    Set (Local 1, AddrOf (Global argv_name, !n*ptr_sz), Ptr) 
	  in
	  let set_argc = Set (Local 0, exp_of_int !n, Int k) in
	    (set_argv, locUnknown)::(set_argc, locUnknown)::[]
      | [] -> []
      | _ -> invalid_arg "Newspeak.build_main_call: invalid type for main"
  in
    (!globs, build_call_aux "main" prolog (args_t, ret_t))



class simplification =
object
  method process_exp (x: exp) = x
  method process_blk (x: blk) = x
end;;

let contains (l1, u1) (l2, u2) = 
  (Int64.compare l1 l2 <= 0) && (Int64.compare u2 u1 <= 0)

class simplify_coerce =
object 
  inherit simplification

  method process_exp e =
    match e with
        (* Coerce [a;b] Coerce [c;d] e 
	   -> Coerce [c;d] if [a;b] contains [c;d] *)
      | UnOp (Coerce r1, UnOp (Coerce r2, e)) when contains r1 r2 -> 
	  UnOp (Coerce r2, e)
	  
      (* Coerce [a;b] Coerce [c;d] e -> Coerce [a;b] if [c;d] contains [a;b] *)
      | UnOp (Coerce r1, UnOp (Coerce r2, e)) when contains r2 r1 -> 
	  UnOp (Coerce r1, e)

      (* Coerce/Belongs [a;b] Const c -> Const c if c in [a;b] *)
      | UnOp (Coerce (l1,u1), Const (CInt64 c))
      | UnOp (Belongs (l1,u1), Const (CInt64 c))
          when Int64.compare l1 c <= 0 && Int64.compare c u1 <= 0 ->
          Const (CInt64 c)

      (* Coerce/Belongs [a;b] Lval (lv, t) -> Lval (lv, t)
	 if [a; b] contains dom(t) *)
      | UnOp (Coerce r, (Lval (_, Int k) as lv))
      | UnOp (Belongs r, (Lval (_, Int k) as lv)) 
	  when contains r (domain_of_typ k) -> lv

      | _ -> e

end;;

class simplify_choose =
object 
  inherit simplification

  method process_blk x =
    match x with
	(ChooseAssert [([], body)], _)::tl -> body@tl
      | (ChooseAssert _, _)::_ -> x
      | _ -> x
end;;

let simplify_gotos blk =
  let necessary_lbls = ref [] in
  let rec simplify_blk x =
    match x with
	(Goto l1, _)::((Label l2, _)::_ as tl) when l1 = l2 -> simplify_blk tl
      | [] -> []
      | (Label l, _)::tl when not (List.mem l !necessary_lbls) -> 
	  simplify_blk tl
      | hd::tl -> 
	  let hd = simplify_stmt hd in
	  let tl = simplify_blk tl in
	    hd::tl
		
  and simplify_choose_elt (l, b) = (l, simplify_blk b)

  and simplify_stmt (x, loc) =
    match x with
	Goto l -> 
	  necessary_lbls := l::!necessary_lbls;
	  (x, loc)
      | Decl (name, t, body) -> (Decl (name, t, simplify_blk body), loc)
      | ChooseAssert elts ->
	  (ChooseAssert (List.map simplify_choose_elt elts), loc)
      | InfLoop body -> (InfLoop (simplify_blk body), loc)
      | _ -> (x, loc)
  in
    simplify_blk blk


let simplify_aux actions blk =
  let rec simplify_stmt (x, loc) =
    let x =
      match x with
	| Set (lv, e, sca) -> Set (simplify_lval lv, simplify_exp e, sca)
	| Call (FunDeref (e, t)) -> Call (FunDeref (simplify_exp e, t))
	| Decl (name, t, body) -> Decl (name, t, simplify_blk body)
	| ChooseAssert elts ->
            let elts = List.map simplify_choose_elt elts in
	      ChooseAssert (elts)
	| InfLoop body ->
            let body = simplify_blk body in
	      InfLoop body
	| _ -> x
    in
      (x, loc)

  and simplify_choose_elt (cond, body) = 
    let cond = List.map simplify_exp cond in
    let body = simplify_blk body in
      (cond, body)

  and simplify_exp e =
    let e = ref e in
      List.iter (fun x -> e := x#process_exp !e) actions;
      match !e with
	  Lval (lv, sca) -> Lval (simplify_lval lv, sca)
	| AddrOf (lv, sz) -> AddrOf (simplify_lval lv, sz)
	| UnOp (o, e) -> UnOp (o, simplify_exp e)
	| BinOp (o, e1, e2) -> BinOp (o, simplify_exp e1, simplify_exp e2)
	| _ -> !e

  and simplify_lval lv =
    match lv with
      | Deref (e, sz) -> Deref (simplify_exp e, sz)
      | Shift (l, e) -> Shift (simplify_lval l, simplify_exp e)
      | _ -> lv
	  
  and simplify_blk blk =
    let blk = ref blk in
      List.iter (fun x -> blk := x#process_blk !blk) actions;
      match !blk with
	  hd::tl -> 
	    let hd = simplify_stmt hd in
	    let tl = simplify_blk tl in
	      hd::tl
	| [] -> []
  in
    simplify_blk blk


(* TODO: do this in one tree traversal, instead of 2 *)
let simplify b = 
  simplify_aux [new simplify_choose; new simplify_coerce] (simplify_gotos b)


type c_prog = (c_typedef list * c_glob list * c_fun list)

and c_typedef = ()

and c_glob = (string * c_typ * c_init)

and c_typ =
    | C_scalar of scalar_t
    | C_array of (c_typ * int)
    | C_struct of c_field list
    | C_union of c_field list

and c_ftyp = c_typ list * c_typ option

and c_field = (int * c_typ)

and c_init = ()

and c_fun = (string * c_ftyp * c_blk option)

and c_blk = c_stmt list

and c_stmt = 
    | C_set of (c_lv * c_exp)
    | C_decl of (string * c_typ * c_blk)
    | C_ite of (c_exp * c_blk * c_blk)

and c_lv = 
    | C_var of string

and c_exp =
    | C_const of cte
    | C_lval of c_lv
    | C_binop of (binop * c_exp * c_exp)

let successive_fields x = 
  let rec check o x =
    match x with
	(o', _)::tl when o < o' -> check o' tl
      | [] -> true
      | _ -> false
  in
    check 0 x

let aligned_fields x = List.for_all (fun (o, _) -> o = 0) x

let npk_to_c (gdecls, fundecs) = 
  let type_tbl = Hashtbl.create 100 in
  let field_cnt = ref 0 in
  let typedefs = ref [] in

  let c_fundecs = ref [] in
    
  let rec typ_to_c t =
    if Hashtbl.mem type_tbl t then Hashtbl.find type_tbl t
    else begin
      match t with
	  Scalar sc -> C_scalar sc
	| Array (t, n) -> C_array (typ_to_c t, n)
	| Region (fields, _) when successive_fields fields -> 
	    C_struct (List.map field_to_c fields)
	| Region (fields, _) when aligned_fields fields ->
	    C_union (List.map field_to_c fields)
	| _ -> 
	    invalid_arg ("Newspeak.npk_to_c.typ_to_c: "
			  ^"decompilation of newspeak type to C not possible")
    end

  and field_to_c (o, t) =
    let name = !field_cnt in
    let t = typ_to_c t in
      incr field_cnt;
      (name, t)
  in

  let init_t_to_c i = 
    match i with
	Zero -> ()
      | _ -> invalid_arg "Newspeak.npk_to_c.init_t_to_c: not implemented yet" 
  in
    
  let gdecl_to_c (string, t, init) =
    let t = typ_to_c t in
    let init = init_t_to_c init in
      (string, t, init)
  in

  let ftyp_to_c (args, ret) = 
    let args = List.map typ_to_c args in
    let ret = 
      match ret with
	| Some t -> Some (typ_to_c t)
	| None -> None
    in
      (args, ret)
  in

  let rec exp_to_c env e = 
    match e with
	Const c -> C_const c
      | Lval (lv, _) -> C_lval (lv_to_c env lv)
      | BinOp (op, e1, e2) -> C_binop (op, exp_to_c env e1, exp_to_c env e2)
      | _ -> invalid_arg ("Newspeak.npk_to_c.exp_to_c: not implemented yet: "
			   ^(string_of_exp e))

  and lv_to_c env lv =
    match lv with
	Local n -> C_var (List.nth env n)
      | _ -> invalid_arg ("Newspeak.npk_to_c.lv_to_c: not implemented yet: "
			   ^(string_of_lval lv))
  in
    
  let rec blk_to_c env x = List.map (stmt_to_c env) x

  and stmt_to_c env (x, loc) =
    match x with
	Set (lv, e, _) -> 
	  let lv = lv_to_c env lv in
	  let e = exp_to_c env e in
	    C_set (lv, e)
      | Decl (name, t, body) ->
	  let t = typ_to_c t in
	  let env' = name::env in
	  let body = blk_to_c env' body in
	    C_decl (name, t, body)
      | ChooseAssert [([e1], body1); ([e2], body2)]
	when e2 = negate e1 ->
	  let e = exp_to_c env e1 in
	  let body1 = blk_to_c env body1 in
	  let body2 = blk_to_c env body2 in
	    C_ite (e, body1, body2)
      | _ -> 
	  let stmt = string_of_stmt (x, loc) in
	    invalid_arg ("Newspeak.npk_to_c.stmt_to_c: not implemented yet: "
			  ^stmt)
  in

  let fundec_to_c fid (ftyp, body) = 
    let ftyp = ftyp_to_c ftyp in
    let body =
      match body with
	| Some body -> Some (blk_to_c [] body)
	| None -> None
    in
      c_fundecs := (fid, ftyp, body)::(!c_fundecs)
  in

  let gdecls = List.map gdecl_to_c gdecls in
    Hashtbl.iter fundec_to_c fundecs;
    (!typedefs, gdecls, !c_fundecs)

let print_c (typedefs, gdecls, fundecs) =
  let string_of_scalar x =
    match x with
	Int (Signed, 4) -> "int"
      | _ -> 
	  invalid_arg "Newspeak.print_c.string_of_scalar: not implemented yet"
  in

  let string_of_c_typ t = 
    match t with
      | C_scalar sc -> string_of_scalar sc 
      | _ -> 
	  invalid_arg "Newspeak.print_c.string_of_c_typ: not implemented yet"
  in

  let print_glob (name, t, _)  =
    let t = string_of_c_typ t in
      print_endline (t^" "^name^";")
  in

  let string_of_args args =
    match args with
	[] ->  ""
      | _ -> invalid_arg "Newpeak.print_c.string_of_args: not implemented yet"
  in

  let string_of_fid f (args, ret) =
    let args = string_of_args args in
    let ret =
      match ret with
	  None -> "void"
	| _ -> invalid_arg "Newpeak.print_c.string_of_fid: not implemented yet"
    in
      ret^" "^f^"("^args^")"
  in

  let string_of_cte c =
    match c with
	CInt64 c -> Int64.to_string c
      | CFloat (_, s) -> s
      | Nil -> "0"
  in

  let rec string_of_lv lv =
    match lv with
	C_var name -> name

  and string_of_exp e =
    match e with
	C_const c -> string_of_cte c
      | C_lval lv -> string_of_lv lv
      | C_binop (op, e1, e2) ->
	  let e1 = string_of_exp e1 in
	  let e2 = string_of_exp e2 in
	  let op = string_of_binop false op in
	    "("^e1^" "^op^" "^e2^")"
  in

  let rec print_stmt x =
    match x with
      | C_set (lv, e) ->
	  let lv = string_of_lv lv in
	  let e = string_of_exp e in
	    print_endline (lv^" = "^e^";")
      | C_decl (name, t, body) ->
	  let t = string_of_c_typ t in
	    print_endline (t^" "^name^";");
	    print_blk body
      | C_ite (e, body1, body2) ->
	  let e = string_of_exp e in
	    print_endline ("if "^e^" {");
	    print_blk body1;
	    print_endline ("} else { ");
	    print_blk body2;
	    print_endline "}"
	      
  and print_blk blk = List.iter print_stmt blk in
    
  let print_fun (f, t, body) =
    let f = string_of_fid f t in 
      match body with
	  Some body -> 
	    print_endline (f^" {");
	    print_blk body;
	    print_endline "}"
	| None -> print_endline (f^";")
  in

    List.iter print_glob gdecls;
    List.iter print_fun fundecs
  

let dump_as_C prog =
  let c_prog = npk_to_c prog in
    print_c c_prog
(*
and stmtkind =
  | Copy of (lval * lval * size_t)
  | Label of lbl
  | Goto of lbl
  | Call of fn
  | ChooseAssert of (exp list * blk) list
  | InfLoop of blk

and stmt = stmtkind * location

and blk = stmt list

and lval =
  | Global of string
  | Deref of (exp * size_t)
  | Shift of (lval * exp)

and exp =
  | Lval of (lval * scalar_t)
  | AddrOf of (lval * size_t)
  | AddrOfFun of fid
  | UnOp of (unop * exp)
  | BinOp of (binop * exp * exp)

and cte = 
    CInt64 of Int64.t
  (* TODO: warning floats with more than 64 bits can not be represented *)
  | CFloat of float * string
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
  | Gt of scalar_t
  | Eq of scalar_t

and fn =
    FunId of fid
  | FunDeref of (exp * ftyp)

and field = offset * typ

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

type size_of = typ -> size_t

type size_of_scalar = scalar_t -> size_t
*)
(*
let dump_as_c (gdecls, fundecs) =
  (* TODO: Clean this mess... String_map *)
  let funs = ref (String_map.empty) in
  let add_fun name (_, body) =
    funs := String_map.add name body !funs
  in
    Hashtbl.iter add_fun fundecs;
    List.iter dump_gdecl_as_c gdecls
    String_map.iter dump_fundec !funs;
*)

(* Needs to find the type of structures to be able to make assignment first,
   and what about offsets in structures ?? *)
