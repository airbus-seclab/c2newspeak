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

(* TODO: put this in another file?? *)
module Nat =
struct
  type t = string
      
  let zero = "0"
  let one = "1"
  let of_string x = x
  let of_int x = string_of_int x
  let of_big_int = Big_int.string_of_big_int
  let to_big_int = Big_int.big_int_of_string

  let apply_big_int_op op x y =
    let x = Big_int.big_int_of_string x in
    let y = Big_int.big_int_of_string y in
    let z = op x y in
      Big_int.string_of_big_int z

  let add = apply_big_int_op Big_int.add_big_int

  let sub = apply_big_int_op Big_int.sub_big_int

  let mul = apply_big_int_op Big_int.mult_big_int

  let div = apply_big_int_op Big_int.div_big_int

  let neg x = 
    let x = Big_int.big_int_of_string x in
    let y = Big_int.minus_big_int x in
      Big_int.string_of_big_int y

  let add_int i x = 
    let x = Big_int.big_int_of_string x in
    let y = Big_int.add_int_big_int i x in
      Big_int.string_of_big_int y

  let mul_int i x = 
    let x = Big_int.big_int_of_string x in
    let y = Big_int.mult_int_big_int i x in
      Big_int.string_of_big_int y

  let compare x y = 
    let x = Big_int.big_int_of_string x in
    let y = Big_int.big_int_of_string y in
      Big_int.compare_big_int x y

  let to_string x = x
end

type t = (file list * prog * size_t)

and prog = globals * (fid, fundec) Hashtbl.t * specs

and globals = (string, gdecl) Hashtbl.t

and gdecl = typ * init_t

and fundec = ftyp * blk option

and specs = assertion list

and assertion = spec_token list

and spec_token =
    | SymbolToken of char
    | IdentToken of string
    | VarToken of string
    | CstToken of cte

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
    CInt of Nat.t
  (* TODO: warning floats with more than 64 bits can not be represented *)
  | CFloat of (float * string)
  | Nil

and unop =
    Belongs of bounds
  (* TODO: warning coercion to unsigned long long can not be represented *)
  | Coerce of bounds
  | Not
  | BNot of bounds
  | PtrToInt of ikind
  | IntToPtr of ikind
  | Cast of (scalar_t * scalar_t)

and binop =
  | PlusI | MinusI | MultI | DivI | Mod
  | PlusF of size_t | MinusF of size_t | MultF of size_t | DivF of size_t
  | BOr of bounds | BAnd of bounds | BXor of bounds
  | Shiftlt | Shiftrt
  | PlusPI
  | MinusPP
  | Gt of scalar_t
  | Eq of scalar_t

and fn =
    FunId of fid
  | FunDeref of (exp * ftyp)

and typ =
    Scalar of scalar_t
  | Array of (typ * length)
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
and file = string

and ikind = sign_t * size_t
and sign_t = Signed | Unsigned
(* a size in number of bits *)
and length = int
and offset = int
and size_t = int
and bounds = (Nat.t * Nat.t)

and location = string * int * int

let belongs c (l, u) = (Nat.compare l c <= 0) && (Nat.compare c u <= 0)

let contains (l1, u1) (l2, u2) = 
  (Nat.compare l1 l2 <= 0) && (Nat.compare u2 u1 <= 0)

(*-----------*)
(* Constants *)
(*-----------*)

let zero = Const (CInt Nat.zero)
let one = Const (CInt Nat.one)
let zero_f = Const (CFloat (0., "0."))



(*----------------------------------*)
(* Manipulation and Simplifications *)
(*----------------------------------*)
	
let size_of_scalar ptr_sz t = 
  match t with
      Int (_, n) -> n
    | Float n -> n
    | Ptr -> ptr_sz
    | FunPtr -> ptr_sz

let size_of ptr_sz t =
  let rec size_of t =
    match t with
      | Scalar t -> size_of_scalar ptr_sz t
      | Array (t, n) -> (size_of t) * n
      | Region (_, n) -> n
  in
    size_of t

let domain_of_typ (sign, size) =
    match (sign, size) with
      (Unsigned, 8) -> (Nat.zero, Nat.of_string "255")
    | (Signed, 8) -> (Nat.of_string "-128", Nat.of_string "127")
    | (Unsigned, 16) -> (Nat.zero, Nat.of_string "65535")
    | (Signed, 16) -> (Nat.of_string "-32768", Nat.of_string "32767")
    | (Unsigned, 32) -> (Nat.zero, Nat.of_string "4294967295")
    | (Signed, 32) -> (Nat.of_string "-2147483648", Nat.of_string "2147483647")
    | (Signed, 64) -> 
	(Nat.of_string "-9223372036854775808", 
	Nat.of_string "9223372036854775807")
    | (Unsigned, 64) -> (Nat.zero, Nat.of_string "18446744073709551615")
(* For bitfields *)
    | (Signed, n) when n < 64 ->
	let x = Int64.shift_left Int64.one (n-1) in
	let l = Int64.to_string (Int64.neg x) in
	let u = Int64.to_string (Int64.pred x) in
	  (Nat.of_string l, Nat.of_string u)
    | (Unsigned, n) when n < 64 ->
	let x = Int64.pred (Int64.shift_left Int64.one n) in
	  (Nat.zero, Nat.of_string (Int64.to_string x))
    | _ -> invalid_arg "Newspeak.domain_of_typ"

let rec negate exp =
  match exp with
    | UnOp (Not, BinOp (Eq t, e2, e1)) -> BinOp (Eq t, e1, e2)
    | UnOp (Not, e) -> e
    | BinOp (Gt t, e1, e2) -> UnOp (Not, BinOp (Gt t, e1, e2))
    | BinOp (Eq t, e1, e2) -> UnOp (Not, BinOp (Eq t, e1, e2))
    | UnOp (Coerce i, e) -> UnOp (Coerce i, negate e)
    | _ -> invalid_arg "Newspeak.negate"

let exp_of_int x = Const (CInt (Nat.of_int x))

type alt_stmtkind =
    (* the condition is a list of expression separated by && 
       Careful! It behaves like the C operator: 
       if the first expression evaluates to false, evaluation stops. *)
    | While of (exp list * blk)
    | Npk of stmtkind

type alt_blk = (alt_stmtkind * location) list

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

let rec convert_loops _ = invalid_arg "Not implemented yet"
(*
  match blk with
      (InfLoop body, loc)::(Label lbl, _)::tl ->
	let (cond, body) = extract_cond_body lbl body in
	  (While (cond, body), loc)::(convert_loops tl)
    | (x, loc)::tl -> (Npk x, loc)::(convert_loops tl)
    | [] -> []
*)

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


(* Types *)

let string_of_size_t = string_of_int

let string_of_sign_t sg =
  match sg with
      Unsigned -> "u"
    | Signed -> ""

let string_of_scalar s =
  match s with
      Int (sg, sz) -> (string_of_sign_t sg)^"int"^(string_of_size_t sz)
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


let string_of_loc (fname, line, carac) = 
  if (fname = "") then invalid_arg "Newspeak.string_of_loc: unknown location";
  if (line = -1) && (carac = -1) then fname
  else if (line >= 0) && (carac >= 0) 
  then (fname^":"^(string_of_int line)^"#"^(string_of_int carac))
  else invalid_arg "Newspeak.string_of_loc: invalid location"
  
  
let dummy_loc fname = 
  if fname = "" 
  then invalid_arg "Newspeak.dummy_loc: invalid function name for location";
  (fname, -1, -1)

let unknown_loc = ("", -1, -1)


(* Expressions *)
let string_of_cte c =
  match c with
      CInt c -> Nat.to_string c
    | CFloat (_, s) -> s
    | Nil -> "nil"
	
let string_of_bounds (l, u) = "["^(Nat.to_string l)^","^(Nat.to_string u)^"]"

let string_of_unop op =
  match op with
      Belongs r -> "belongs"^(string_of_bounds r)
    | Coerce r -> "coerce"^(string_of_bounds r)
    | Cast (typ, typ') ->
	"("^(string_of_scalar typ')^" <= "^(string_of_scalar typ)^")"
    | Not -> "!"
    | BNot _ -> "~"
    | PtrToInt i -> "("^(string_of_scalar (Int i))^")"
    | IntToPtr _ -> "(ptr)"
	  
let string_of_binop op =
  match op with
    | Gt _ -> ">"
    | Eq t -> "==_"^(string_of_scalar t)
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
    | AddrOfFun fid -> "&_fun("^fid^")"

    | BinOp (op, e1, e2) ->
	"("^(string_of_exp e1)^" "^(string_of_binop op)^
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

let dump_gdecl name (t, i) =
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
  let dump_line_at loc str =
    let loc = if loc = unknown_loc then "" else "("^(string_of_loc loc)^")^" in
    let margin = String.make !offset ' ' in
      Buffer.add_string buf (margin^loc^str^"\n") 
  in

  let rec dump_stmt only (sk, loc) =
    match sk with
	Set (lv, e, sc) ->
	  dump_line_at loc ((string_of_lval lv)^" =("^(string_of_scalar sc)^
			") "^(string_of_exp e)^";")
      | Copy (lv1, lv2, sz) ->
	  dump_line_at loc ((string_of_lval lv1)^" ="^(string_of_size_t sz)^
			" "^(string_of_lval lv2)^";")
	    
      | Decl (_, t, body) ->
	  if only then begin
	    dump_line_at loc ((string_of_typ t)^";");
	    dump_blk body
	  end else begin
	    dump_line_at loc "{";
	    incr_margin ();
	    dump_line ((string_of_typ t)^";");
	    dump_blk body;
	    decr_margin ();
	    dump_line "}"
	  end
	    
      | DoWith (body, lbl, action) ->
	  dump_line_at loc "do {";
	  incr_margin ();
	  dump_blk body;
	  decr_margin ();
	  dump_line ("} with lbl"^(string_of_int lbl)^": {");
	  incr_margin ();
	  dump_blk action;
	  decr_margin ();
	  dump_line "}"
      | Goto l -> dump_line_at loc ("goto "^(string_of_lbl l)^";")
	  
      | Call f -> dump_line_at loc ((string_of_fn f)^";")
	  
      | ChooseAssert elts ->
	  dump_line_at loc "choose {";
	  incr_margin ();
	  List.iter dump_assertblk elts;
	  decr_margin ();
	  dump_line "}"

      | InfLoop body -> 
	  dump_line_at loc "while (1) {";
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


let dump_globals gdecls = 
  (* TODO: Clean this mess... String_map *)
  let glbs = ref (String_map.empty) in
    Hashtbl.iter 
      (fun name info -> glbs := (String_map.add name info !glbs)) 
      gdecls;
    String_map.iter dump_gdecl !glbs
      
let dump_token x = 
  let t =
    match x with
	SymbolToken x -> String.make 1 x
      | IdentToken x -> x
      | VarToken x -> "'"^x^"'"
      | CstToken c -> string_of_cte c
  in
    print_string (t^" ")

let dump_assertion x = 
  List.iter dump_token x;
  print_newline ()

(* Exported print functions *)
let dump_prog (gdecls, fundecs, spec) =
  (* TODO: Clean this mess... String_map *)
  let funs = ref (String_map.empty) in
    Hashtbl.iter 
      (fun name (_, body) -> funs := (String_map.add name body !funs))
      fundecs;
    String_map.iter dump_fundec !funs;
    dump_globals gdecls;
    List.iter dump_assertion spec

let dump (fnames, prog, _) =
  List.iter (fun x -> print_endline x) fnames;
  dump_prog prog

let dump_fundec name (_, body) = dump_fundec name body

let string_of_blk x = string_of_blk 0 x

let string_of_stmt x = string_of_blk (x::[])

(* TODO: Implement two dumps, a pretty and an bare one
   implement the pretty one in a separate utility: npkpretty *)

(* Input/output functions *)
let write_hdr cout (filenames, decls, specs, ptr_sz) =
  Marshal.to_channel cout "NPK!" [];
  Marshal.to_channel cout Version.version [];
  Marshal.to_channel cout Version.revision [];
  Marshal.to_channel cout filenames [];
  Marshal.to_channel cout ptr_sz [];
  Marshal.to_channel cout decls [];
  Marshal.to_channel cout specs []
  
let write_fun cout f spec = Marshal.to_channel cout (f, spec) []

let write name (filenames, (decls, funs, specs), ptr_sz) =
  let cout = open_out_bin name in
    write_hdr cout (filenames, decls, specs, ptr_sz);
    Hashtbl.iter (write_fun cout) funs;
    close_out cout

let read name = 
  let cin = open_in_bin name in
  let str = Marshal.from_channel cin in
    if str <> "NPK!" 
    then invalid_arg ("Newspeak.read: "^name^" is not an .npk file");
    let version = Marshal.from_channel cin in
    let revision = Marshal.from_channel cin in
      if (version <> Version.version) 
	|| (revision <> Version.revision) then begin
	invalid_arg ("Newspeak.read: this file was generated with a "
		      ^"different version of c2newspeak. "
		      ^"Please regenerate your file or install the latest "
		      ^"version of newspeak."^
		      " Operation aborted.");
      end;
      let filenames = Marshal.from_channel cin in
      let ptr_sz = Marshal.from_channel cin in
      let decls = Marshal.from_channel cin in
      let specs = Marshal.from_channel cin in
      let funs = Hashtbl.create 100 in
	begin try 
	    while true do
	      let (f, spec) = Marshal.from_channel cin in
		Hashtbl.add funs f spec
	    done
	  with End_of_file -> ()
	end;
	close_in cin;
	(filenames, (decls, funs, specs), ptr_sz)
	    
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
let char_sz = 8
let char_typ = Int (Signed, char_sz)

let init_of_string str =
  let len = String.length str in
  let res = ref [(len*char_sz, char_typ, exp_of_int 0)] in
    for i = len - 1 downto 0 do 
      let c = Char.code (String.get str i) in
	res := (i*char_sz, char_typ, exp_of_int c)::!res
    done;
    (len + 1, !res)

let create_cstr name str =
  let (len, init) = init_of_string str in
  let t = Array (Scalar char_typ, len) in
    (name, (t, Init init))

let bind_var x t body =
  let rec bind_in_lval n lv =
    match lv with
	Local x -> Local x
      | Global y when x = y -> Local n
      | Deref (e, sz) -> Deref (bind_in_exp n e, sz)
      | Shift (lv, e) -> Shift (bind_in_lval n lv, bind_in_exp n e)
      | _ -> lv

  and bind_in_exp n e =
    match e with
	Lval (lv, t) -> Lval (bind_in_lval n lv, t)
      | AddrOf (lv, sz) -> AddrOf (bind_in_lval n lv, sz)
      | UnOp (op, e) -> UnOp (op, bind_in_exp n e)
      | BinOp (op, e1, e2) -> BinOp (op, bind_in_exp n e1, bind_in_exp n e2)
      | _ -> e
  in

  let rec bind_in_blk n x = List.map (bind_in_stmt n) x

  and bind_in_stmt n (x, loc) = (bind_in_stmtkind n x, loc)

  and bind_in_stmtkind n x =
    match x with
	Set (lv, e, t) ->
	  let lv = bind_in_lval n lv in
	  let e = bind_in_exp n e in
	    Set (lv, e, t)
      | Copy (lv1, lv2, sz) ->
	  let lv1 = bind_in_lval n lv1 in
	  let lv2 = bind_in_lval n lv2 in
	    Copy (lv1, lv2, sz)
      | Decl (x, t, body) -> 
	  let body = bind_in_blk (n+1) body in
	    Decl (x, t, body)
      | ChooseAssert choices -> 
	  let choices = List.map (bind_in_choice n) choices in
	    ChooseAssert choices
      | InfLoop body -> 
	  let body = bind_in_blk n body in
	    InfLoop body
      | DoWith (body, lbl, action) -> 
	  let body = bind_in_blk n body in
	  let action = bind_in_blk n action in
	    DoWith (body, lbl, action)
      | Call (FunDeref (e, t)) ->
	  let e = bind_in_exp n e in
	    Call (FunDeref (e, t))
      | _ -> x

  and bind_in_choice n (b, body) =
    let b = List.map (bind_in_exp n) b in
    let body = bind_in_blk n body in
      (b, body)
  in
    
  let body = bind_in_blk 0 body in
    Decl (x, t, body)    
  
class builder =
object
  val mutable curloc = unknown_loc
  method set_curloc loc = curloc <- loc
  method curloc = curloc
  method process_global (_: string) (x: gdecl) = x
  method process_lval (x: lval) = x
  method process_exp (x: exp) = x
  method process_blk (x: blk) = x
  method process_size_t (x: size_t) = x
end

class simplify_coerce =
object 
  inherit builder

(* TODO: put these theorems in Newspeak semantics paper *)
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
      | UnOp (Coerce r, Const (CInt c)) 
      | UnOp (Belongs r, Const (CInt c)) when belongs c r ->
          Const (CInt c)

      (* Coerce/Belongs [a;b] Lval (lv, t) -> Lval (lv, t)
	 if [a; b] contains dom(t) *)
      | UnOp (Coerce r, (Lval (_, Int k) as lv))
      | UnOp (Belongs r, (Lval (_, Int k) as lv))
	  when contains r (domain_of_typ k) -> lv

      | _ -> e
end

class simplify_ptr =
object
  inherit builder

  method process_lval lv =
    match lv with
	Deref (AddrOf (lv, n), n') when n' <= n -> lv
      | _ -> lv
end

let nat_op op =
  match op with
      PlusI -> Nat.add
    | MinusI -> Nat.sub
    | MultI -> Nat.mul
    | _ -> invalid_arg "Newspeak.big_int_op: unexpected operator"

class simplify_arith =
object (self)
  inherit builder
    
  method process_lval x =
    match x with
	Shift (lv, Const CInt c) when Nat.compare c Nat.zero = 0 -> lv
      | Shift (Shift (lv, Const CInt c1), Const CInt c2) ->
	  let c = Nat.add c1 c2 in
	  let lv = Shift (lv, Const (CInt c)) in
	    self#process_lval lv
      | _ -> x

  (* TODO: generatlization of all this: do the operations with bignums
     and then come back to Int64 *)
  (* TODO: should use string to representer constants, not Int64, since 
     not all unsigned long long can be represented *)
  method process_exp e =
    match e with
	BinOp (MultI|PlusI|MinusI as op, Const CInt x, Const CInt y) ->
	  let z = (nat_op op) x y in
	    Const (CInt z)

      | BinOp (PlusPI, e, Const CInt x) when (Nat.compare x Nat.zero = 0) -> e

      | BinOp (DivI, Const CInt i1, Const CInt i2) 
	  when Nat.compare i2 Nat.zero <> 0 ->
	  Const (CInt (Nat.div i1 i2))

(* TODO: could do this after a sanity checks that checks the largest and 
   smallest integer ever computed in expressions!! *)
      | UnOp (Coerce r, 
	     (BinOp (MultI, UnOp (Belongs (l, u), _), Const CInt x) as e')) -> 
	  let l = Nat.mul l x in
	  let u = Nat.mul u x in
	    if contains r (l, u) then e' else e

      | _ -> e
end

class simplify_choose =
object 
  inherit builder

  method process_blk x =
    match x with
	(ChooseAssert [([], body)], _)::tl -> body@tl
      | (ChooseAssert _, _)::_ -> x
      | _ -> x
end

let remove_final_goto l blk =
  let rec remove blk = 
    match blk with
      	(Goto l', _)::[] when l = l' -> []
      | hd::tl -> hd::(remove tl)
      | [] -> []
  in
    remove blk

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

module Lbl = 
struct
  type t = lbl
  let compare = compare
end

module LblSet = Set.Make(Lbl)

let simplify_gotos blk =
  let rec simplify_blk x =
    match x with
      | [] -> ([], LblSet.empty)
      | hd::tl -> 
	  let (hd, hd_lbls) = simplify_stmt hd in
	  let (tl, tl_lbls) = simplify_blk tl in
	    (hd@tl, LblSet.union hd_lbls tl_lbls)
    
  and simplify_stmt (x, loc) =
    match x with
	Goto l -> ([x, loc], LblSet.singleton l)
      | Decl (name, t, body) -> 
	  let (body, lbls) = simplify_blk body in
	    if not (LblSet.is_empty lbls) 
	    then invalid_arg "Newspeak.simplify_gotos";
	    ([Decl (name, t, body), loc], lbls)

      | DoWith ((Goto l1, _)::[], l2, []) when l1 = l2 -> ([], LblSet.empty)
      | DoWith ([], _, []) -> ([], LblSet.empty)

      | DoWith (hd::tl, l, []) ->
	  let (hd, hd_lbls) = simplify_stmt hd in
	  let tl = remove_final_goto l tl in
	    if LblSet.mem l hd_lbls then begin
	      let (tl, tl_lbls) = simplify_blk tl in
	      let lbls = LblSet.union hd_lbls tl_lbls in
	      let lbls = LblSet.remove l lbls in
		((DoWith (hd@tl, l, []), loc)::[], lbls)
	    end else begin
	      let (tl, tl_lbls) = simplify_stmt (DoWith (tl, l, []), loc) in
	      let lbls = LblSet.union hd_lbls tl_lbls in
		(hd@tl, lbls)
	    end

      | ChooseAssert elts ->
	  let lbls = ref LblSet.empty in
	  let simplify_choice (cond, body) = 
	    let (body, choice_lbls) = simplify_blk body in
	      lbls := LblSet.union !lbls choice_lbls;
	      (cond, body)
	  in
	  let choices = List.map simplify_choice elts in
	    ([ChooseAssert choices, loc], !lbls)

      | InfLoop body -> 
	  let (body, lbls) = simplify_blk body in
	    ([InfLoop body, loc], lbls)

      | DoWith _ -> 
	  invalid_arg "Newspeak.simplify_gotos: empty action expected"

      | _ -> ([x, loc], LblSet.empty)
  in
    
  let (blk, lbls) = simplify_blk blk in
    if not (LblSet.is_empty lbls) 
    then invalid_arg "Newspeak.simplify_gotos: unexpected goto without label";
    blk

let rec simplify_stmt actions (x, loc) =
  let x =
    match x with
      | Set (lv, e, sca) -> 
	  Set (simplify_lval actions lv, simplify_exp actions e, sca)
      | Call (FunDeref (e, t)) -> Call (FunDeref (simplify_exp actions e, t))
      | Decl (name, t, body) -> Decl (name, t, simplify_blk actions body)
      | ChooseAssert elts ->
          let elts = List.map (simplify_choose_elt actions) elts in
	    ChooseAssert elts
      | InfLoop body ->
          let body = simplify_blk actions body in
	    InfLoop body
      | DoWith (body, l, action) -> 
	  let body = simplify_blk actions body in
	  let action = simplify_blk actions action in
	    DoWith (body, l, action)
      | _ -> x
  in
    (x, loc)
      
and simplify_choose_elt actions (cond, body) = 
  let cond = List.map (simplify_exp actions) cond in
  let body = simplify_blk actions body in
    (cond, body)
      
(* TODO: clean up simplifications, systematic:
   have a class list of simplifications, instead of applying a list,
   have an option to continue,
   suppress simplify_arithmexp *)
and simplify_exp actions e =
  let e = 
    match e with
	Lval (lv, sca) -> Lval (simplify_lval actions lv, sca)
      | AddrOf (lv, sz) -> AddrOf (simplify_lval actions lv, sz)
      | UnOp (o, e) -> UnOp (o, simplify_exp actions e)
      | BinOp (o, e1, e2) -> 
	  BinOp (o, simplify_exp actions e1, simplify_exp actions e2)
      | _ -> e
  in
  let e = ref e in
    List.iter (fun x -> e := x#process_exp !e) actions;
    !e

and simplify_lval actions lv =
  let lv =
    match lv with
      | Deref (e, sz) -> Deref (simplify_exp actions e, sz)
      | Shift (l, e) -> Shift (simplify_lval actions l, simplify_exp actions e)
      | _ -> lv
  in
  let lv = ref lv in
    List.iter (fun x -> lv := x#process_lval !lv) actions;
    !lv
	
and simplify_blk actions blk =
  let blk = ref blk in
    List.iter (fun x -> blk := x#process_blk !blk) actions;
    match !blk with
	hd::tl -> 
	  let hd = simplify_stmt actions hd in
	  let tl = simplify_blk actions tl in
	    hd::tl
      | [] -> []


(* TODO: do this in one tree traversal, instead of 2 *)
(* TODO: code optimization, this could be optimized, 
   maybe using inheritance ?? *)
(* TODO: once simplify choose is applied, there are opportunities for 
   simplify_gotos
   Fixpoint ??? *)
let simplify b = 
  simplify_gotos (simplify_blk [new simplify_choose; new simplify_ptr;
				new simplify_arith; new simplify_coerce] b)

let simplify_exp e =
  simplify_exp [new simplify_ptr; new simplify_arith; new simplify_coerce] e


let build_call f (args_t, ret_t) args =
  let loc = dummy_loc ("!Newspeak.build_call_"^f) in
  let call = ref [Call (FunId f), loc] in
  let i = ref 0 in
  let create_arg t e =
    match t with
	Scalar st ->
	  call := (Set (Local 0, e, st), loc)::!call;
	  call := [Decl ("arg"^(string_of_int !i), t, !call), loc];
	  incr i
      | _ -> invalid_arg "Newspeak.build_call: non scalar argument type"
  in
    List.iter2 create_arg (List.rev args_t) (List.rev args);
    begin match ret_t with
	None -> ()
      | Some t -> call := [Decl ("value_of_"^f, t, !call), loc]
    end;
    !call

let set_of_init loc name init =
  let set_of_init (offs, t, e) = 
    (Set (Shift (Global name, exp_of_int offs), e, t), loc)
  in
    List.map set_of_init init

let build_main_args ptr_sz loc params =
  let argv_name = "!ptr_array" in
  let process_param (n, globals, init) p =
    let name = "!param_str"^(string_of_int n) in
    let (len, param_init) = init_of_string p in
    let param_init = List.rev (set_of_init loc name param_init) in
    let lv = Shift (Global argv_name, exp_of_int (n * ptr_sz)) in
    let e = AddrOf (Global name, len*char_sz) in
    let init_argv = (Set (lv, e, Ptr), loc) in
    let globals = (name, Array (Scalar char_typ, len))::globals in
    let init = init_argv::param_init@init in
      (n+1, globals, init)
  in
  let (n, globals, init) = List.fold_left process_param (0, [], []) params in
  let init = List.rev init in
  let argv_glob = (argv_name, Array (Scalar Ptr, n)) in
  let argc = exp_of_int n in
  let argv = AddrOf (Global argv_name, n*ptr_sz) in
    (argv_glob::globals, init, argc::argv::[])


let build_main_call ptr_sz ft params =
  let fname = "main" in
  let loc = dummy_loc "!Newspeak.build_call_main" in
  let (args_t, _) = ft in
    match args_t with
	[Scalar Int _; Scalar Ptr] ->
	  let (globals, init, args) = build_main_args ptr_sz loc params in
	  let call = build_call fname ft args in
	  let call = ref (init@call) in
	  let bind_global (x, t) = call := [bind_var x t !call, loc] in
	    List.iter bind_global globals;
	    simplify !call
      | [] -> build_call fname ft []
      | _ -> invalid_arg "Newspeak.build_main_call: invalid type for main"


let has_goto lbl x =
  let rec blk_has_goto x = List.exists has_goto x

  and has_goto (x, _) =
  match x with
      Decl (_, _, body) | InfLoop body -> blk_has_goto body
    | ChooseAssert choices -> 
	List.exists (fun (_, x) -> blk_has_goto x) choices
    | DoWith (body, _, action) -> (blk_has_goto body) && (blk_has_goto action)
    | Goto lbl' -> lbl = lbl'
    | _ -> false
  in
    has_goto x

let split_loop lbl body =
  let rec split x =
    match x with
	hd::tl when not (has_goto lbl hd) -> 
	  let (prefix, suffix) = split tl in
	    (hd::prefix, suffix)
      | _ -> ([], x)
  in
    split body

let rec normalize_loop blk =
  match blk with
      (DoWith ([InfLoop body, loc], lbl, action), loc')::tl ->
	let (prefix, suffix) = split_loop lbl body in
	let body = prefix@[InfLoop (suffix@prefix), loc] in
	  (DoWith (body, lbl, action), loc')::(normalize_loop tl)
    | hd::tl -> hd::(normalize_loop tl)
    | [] -> []

class simplify_loops =
object 
  inherit builder

  method process_blk x = normalize_loop x
end

let normalize_loops b = simplify_blk [new simplify_loops] b

let rec build builder (globals, fundecs, spec) = 
  let globals' = Hashtbl.create 100 in
  let fundecs' = Hashtbl.create 100 in
  let build_global x gdecl = 
    builder#set_curloc unknown_loc;
    let gdecl = build_gdecl builder gdecl in
    let gdecl = builder#process_global x gdecl in
      Hashtbl.add globals' x gdecl
  in
  let build_fundec f fundec =
    builder#set_curloc unknown_loc;
    let fundec = build_fundec builder fundec in
      Hashtbl.add fundecs' f fundec
  in
    Hashtbl.iter build_global globals;
    Hashtbl.iter build_fundec fundecs;
    (globals', fundecs', spec)

and build_gdecl builder (t, init) =
  let t = build_typ builder t in
  let init = build_init_t builder init in
    (t, init)

and build_fundec builder (ft, body) = 
  let ft = build_ftyp builder ft in
  let body = 
    match body with
	Some body -> Some (build_blk builder body)
      | None -> None
  in
    (ft, body)

and build_typ builder t =
  match t with
      Scalar t -> Scalar (build_scalar_t builder t)
    | Array (t, n) ->
	let t = build_typ builder t in
	  Array (t, n)
    | Region (fields, sz) ->
	let fields = List.map (build_field builder) fields in
	let sz = build_size_t builder sz in
	  Region (fields, sz)

and build_scalar_t builder t =
  match t with
      Int k ->
	let k = build_ikind builder k in
	  Int k
    | Float sz ->
	let sz = build_size_t builder sz in
	  Float sz
    | Ptr -> t
    | FunPtr -> t

and build_field builder (o, t) =
  let t = build_typ builder t in
    (o, t)

and build_ikind builder (sign, sz) =
  let sz = build_size_t builder sz in
    (sign, sz)

and build_ftyp builder (args, ret) =
  let args = List.map (build_typ builder) args in
  let ret = 
    match ret with
	Some t -> Some (build_typ builder t)
      | None -> None
  in
    (args, ret)

and build_init_t builder init =
  match init with
      Zero -> Zero
    | Init cells -> 
	let cells = List.map (build_cell builder) cells in
	  Init cells

and build_cell builder (o, t, e) =
  let o = build_size_t builder o in
  let t = build_scalar_t builder t in
  let e = build_exp builder e in
    (o, t, e)

and build_size_t builder sz = builder#process_size_t sz

and build_blk builder blk = List.map (build_stmt builder) blk

and build_stmt builder (x, loc) =
  builder#set_curloc loc;
  let x = build_stmtkind builder x in
    (x, loc)

and build_stmtkind builder x =
  match x with
      Set (lv, e, t) ->
	let lv = build_lval builder lv in
	let e = build_exp builder e in
	let t = build_scalar_t builder t in
	  Set (lv, e, t)

    | Copy (lv1, lv2, n) ->
	let lv1 = build_lval builder lv1 in
	let lv2 = build_lval builder lv2 in
	let n = build_size_t builder n in
	  Copy (lv1, lv2, n)

    | Decl (x, t, body) ->
	let t = build_typ builder t in
	let body = build_blk builder body in
	  Decl (x, t, body)

    | ChooseAssert choices -> 
	let choices = List.map (build_choice builder) choices in
	  ChooseAssert choices

    | InfLoop body ->
	let body = build_blk builder body in
	  InfLoop body

    | DoWith (body, lbl, action) ->
	let body = build_blk builder body in
	let action = build_blk builder action in
	  DoWith (body, lbl, action)

    | Goto lbl -> Goto lbl

    | Call fn -> 
	let fn = build_fn builder fn in
	  Call fn

and build_choice builder (cond, body) =
  let cond = List.map (build_exp builder) cond in
  let body = build_blk builder body in
    (cond, body)

and build_fn builder fn =
  match fn with
      FunId f -> FunId f
    | FunDeref (e, ft) ->
	let e = build_exp builder e in
	let ft = build_ftyp builder ft in
	  FunDeref (e, ft)

and build_lval builder lv =
  let lv =
    match lv with
	Local x -> Local x
      | Global str -> Global str
      | Deref (e, sz) -> 
	  let e = build_exp builder e in
	  let sz = build_size_t builder sz in
	    Deref (e, sz)
      | Shift (lv, e) ->
	  let lv = build_lval builder lv in
	  let e = build_exp builder e in
	    Shift (lv, e)
  in
    builder#process_lval lv

and build_exp builder e =
  let e =
    match e with
	Const c -> Const c
      | Lval (lv, t) ->
	  let lv = build_lval builder lv in
	  let t = build_scalar_t builder t in
	    Lval (lv, t)
      | AddrOf (lv, sz) -> 
	  let lv = build_lval builder lv in
	  let sz = build_size_t builder sz in
	    AddrOf (lv, sz)
      | AddrOfFun f -> AddrOfFun f
      | UnOp (op, e) ->
	  let op = build_unop builder op in
	  let e = build_exp builder e in
	    UnOp (op, e)
      | BinOp (op, e1, e2) ->
	  let op = build_binop builder op in
	  let e1 = build_exp builder e1 in
	  let e2 = build_exp builder e2 in
	    BinOp (op, e1, e2)
  in
    builder#process_exp e

and build_unop builder op =
  match op with
      PtrToInt k ->
	let k = build_ikind builder k in
	  PtrToInt k
    | IntToPtr k ->
	let k = build_ikind builder k in
	  IntToPtr k
    | Cast (t1, t2) ->
	let t1 = build_scalar_t builder t1 in
	let t2 = build_scalar_t builder t2 in
	  Cast (t1, t2)
    | Belongs _ | Coerce _ | Not | BNot _-> op

and build_binop builder op =
  match op with
      PlusF sz -> PlusF (build_size_t builder sz)
    | MinusF sz -> MinusF (build_size_t builder sz)
    | MultF sz -> MultF (build_size_t builder sz)
    | DivF sz -> DivF (build_size_t builder sz)
    | Gt t -> Gt (build_scalar_t builder t)
    | Eq t -> Eq (build_scalar_t builder t)
    | PlusI | MinusI | MultI | DivI | Mod
    | BOr _ | BAnd _ | BXor _ | Shiftlt | Shiftrt | PlusPI | MinusPP -> op

(* Visitor *)
class visitor =
object 
  method process_gdecl (_: string) (_: gdecl) = true
  method process_fun (_: fid) (_: fundec) = true
  method process_fun_after () = ()
  method process_stmt (_: stmt) = true
  method process_fn (_: fn) = true
  method process_exp (_: exp) = true
  method process_lval (_: lval) = true
  method process_unop (_: unop) = ()
  method process_binop (_: binop) = ()
end

let rec visit_lval visitor x =
  let continue = visitor#process_lval x in
    match x with
	Deref (e, _) when continue -> visit_exp visitor e
      | Shift (lv, e) when continue ->
	  visit_lval visitor lv;
	  visit_exp visitor e
      | _ -> ()
  
and visit_exp visitor x =
  let continue = visitor#process_exp x in
    if continue then begin
      match x with
	  Lval (lv, _) -> visit_lval visitor lv
	| AddrOf (lv, _) -> visit_lval visitor lv
	| UnOp (op, e) -> 
	    visitor#process_unop op;
	    visit_exp visitor e
	| BinOp (bop, e1, e2) ->
	    visitor#process_binop bop;
	    visit_exp visitor e1;
	    visit_exp visitor e2
	| _ -> ()
    end else ()

let visit_fn visitor x =
  let continue = visitor#process_fn x in
    match x with
	FunDeref (e, _) when continue -> visit_exp visitor e
      | _ -> ()
  
let rec visit_blk visitor x = List.iter (visit_stmt visitor) x
    
and visit_stmt visitor (x, loc) =
  let continue = visitor#process_stmt (x, loc) in
    if continue then begin
      match x with
	  Set (lv, e, _) -> 
	    visit_lval visitor lv;
	    visit_exp visitor e
	| Copy (lv1, lv2, _) ->
	    visit_lval visitor lv1;
	    visit_lval visitor lv2
	| Decl (_, _, body) -> visit_blk visitor body
	| Call fn -> visit_fn visitor fn
	| ChooseAssert choices -> List.iter (visit_choice visitor) choices
	| InfLoop x -> visit_blk visitor x
	| DoWith (body, _, action) -> 
	    visit_blk visitor body;
	    visit_blk visitor action
	| Goto _ -> ()
    end else ()

and visit_choice visitor (cond, body) =
  List.iter (visit_exp visitor) cond;
  visit_blk visitor body

let visit_fun visitor fid (t, body) =
  let continue = visitor#process_fun fid (t, body) in
    match body with
	Some body when continue -> 
	  visit_blk visitor body;
	  visitor#process_fun_after ()
      | _ -> ()

let visit_init visitor (_, _, e) = visit_exp visitor e

let visit_glb visitor id (t, init) =
  let continue = visitor#process_gdecl id (t, init) in
    match init with
	Init x when continue -> List.iter (visit_init visitor) x 
      | _ -> ()

let visit visitor (globals, fundecs, _) =
  Hashtbl.iter (visit_glb visitor) globals;
  Hashtbl.iter (visit_fun visitor) fundecs

let max_ikind = max
  
