open Newspeak

module C = Csyntax

let typedefs = Hashtbl.create 100

let clean () = Hashtbl.clear typedefs

let define_type x t = Hashtbl.add typedefs x t

let is_type x = Hashtbl.mem typedefs x

type base_typ =
    | Void 
    | Integer of (sign_t * ityp)    
    | Struct of decl list
    | Union of decl list
    | Name of string

and var_modifier = 
    | Abstract
    | Variable of string
    | Function of (var_modifier * decl list)
    | Array of (var_modifier * Int64.t option)
    | Pointer of var_modifier
    | Attr of (attr list * var_modifier)

and attr = Const

and ityp = 
    | Char 
    | Short
    | Int
    | Long
    | LongLong

and decl = ((base_typ * attr list) * var_modifier)

let size_of_ityp t =
  match t with
      Char -> Config.size_of_char
    | Short -> Config.size_of_short
    | Int -> Config.size_of_int
    | Long -> Config.size_of_long
    | LongLong -> Config.size_of_longlong

let int64_to_int x =
  if Int64.compare x (Int64.of_int max_int) > 0 
  then Npkcontext.error "Firstpass.int64_to_int" "integer too big";
  if Int64.compare x (Int64.of_int max_int) > 0 
  then Npkcontext.error "Firstpass.int64_to_int" "expecting positive integer";
  Int64.to_int x

(* TODO: check this for various architecture ? 
   Here align everything on 4 *)
let align o sz = 
  let offset = o mod 4 in
  if offset = 0 then o
  else if offset + sz <= 4 then o
  else (o - offset) + 4

let append_attrs attrs t = if attrs = [] then t else Attr (attrs, t)

let rec normalize_base_typ t =
  match t with
      Integer (s, t) -> C.Int (s, size_of_ityp t)
    | Struct f -> C.Struct (normalize_struct_fields f)
    | Union f -> C.Union (normalize_union_fields f)
    | Void -> C.Void
    | Name x -> 
	try Hashtbl.find typedefs x
	with Not_found ->
	  Npkcontext.error "Synthack.normalize_base_typ" ("Unknown type "^x)

and normalize_struct_fields f =
  let rec normalize o f=
    match f with
	d::f ->
	  let (t, x) = normalize_decl d in
	  let sz = C.size_of t in
	  let o = align o sz in
	  let (f, n) = normalize (o+sz) f in
	    ((x, (o, t))::f, n)
      | [] -> ([], align o Config.size_of_int)
  in
  let (f, n) = 
    match f with
	d::[] ->
	  let (t, x) = normalize_decl d in
	  let sz = C.size_of t in
	    ((x, (0, t))::[], sz)
      | _ -> normalize 0 f 
  in
    (f, n)

and normalize_union_fields f =
  let n = ref 0 in
  let normalize d =
    let (t, x) = normalize_decl d in
    let sz = C.size_of t in
      if !n < sz then n := sz;
      (x, (0, t))
  in
  let f = List.map normalize f in
    (f, !n)

and normalize_var_modifier b v is_const =
  match v with
      Abstract -> (b, C.undefined, is_const)
    | Variable x -> (b, x, is_const)
    | Function (Variable f, args) -> 
	(C.Fun (List.map normalize_decl args, b), f, false)
    | Function (Pointer v, args) -> 
	let args = List.map normalize_decl args in
	  normalize_var_modifier (C.Ptr (C.Fun (args, b))) v false
    | Array (v, n) -> 
	let n = 
	  match n with
	      None -> None
	    | Some n -> Some (int64_to_int n) 
	in
	  normalize_var_modifier (C.Array (b, n)) v false
    | Pointer v -> normalize_var_modifier (C.Ptr b) v false
    | Attr (Const::_, v) -> normalize_var_modifier b v true
    | Function _ -> 
	Npkcontext.error "Synthack.normalize_var_modifier" 
	  "case not implemented yet"
    | Attr _ -> 
	Npkcontext.error "Synthack.normalize_var_modifier" 
	  "case not implemented yet"

and normalize_decl d =
  let (t, x, _) = normalize_glbdecl d in
    (t, x)

and normalize_glbdecl ((b, attrs), v) =
  let b = normalize_base_typ b in
    normalize_var_modifier b (append_attrs attrs v) false

