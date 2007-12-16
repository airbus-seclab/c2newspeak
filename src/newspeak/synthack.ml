open Newspeak

module B = Bare_csyntax
module C = Csyntax

type base_typ =
    | Void 
    | Integer of ikind
    | Float of int
    | Struct of (string * decl list option)
    | Union of (string * decl list option)
    | Name of string
    | Enum of (string * Int64.t option) list option

and var_modifier = 
    | Abstract
    | Variable of (string * location)
    | Function of (var_modifier * decl list)
    | Array of (var_modifier * Int64.t option)
    | Pointer of var_modifier

and decl = (base_typ * var_modifier)

let typedefs = Hashtbl.create 100
let enumdefs = Hashtbl.create 100
let compdefs = ref (Hashtbl.create 100)

let get_compdefs () = 
  let res = !compdefs in
    Hashtbl.clear typedefs;
    Hashtbl.clear enumdefs;
    compdefs := Hashtbl.create 100;
    res

let define_type x t = Hashtbl.add typedefs x t

let is_type x = Hashtbl.mem typedefs x

let define_comp n f = Hashtbl.add !compdefs n f

let define_enum e =
  let rec define_enum e n =
    match e with
	(x, v)::tl ->
	  if Hashtbl.mem enumdefs x then begin
	    Npkcontext.error "Synthack.define_enum"
	      ("Enum "^x^" already defined")
	  end;
	  let n = 
	    match v with
		None -> n
	      | Some n -> n
	  in
	  Hashtbl.add enumdefs x n;
	  define_enum tl (Int64.succ n)
      | [] -> ()
  in
    define_enum e Int64.zero

let find_enum x = Hashtbl.find enumdefs x

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

let rec normalize_base_typ t =
  match t with
      Integer k -> C.Int k
    | Float n -> C.Float n
    | Struct (n, f) -> 
	begin match f with
	    None -> ()
	  | Some f -> 
	      let f = normalize_struct_fields f in
		define_comp n f
	end;
	C.Struct n
    | Union (n, f) -> 
	begin match f with
	    None -> ()
	  | Some f -> 
	      let f = normalize_union_fields f in
		define_comp n f
	end;
	C.Union n
    | Void -> C.Void
    | Enum f ->
	begin match f with
	    None -> ()
	  | Some f -> define_enum f
	end; 
	C.int_typ
    | Name x -> 
	try Hashtbl.find typedefs x
	with Not_found ->
	  Npkcontext.error "Synthack.normalize_base_typ" ("Unknown type "^x)

and normalize_struct_fields f =
  let rec normalize o f=
    match f with
	d::f ->
	  let (t, x) = normalize_arg d in
	  let sz = C.size_of !compdefs t in
	  let o = align o sz in
	  let (f, n) = normalize (o+sz) f in
	    ((x, (o, t))::f, n)
      | [] -> ([], align o Config.size_of_int)
  in
  let (f, n) = 
    match f with
	d::[] ->
	  let (t, x) = normalize_arg d in
	  let sz = C.size_of !compdefs t in
	    ((x, (0, t))::[], sz)
      | _ -> normalize 0 f 
  in
    (f, n)

and normalize_union_fields f =
  let n = ref 0 in
  let normalize d =
    let (t, x) = normalize_arg d in
    let sz = C.size_of !compdefs t in
      if !n < sz then n := sz;
      (x, (0, t))
  in
  let f = List.map normalize f in
    (f, !n)

and normalize_var_modifier b v =
  match v with
      Abstract -> (b, C.undefined, Newspeak.dummy_loc "")
    | Variable (x, loc) -> (b, x, loc)
    | Function (Variable (f, loc), args) -> 
	(C.Fun (List.map normalize_arg args, b), f, loc)
    | Function (Pointer v, args) -> 
	let args = List.map normalize_arg args in
	  normalize_var_modifier (C.Ptr (C.Fun (args, b))) v
    | Array (v, n) -> 
	let n = 
	  match n with
	      None -> None
	    | Some n -> Some (int64_to_int n) 
	in
	  normalize_var_modifier (C.Array (b, n)) v
    | Pointer v -> normalize_var_modifier (C.Ptr b) v
    | Function _ -> 
	Npkcontext.error "Synthack.normalize_var_modifier" 
	  "case not implemented yet"
	  
and normalize_arg a = 
  let (t, x, _) = normalize_decl a in
    (t, x)

and normalize_decl (b, v) =
  let t = normalize_base_typ b in
    normalize_var_modifier t v
