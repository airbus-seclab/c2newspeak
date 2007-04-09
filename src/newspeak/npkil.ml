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
(* TODO: replace Global_tmp by Global *)
  | Global_tmp of string
  | Deref of (exp * size_t)
  | Shift of (lval * exp)

and exp =
    Const of cte
  | Lval of (lval * scalar_t)
  | AddrOf of (lval * size_t)
  | AddrOfFun of fid
  | UnOp of (unop * exp)
  | BinOp of (binop * exp * exp)

and fn =
    FunId of fid
  | FunDeref of (exp * ftyp)

and init_t = 
  | Zero
  | Init of (size_t * scalar_t * exp) list

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
      Known of Int64.t
    | Glob_array_lst of string  (* last index of array *)

and tmp_size_t = int option

module String_set = 
  Set.Make (struct type t = string let compare = Pervasives.compare end)

type glb_type = {
(* TODO: replace with typ *)
  mutable gtype : Cil.typ;
  mutable gloc  : Newspeak.location;
(* This field is false when the global is false *)
(* When this field is false, the next one is None *)
(* TODO: should merge this field and the next one *)
  mutable gdefd : bool;
(* TODO: replace with init_t option
   None -> extern variable
   Some init_t -> defined variable with given initialization value 
*)
  mutable ginit : Cil.init option;
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
  ifuns  : (Newspeak.fid, fspec_type) Hashtbl.t;
  iusedglbs : String_set.t;
  iusedcstr : String_set.t;
  iusedfuns : String_set.t;
}

let zero = Const (CInt64 (Int64.zero))
let zero_f = Const (CFloat (0., "0."))

let make_int_coerce int_t e =
  UnOp (Coerce (domain_of_typ int_t), e)


let make_belongs len e =
  UnOp (Belongs_tmp (Int64.zero, Known (Int64.of_int (len-1))), e)


let exp_of_int x = Const (CInt64 (Int64.of_int x))

let rec negate exp =
  match exp with
    | UnOp (Not, BinOp (Eq t, e2, e1)) -> BinOp (Eq t, e1, e2)
    | UnOp (Not, e) -> e
    | BinOp (Gt t, e1, e2) -> UnOp (Not, BinOp (Gt t, e1, e2))
    | BinOp (Eq t, e1, e2) -> UnOp (Not, BinOp (Eq t, e2, e1))
    | UnOp (Coerce i, e) -> UnOp (Coerce i, negate e)
    | _ -> invalid_arg "Newspeak.negate"


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

let string_of_tmp_int x =
  match x with
      Known i -> Int64.to_string i
    | Glob_array_lst v -> "len("^v^")-1"

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
    | Global_tmp name -> "Global_tmp("^name^")"
    | Deref (e, sz) -> "["^(string_of_exp decls e)^"]"^(string_of_size_t sz)
    | Shift (lv, sh) -> (string_of_lval decls lv)^" + "^(string_of_exp decls sh)

and string_of_exp decls e =
  match e with
      Const c -> string_of_cte c
    | Lval (lv, t) -> (string_of_lval decls lv)^"_"^(string_of_scalar t)
    | AddrOf (lv, sz) -> "&_"^(string_of_size_t sz)^"("^(string_of_lval decls lv)^")"
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

let dump_npko inter = 
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
	      let new_align = (align^"  ") in
		print_string new_align;
		print_endline ((string_of_typ t)^" "^name^";");
		dump_blk new_align new_decls body;
		print_endline (align^"}")
	    end
	      
      | Label l -> 
	  print_endline ((string_of_lbl l)^":")
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
      | [], [] -> invalid_arg "Newspeak.dump_assertblk"
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


  let print_list title list =
    print_endline title;
    String_set.iter print_endline list;
    print_newline ()
  in

  let print_glob n g =
    if not g.gdefd then print_string "extern ";
    print_string ((string_of_type g.gtype)^" "^n);
    match g.ginit with
	None -> print_endline ";"
      | Some i -> print_endline (" = "^(string_of_init i)^";")
  in

  let print_fundef n f =
    dump_fundec n f.pbody;
    if f.pbody <> None then print_newline ()
  in
    print_endline inter.ifilename;

    print_list "Global used" inter.iusedglbs;
    print_list "Functions called" inter.iusedfuns;
    print_list "Constant Strings" inter.iusedcstr;

    print_endline "Global variables";
    Hashtbl.iter print_glob inter.iglobs;
    print_newline ();

    print_endline "Function definitions";
    Hashtbl.iter print_fundef inter.ifuns

let rec size_of t =
  match t with
    | Scalar t -> size_of_scalar t
    | Array (t, Some n) -> (size_of t) * n
    | Array (_, None) -> invalid_arg "Npkil.size_of: unknown size of array"
    | Region (_, n) -> n

