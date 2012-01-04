(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007-2011  Charles Hymans, Sarah Zennou
  
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
 email: charles.hymans@penjili.org

  Sarah Zennou
  EADS Innovation Works - SE/IT
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: sarah(dot)zennou(at)eads(dot)net
*)

open Newspeak

type 'ty t = {
  globals: 'ty globals;
  init: 'ty blk;
  fundecs: (Newspeak.fid, 'ty fundec) Hashtbl.t;
  ptr_sz: Newspeak.size_t;
  src_lang: Newspeak.src_lang;
  abi: Newspeak.abi_t;
}

and 'ty fundec = {
  args : (string * 'ty) list;
  rets : (string * 'ty) list;
  body : 'ty blk;
  position: Newspeak.location;
}

and 'ty globals = (string, 'ty) Hashtbl.t

and 'ty stmtkind =
    Set	     of ('ty lval * 'ty exp * Newspeak.scalar_t)
  | Copy     of ('ty lval * 'ty lval * Newspeak.size_t)
  | Guard    of 'ty exp
  | Decl     of (string * 'ty * 'ty blk)
  | Select   of ('ty blk * 'ty blk)
  | InfLoop  of 'ty blk
  | DoWith   of ('ty blk * Newspeak.lbl)
  | Goto     of Newspeak.lbl
(* TODO: maybe should use a record rather than a tuple? *)
(* arguments, function type, function expression, return left values *)
  | Call     of (('ty exp * 'ty) list * 'ty funexp * ('ty lval * 'ty) list  )
  | UserSpec of 'ty assertion

and 'ty specs = 'ty assertion list

and 'ty assertion = 'ty spec_token list

and 'ty spec_token =
  | SymbolToken of char
  | IdentToken  of string
  | LvalToken   of ('ty lval * 'ty)
  | CstToken    of Newspeak.cst

and 'ty stmt = 'ty stmtkind * Newspeak.location

and 'ty blk = 'ty stmt list

and 'ty lval =
    Local  of string
  | Global of string
  | Deref  of ('ty exp * Newspeak.size_t)
  | Shift  of ('ty lval * 'ty exp)

and 'ty exp =
    Const     of Newspeak.cst
  | Lval      of ('ty lval * 'ty)
  | AddrOf    of 'ty lval
  | AddrOfFun of (Newspeak.fid * 'ty ftyp)
  | UnOp      of (Newspeak.unop * 'ty exp)
  | BinOp     of (Newspeak.binop * 'ty exp * 'ty exp)

(* TODO: try to remove ftyp?? maybe not, it comes in handy *)
and 'ty ftyp = 'ty list * 'ty list

and 'ty field = Newspeak.offset * 'ty

and 'ty funexp =
    FunId of Newspeak.fid
  | FunDeref of 'ty exp


(*---------*)
(* Display *)
(*---------*)

(* Useful types, functions and variables *)

module StringMap = 
  Map.Make (struct type t = string let compare = Pervasives.compare end)

let rec seq sep f l =
  match l with
    | [] -> ""
    | [e] -> f e
    | e::r -> (f e)^sep^(seq sep f r)

(* Types *)

let string_of_scalar s =
  match s with
      Int (sg, sz) -> (string_of_sign_t sg)^"int"^(string_of_size_t sz)
    | Float sz 	   -> "float" ^ (string_of_size_t sz)
    | Ptr 	   -> "ptr"
    | FunPtr 	   -> "fptr"

let string_of_list print l =
  "(" ^ (String.concat ", " (List.map print l)) ^ ")"

let string_of_formal_arg sty (arg_name, typ) =
  sty typ ^ " " ^ arg_name

let string_of_ret sty ret =
  match ret with
      [] -> "void"
    | l -> String.concat ", " (List.map (fun (_, t) -> sty t) l)

let string_of_formal_args sty args = 
  match args with
      [] -> "(void)"
    | l -> string_of_list (string_of_formal_arg sty) l
(* TODO: uniformize/cleanup outputs functions *)
let string_of_typ_list sty = function
  | []  -> "void"
  | args -> string_of_list sty args

let string_of_ret_list sty = function
  | []  -> "void"
  | args -> String.concat ", " (List.map sty args)

let string_of_ftyp sty (args, ret) = 
  string_of_typ_list sty args ^ " -> " ^ string_of_ret_list sty ret

let string_of_loc (fname, line, carac) = 
  if (fname = "") then invalid_arg "Newspeak.string_of_loc: unknown location";
  if (line < 0) || (carac < 0) then fname
  else (fname^":"^(string_of_int line)^"#"^(string_of_int carac))
  
let dummy_loc fname = 
  if fname = "" 
  then invalid_arg "Newspeak.dummy_loc: invalid function name for location";
  (fname, -1, -1)

let unknown_loc = ("", -1, -1)

(* Expressions *)
let string_of_cst c =
  match c with
      CInt c -> Nat.to_string c
    | CFloat (_, s) -> s
    | Nil -> "nil"
        
let string_of_bounds (l, u) = "["^(Nat.to_string l)^","^(Nat.to_string u)^"]"

let string_of_unop op =
  match op with
      Belongs r        -> "belongs"^(string_of_bounds r)
    | Coerce r 	       -> "coerce"^(string_of_bounds r)
    | Focus sz 	       -> "focus"^(string_of_size_t sz)
    | Cast (typ, typ') ->
        "("^(string_of_scalar typ')^" <= "^(string_of_scalar typ)^")"
    | Not 	       -> "!"
    | BNot _ 	       -> "~"
    | PtrToInt i       -> "("^(string_of_scalar (Int i))^")"
    | IntToPtr _       -> "(ptr)"
          
let rec string_of_lval sty lv =
  match lv with
    | Local name     -> name
    | Global name    -> name
    | Deref (e, sz)  -> "["^(string_of_exp sty e)^"]"^(string_of_size_t sz)
    | Shift (lv, sh) -> (string_of_lval sty lv)^" + "^(string_of_exp sty sh)

and string_of_args sty args = string_of_list (string_of_exp sty) args

and string_of_exp sty e =
  match e with
      Const c 		  -> string_of_cst c
    | Lval (lv, t) 	  -> (string_of_lval sty lv)^"_"^(sty t)
    | AddrOf lv 	  -> "&("^(string_of_lval sty lv)^")"
    | AddrOfFun (fid, ft) -> "&_{"^(string_of_ftyp sty ft)^"}("^fid^")"
    | BinOp (op, e1, e2)  ->
        "("^(string_of_exp sty e1)^" "^(Newspeak.string_of_binop op)^
          " "^(string_of_exp sty e2)^")"

    | UnOp (op, exp) -> (string_of_unop op)^" "^(string_of_exp sty exp)

let string_of_funexp sty f =
  match f with
      FunId fid -> fid
    | FunDeref exp -> "["^(string_of_exp sty exp)^"]"

(* Actual dump *)
let string_of_lbl l = "lbl"^(string_of_int l)

let dump_gdecl sty name t = print_endline (sty t^" "^name^";")

let string_of_token sty x =
  match x with
      SymbolToken x -> String.make 1 x
    | IdentToken x -> x
    | LvalToken (x, _) -> "'"^(string_of_lval sty x)^"'"
    | CstToken c -> string_of_cst c

let string_of_assertion sty x =
  let res = ref "" in
  let append_token x = res := !res^(string_of_token sty x)^" " in
    List.iter append_token x;
    !res

let string_of_loc_as_prefix loc = 
  if loc = unknown_loc then "" else "("^(string_of_loc loc)^")^"

let string_of_blk sty offset x =
  let buf = Buffer.create 80 in
  let offset = ref offset in
  let incr_margin () = offset := !offset + 2 in
  let decr_margin () = offset := !offset - 2 in
  let dump_line str = 
    let margin = String.make !offset ' ' in
      Buffer.add_string buf (margin^str^"\n") 
  in
  let dump_line_at loc str =
    let loc = string_of_loc_as_prefix loc in
    let margin = String.make !offset ' ' in
      Buffer.add_string buf (margin^loc^str^"\n") 
  in

  let rec dump_stmt only (sk, loc) =
    match sk with
        Set (lv, e, sc) ->
          dump_line_at loc ((string_of_lval sty lv)^" =("^(string_of_scalar sc)^
                        ") "^(string_of_exp sty e)^";")
      | Guard b -> dump_line_at loc ("guard("^(string_of_exp sty b)^");")
      | Copy (lv1, lv2, sz) ->
          dump_line_at loc ((string_of_lval sty lv1)^" ="^(Newspeak.string_of_size_t sz)^
                        " "^(string_of_lval sty lv2)^";")
            
      | Decl (x, t, body) ->
          if only then begin
            dump_line_at loc ((sty t)^" "^x^";");
            dump_blk body
          end else begin
            dump_line_at loc "{";
            incr_margin ();
            dump_line ((sty t)^" "^x^";");
            dump_blk body;
            decr_margin ();
            dump_line "}"
          end
            
      | DoWith (body, lbl) ->
          dump_line_at loc "do {";
          incr_margin ();
          dump_blk body;
          decr_margin ();
          dump_line ("} with lbl"^(string_of_int lbl)^":")

      | Goto l -> dump_line_at loc ("goto "^(string_of_lbl l)^";")
      | Call (args, fn, ret_vars) ->  
	  let string_of_args (x, t) = string_of_exp sty x^": "^sty t in
	  let string_of_rets (x, t) = string_of_lval sty x^": "^sty t in
	  let args = List.map string_of_args args in
	  let rets = List.map string_of_rets ret_vars in
          
	  let ret_str = 
	    match rets with
	      | [] -> ""
	      | r::[] -> r ^ " <- "
	      | _ -> (string_of_list (fun x -> x) rets) ^ " <- "
          in
	  let arg_str = string_of_list (fun x -> x) args in
          let result = 
	    ret_str ^ (string_of_funexp sty fn) ^ arg_str ^ ";" 
	  in
            dump_line_at loc result
      | Select (body1, body2) ->
          dump_line_at loc "choose {";
          dump_line " -->";
          incr_margin ();
          dump_blk body1;
          decr_margin ();
          dump_line " -->";
          incr_margin ();
          dump_blk body2;
          decr_margin ();
          dump_line "}"

      | InfLoop body -> 
          dump_line_at loc "while (1) {";
          incr_margin ();
          dump_blk body;
          decr_margin ();
          dump_line "}"

      | UserSpec x -> dump_line_at loc (string_of_assertion sty x)

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
  
let string_of_fundec sty name declaration =
  let str_args = string_of_formal_args sty declaration.args in
  let str_ret  = string_of_ret sty declaration.rets in
  let position = string_of_loc_as_prefix declaration.position in
  let result   = str_ret ^ " " ^ position ^ name ^ str_args ^ " {\n" in
  let result   = result^string_of_blk sty 2 declaration.body^"}\n" in
    result

let dump_fundec sty name declaration = 
  print_endline (string_of_fundec sty name declaration)

let dump_globals sty gdecls = 
  (* TODO: Clean this mess... StringMap *)
  let glbs = ref (StringMap.empty) in
    Hashtbl.iter 
      (fun name info -> glbs := (StringMap.add name info !glbs)) 
      gdecls;
    StringMap.iter (dump_gdecl sty) !glbs

(* Exported print functions *)
let dump string_of_ty prog =
  (* TODO: Clean this mess... StringMap *)
  let funs = ref (StringMap.empty) in
  let collect_funbody name body =
    funs := StringMap.add name body !funs
  in
  let init = string_of_blk string_of_ty 0 prog.init in
    Hashtbl.iter collect_funbody prog.fundecs;
    StringMap.iter (dump_fundec string_of_ty) !funs;
    dump_globals string_of_ty prog.globals;
    print_string init

(* Input/output functions *)
let write name prog =
  let cout = open_out_bin name in
    Marshal.to_channel cout "NPK!" [];
    Marshal.to_channel cout Version.newspeak_hash [];
    Marshal.to_channel cout prog [];
    close_out cout

let read name = 
  try
    let cin = open_in_bin name in
    let str = Marshal.from_channel cin in
      if str <> "NPK!" 
      then invalid_arg ("Newspeak.read: "^name^" is not an .npk file");
      let version = Marshal.from_channel cin in
        if (version <> Version.newspeak_hash) then begin
          invalid_arg ("Newspeak.read: this file was generated with a "
                       ^"different version of c2newspeak. "
                       ^"Please regenerate your file or install the latest "
                       ^"version of newspeak."^
                       " Operation aborted.")
        end;
        Marshal.from_channel cin
  with Failure "input_value: bad object" -> 
    invalid_arg ("Newspeak.read: "^name^" is not an .npk file")


let rec addr_of_deref lv = 
  match lv with
      Deref (e, _) -> e
    | Shift (lv, i) -> BinOp (PlusPI, addr_of_deref lv, i)
    | _ -> raise Not_found

let nat_op op =
  match op with
      PlusI -> Nat.add
    | MinusI -> Nat.sub
    | MultI -> Nat.mul
    | _ -> invalid_arg "Newspeak.big_int_op: unexpected operator"

module Lbl = 
struct
  type t = lbl
  let compare = compare
end

module LblSet = Set.Make(Lbl)
(* TODO: try to implement it with a builder
   or propose a different kind of builder? *)

let has_goto lbl x =
  let rec blk_has_goto x = List.exists has_goto x

  and has_goto (x, _) =
  match x with
      Decl (_, _, body) | InfLoop body | DoWith (body, _) -> blk_has_goto body
    | Select (body1, body2) 				  -> 
	(blk_has_goto body1) || (blk_has_goto body2)
    | Goto lbl' 	    				  -> lbl = lbl'
    | _ 		    				  -> false
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
      (DoWith ([InfLoop body, loc], lbl), loc')::tl ->
        let (prefix, suffix) = split_loop lbl body in
        let body 	     = prefix@[InfLoop (suffix@prefix), loc] in
          (DoWith (body, lbl), loc')::(normalize_loop tl)
    | hd::tl -> hd::(normalize_loop tl)
    | [] -> []

let rec build builder prog = 
  let globals' = Hashtbl.create 100 in
  let fundecs' = Hashtbl.create 100 in
  let build_global x gdecl = 
    let gdecl = build_gdecl builder gdecl in
    let gdecl = builder#process_global x gdecl in
      Hashtbl.add globals' x gdecl
  in
  let build_fundec f fundec =
    builder#set_curloc unknown_loc;
    let fundec = build_fundec builder fundec in
      Hashtbl.add fundecs' f fundec
  in
    Hashtbl.iter build_global prog.globals;
    Hashtbl.iter build_fundec prog.fundecs;
    { prog with globals = globals'; fundecs = fundecs' }

and build_gdecl builder t =
  build_typ builder t

and build_fundec builder fd = 
  let (args_t, ret_t) = build_formal_ftyp builder (fd.args, fd.rets) in
    { args = args_t;
      rets = ret_t;
      body = build_blk builder fd.body;
      position = fd.position;
    }

and build_typ builder t =
  match t with
      Scalar t 		  -> Scalar (build_scalar_t builder t)
    | Array (t, n) 	  ->
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
  let o = build_offset builder o in
  let t = build_typ builder t in
    (o, t)

and build_ikind builder (sign, sz) =
  let sz = build_size_t builder sz in
    (sign, sz)

and build_formal_ftyp builder (args, ret) =
  let build_arg (x, t) = (x, build_typ builder t) in
  let args 	       = List.map build_arg args in
  let ret 	       = List.map build_arg ret in
    (args, ret)

and build_ftyp builder (args, ret) =
  let args = List.map (build_typ builder) args in
  let ret  = List.map (build_typ builder) ret in
    (args, ret)

and build_cell builder (o, t, e) =
  let o = build_size_t builder o in
  let t = build_scalar_t builder t in
  let e = build_exp builder e in
    (o, t, e)

and build_offset builder o = builder#process_offset o

and build_size_t builder sz = builder#process_size_t sz

and build_blk builder blk = 
  let blk =
    match blk with
        hd::tl -> 
          let hd = build_stmt builder hd in
          let tl = build_blk builder tl in
            hd::tl
      | [] -> []
  in
    builder#process_blk blk

and build_stmt builder (x, loc) =
  builder#set_curloc loc;
  let x = build_stmtkind builder x in
    (x, loc)

and build_stmtkind builder x =
  builder#enter_stmtkind x; 
  let x = 
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
              
      | Guard b -> Guard (build_exp builder b)

      | Decl (x, t, body) ->
          let t = build_typ builder t in
          let body = build_blk builder body in
            Decl (x, t, body)
              
      | Select (body1, body2) -> 
          Select (build_blk builder body1, build_blk builder body2)
              
      | InfLoop body -> InfLoop (build_blk builder body)
              
      | DoWith (body, lbl) -> DoWith (build_blk builder body, lbl)
              
      | Goto lbl -> Goto lbl
          
      | Call (args, fn, ret_vars) -> 
          let args' = List.map (fun (x, t) -> (build_exp builder x, t)) args in
	  let ret_vars = 
	    List.map (fun (x, t) -> (build_lval builder x, t)) ret_vars 
	  in
          let fn' = build_funexp builder fn in
            Call (args', fn', ret_vars)

      | UserSpec assertion -> 
          let assertion = List.map (build_token builder) assertion in
            UserSpec assertion
  in
    builder#process_stmtkind x

and build_token builder x =
  match x with
      LvalToken (lv, t) -> LvalToken ((build_lval builder lv), t)
    | _ -> x

and build_choice builder (cond, body) =
  let cond = List.map (build_exp builder) cond in
  let body = build_blk builder body in
    (cond, body)

and build_funexp builder fn =
  match fn with
      FunId f -> FunId f
    | FunDeref e -> FunDeref (build_exp builder e)

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
      | Lval (lv, s) ->
          let lv' = build_lval builder lv in
          let s' = build_typ builder s in
            Lval (lv', s')
      | AddrOf lv -> 
          let lv' = build_lval builder lv in
            AddrOf lv'
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
    | Focus sz -> Focus (build_size_t builder sz)
    | Belongs _ | Coerce _ | Not | BNot _-> op

and build_binop builder op =
  match op with
      PlusF sz -> PlusF (build_size_t builder sz)
    | MinusF sz -> MinusF (build_size_t builder sz)
    | MultF sz -> MultF (build_size_t builder sz)
    | DivF sz -> DivF (build_size_t builder sz)
    | MinusPP -> MinusPP
    | Gt t -> Gt (build_scalar_t builder t)
    | Eq t -> Eq (build_scalar_t builder t)
    | PlusI | MinusI | MultI | DivI | Mod
    | BOr _ | BAnd _ | BXor _ | Shiftlt | Shiftrt | PlusPI -> op

let visit_size_t visitor x = visitor#process_size_t x

let visit_length visitor x = visitor#process_length x

let visit_ikind visitor (_, sz) = visit_size_t visitor sz

let visit_scalar_t visitor t =
  match t with
      Int k -> visit_ikind visitor k
    | Float sz -> visit_size_t visitor sz
    | Ptr -> ()
    | FunPtr -> ()

let visit_typ visitor t = 
  visitor#process_typ t;
  let rec visit_typ t =
    match t with
        Scalar t -> visit_scalar_t visitor t
      | Array (t, n) -> 
          visit_typ t;
          visit_length visitor n
      | Region (fields, sz) ->
          List.iter (fun (_, t) -> visit_typ t) fields;
          visit_size_t visitor sz
  in
    visit_typ t

let visit_ftyp visitor (args, ret) =
  List.iter (visit_typ visitor) args;
  List.iter (visit_typ visitor) ret

let rec visit_lval visitor x =
  let continue = visitor#process_lval x in
    match x with
        Deref (e, sz) when continue -> 
          visit_exp visitor e;
          visit_size_t visitor sz
      | Shift (lv, e) when continue ->
          visit_lval visitor lv;
          visit_exp visitor e
      | _ -> ()
  
and visit_exp visitor x =
  let continue = visitor#process_exp x in
    if continue then begin
      match x with
          Lval (lv, _) -> visit_lval visitor lv
        | AddrOf lv -> visit_lval visitor lv
        | UnOp (op, e) -> 
            visitor#process_unop op;
            visit_exp visitor e
        | BinOp (bop, e1, e2) ->
            visitor#process_binop bop;
            visit_binop visitor bop;
            visit_exp visitor e1;
            visit_exp visitor e2
        | _ -> ()
    end

and visit_binop visitor op =
  match op with
      PlusF sz | MinusF sz | MultF sz | DivF sz -> visit_size_t visitor sz
    | _ -> ()

let visit_funexp visitor x =
  let continue = visitor#process_funexp x in
    match x with
        FunDeref e when continue -> visit_exp visitor e
      | _ -> ()

let rec visit_blk visitor x = List.iter (visit_stmt visitor) x
    
and visit_stmt visitor (x, loc) =
  visitor#set_loc loc;
  let continue = visitor#process_stmt (x, loc) in
    if continue then begin
      match x with
          Set (lv, e, _) -> 
            visit_lval visitor lv;
            visit_exp visitor e
        | Copy (lv1, lv2, sz) ->
            visit_lval visitor lv1;
            visit_lval visitor lv2;
            visit_size_t visitor sz
        | Guard b -> 
            visitor#process_bexp b;
            visit_exp visitor b
        | Decl (_, t, body) -> 
            visit_typ visitor t;
            visit_blk visitor body
        | Call (args, fn, ret_vars) ->
	    let visit_arg (x, t) = 
	      visit_exp visitor x;
	      visit_typ visitor t
	    in
	    let visit_ret (x, t) =
	      visit_lval visitor x;
	      visit_typ visitor t
	    in
              List.iter visit_arg args;
	      List.iter visit_ret ret_vars;
              visit_funexp visitor fn
        | Select (body1, body2) -> 
            visitor#set_loc loc;
            visit_blk visitor body1;
            visitor#set_loc loc;
            visit_blk visitor body2
        | InfLoop x -> visit_blk visitor x
        | DoWith (body, _) -> visit_blk visitor body
        | Goto _ -> ()
        | UserSpec assertion -> visit_assertion visitor assertion
    end else ()

and visit_assertion visitor x = List.iter (visit_token visitor) x

and visit_token builder x =
  match x with
      LvalToken (lv, t) -> 
        visit_lval builder lv;
        visit_typ builder t
    | _ -> ()

let visit_fun visitor fid ft =
  let continue = visitor#process_fun fid ft in
  if continue then begin
    visit_ftyp visitor ((List.map snd ft.args), List.map snd ft.rets);
    visit_blk visitor ft.body;
    visitor#process_fun_after ()
  end

let visit_init visitor (_, _, e) = visit_exp visitor e

let visit_glb visitor id t =
  let continue = visitor#process_gdecl id t in
    if continue then visit_typ visitor t

let visit visitor prog =
  Hashtbl.iter (visit_glb visitor) prog.globals;
  visit_blk visitor prog.init;
  Hashtbl.iter (visit_fun visitor) prog.fundecs

let max_ikind = max

let rec equal_stmt (x1, _) (x2, _) =
  match (x1, x2) with
      (Decl (_, t1, body1), Decl (_, t2, body2)) -> 
        t1 = t2 && equal_blk body1 body2
    | (Select (bl1, br1), Select (bl2, br2)) ->
         (equal_blk bl1 bl2) && (equal_blk br1 br2)
    | (InfLoop body1, InfLoop body2) -> equal_blk body1 body2
    | (DoWith (body1, lbl1), DoWith (body2, lbl2)) ->
        equal_blk body1 body2 && lbl1 = lbl2
    | _ -> x1 = x2
  
and equal_blk x1 x2 = List.for_all2 equal_stmt x1 x2

let rec belongs_of_exp x =
  match x with
      Lval (lv, _) | AddrOf lv -> belongs_of_lval lv
    | UnOp (Belongs b, e)      -> (b, e)::(belongs_of_exp e)
    | UnOp (_, e) 	       -> belongs_of_exp e 
    | BinOp (_, e1, e2)        -> (belongs_of_exp e1)@(belongs_of_exp e2)
    | _ 		       -> []

and belongs_of_lval x =
  match x with
      Deref (e, _) -> belongs_of_exp e
    | Shift (lv, e) -> (belongs_of_lval lv)@(belongs_of_exp e)
    | _ -> []

let belongs_of_funexp x =
  match x with
      FunDeref e -> belongs_of_exp e
    | _ -> []

let exp_of_int x = Const (CInt (Nat.of_int x))

let return_value = Temps.return_value

let char_kind () =
  let char_signedness =
   if !Config.is_char_type_signed then Signed else Unsigned
  in
    (char_signedness, !Config.size_of_char)

let char_typ () = Int (char_kind ())
  
let is_generic_temp name = Temps.is_generic_temp name
