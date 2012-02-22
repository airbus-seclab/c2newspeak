(*
  ptrtype: do finer typechecks on C pointers
  Copyright (C) 2007-2011 Charles Hymans, Sarah Zennou
  Copyright (C) 2011-2010 Etienne Millon

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
  fdectype : 'ty scheme;
}

and 'ty scheme = Forall of int list * 'ty

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

and 'ty exp = ('ty bexp * 'ty)

and 'ty bexp =
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

module StringMap = Map.Make(String)

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

and string_of_exp sty (e, ty) =
  "(" ^ string_of_bexp sty e ^ " : " ^ sty ty ^ ")"

and string_of_bexp sty e =
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

let string_of_scheme sty (Forall (bv, t)) =
  let s_forall =
    if bv == [] then
      ""
    else
      String.concat " " (List.map string_of_int bv) ^ " . "
  in
  s_forall ^ sty t


let string_of_fundec sty name declaration =
  Printf.sprintf
    "%s : %s\n%s %s%s%s {\n%s}\n"
       name
       (string_of_scheme sty declaration.fdectype)
       (string_of_ret sty declaration.rets)
       (string_of_loc_as_prefix declaration.position)
       name
       (string_of_formal_args sty declaration.args)
       (string_of_blk sty 2 declaration.body)

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
