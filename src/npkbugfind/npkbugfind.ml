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

open Newspeak

let input = ref []

let speclist = []

let anon_fun file = input := file::!input

let usage_msg = Sys.argv.(0)^" [options] [-help|--help] file.npk"

let warnings = ref []

let print_warning loc msg =
  if not (List.mem (loc, msg) !warnings) then begin
    warnings := (loc, msg)::!warnings;
    let (file, line, _) = loc in
    let pos = 
      if loc = Newspeak.unknown_loc then ""
      else " in "^file^" line "^(string_of_int line)
    in
      print_endline ("Warning: "^msg^pos)
  end

class scanner =
object (this)
  inherit Newspeak.visitor

  method print_warning msg = print_warning (this#get_loc ()) msg
    
  method process_exp e =
    let _ = 
      match e with
	  UnOp (Cast (Float _, Int _), Const _) -> 
	    this#print_warning 
	      "useless float constant: immediate cast to an int"
	| UnOp (Coerce b, Const CInt x) when not (Newspeak.belongs x b) ->
	    this#print_warning 
	      "invalid type for integer constant: integer overflow" 
	| UnOp (Cast (t1, t2), 
		BinOp (_, 
		       UnOp (Cast (t2a, t1a), _), 
		       UnOp (Cast (t2b, t1b), _)))
	    when t1 = t1a && t1 = t1b && t2 = t2a && t2 = t2b ->
	    this#print_warning 
	      "maybe unnecessary casts, due to improper operator" 
	| BinOp (MinusI, Const CInt x, Lval (_, Int (Signed, _))) 
	    when Nat.compare x Nat.zero = 0 -> 
	    this#print_warning 
	      "negation of a variable: make sure it cannot be min_int"
	| _ -> ()
    in
      true

end

let cur_loc = ref Newspeak.unknown_loc

let scan_unop env op e =
  match op with
      Belongs _ -> e::env
    | _ -> env

let scan_binop env op e1 e2 =
  match op with
      Gt (Int _) when (List.mem e1 env) || (List.mem e2 env) ->
	print_warning !cur_loc 
	  ("expression checked after being used for array access: "
	   ^"make sure array access is really protected");
	env
    | _ -> env
	
let rec scan_exp env x =
  match x with
      Lval (lv, _) | AddrOf (lv, _) -> scan_lval env lv
    | UnOp (op, e) -> 
	let env = scan_exp env e in
	  scan_unop env op e
    | BinOp (op, e1, e2) -> 
	let env = scan_exp env e1 in
	let env = scan_exp env e2 in
	  scan_binop env op e1 e2
    | Const _ | AddrOfFun _ -> env

and scan_lval env x =
  match x with
      Local _ | Global _ -> env
    | Deref (e, _) -> scan_exp env e
    | Shift (lv, e) -> 
	let env = scan_lval env lv in
	  scan_exp env e

let rec scan_blk env x = List.iter (scan_stmt env) x

and scan_stmt env (x, loc) =
  cur_loc := loc;
  match x with
      ChooseAssert choices -> List.iter (scan_choice env) choices
    | InfLoop body | Decl (_, _, body) -> scan_blk [] body
    | DoWith (body, _, action) ->
	scan_blk env body;
	scan_blk env action
    | Goto _ | Call _ | Set _ | Copy _ -> ()

and scan_choice env (conds, body) =
  let env = ref env in
  let scan_exp x = env := scan_exp !env x in
    List.iter scan_exp conds;
    scan_blk !env body

let scan_fundef _ (_, body) = scan_blk [] body

let scan_prog fundefs = 
  Hashtbl.iter scan_fundef fundefs

let find_bound e env =
  let rec find env =
    match env with
	BinOp (_, Const CInt c, e')::_
      | BinOp (_, e', Const CInt c)::_ when e = e' -> c
      | _::tl -> find tl
      | [] -> raise Not_found
  in
    find env

let rec scan2_exp env x =
  match x with
      Lval (lv, _) | AddrOf (lv, _) -> scan2_lval env lv
    | UnOp (Belongs (_, u), e) -> begin
	try
	  let c = find_bound e env in
	    if Nat.compare (Nat.sub c Nat.one) u > 0 
	    then print_warning !cur_loc "potential array out of bounds"
	with Not_found -> ()
      end;
	scan2_exp env e
    | UnOp (_, e) -> scan2_exp env e
    | BinOp (_, e1, e2) -> 
	scan2_exp env e1;
	scan2_exp env e2
    | Const _ | AddrOfFun _ -> ()

and scan2_lval env x =
  match x with
      Local _ | Global _ -> ()
    | Deref (e, _) -> scan2_exp env e
    | Shift (lv, e) -> 
	scan2_lval env lv;
	scan2_exp env e

let rec scan2_blk env x = 
  match x with
      hd::tl ->
	let env = scan2_stmt env hd in
	  scan2_blk env tl
    | [] -> env

and scan2_stmt env (x, loc) =
  cur_loc := loc;
  match x with
      ChooseAssert choices -> scan2_choices env choices
    | Decl (_, _, body) -> scan2_blk env body
    | InfLoop body ->
	let _ = scan2_blk env body in 
	  env
    | DoWith (body, _, action) ->
	let _ = scan2_blk env body in
	let _ = scan2_blk env action in
	  env
    | Goto _ -> []
    | Set (lv, e, _) -> 
	scan2_lval env lv;
	scan2_exp env e;
	env
    | Call _ | Copy _ -> env

and scan2_choices env x =
  let res = ref env in
  let scan2_choice (conds, body) =
    let env = conds@env in
    let env' = scan2_blk env body in
      res := env'@(!res)
  in
    List.iter scan2_choice x;
    !res

(* propagates the list of conditions that are verified in each block *)
let scan2_fundef _ (_, body) = 
  let _ = scan2_blk [] body in
    ()

let scan2_prog fundefs = 
  Hashtbl.iter scan2_fundef fundefs

let scan f =
  let prog = Newspeak.read f in
  let scanner = (new scanner :> Newspeak.visitor) in
(* TODO: try to merge together all 3 scanners *)
    Newspeak.visit scanner prog;
    scan_prog prog.fundecs;
    scan2_prog prog.fundecs

let _ = 
  try
    Arg.parse speclist anon_fun usage_msg;
    List.iter scan !input
      
  with Invalid_argument s -> 
    print_endline ("Fatal error: "^s);
    exit 0
