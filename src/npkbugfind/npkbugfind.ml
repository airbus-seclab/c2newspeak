(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain, Sarah Zennou
  
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

  Sarah Zennou
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: sarah(dot)zennou(at)eads(dot)net
*)

open Newspeak

let debug = ref false

let input = ref []

let speclist = [("--debug", Arg.Set debug, "prints debug information");]

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

  method print_warning msg = print_warning this#get_loc msg
    
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
	| BinOp (MinusI, Const CInt x, Lval (_, Scalar (Int (Signed, _))))
	    when Nat.compare x Nat.zero = 0 -> 
	    this#print_warning 
	      "negation of a variable: make sure it cannot be min_int"
	| _ -> ()
    in
      true

  method process_fun f _ =
    if !debug then print_endline ("Scanning function: "^f);
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
      Lval (lv, _) | AddrOf lv -> scan_lval env lv
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

let rec scan_blk env x = 
  match x with
      (Guard b, _)::tl ->
	let env = scan_exp env b in
	  scan_blk env tl
    | hd::tl -> scan_stmt env hd; scan_blk env tl
    | [] -> ()

and scan_stmt env (x, loc) =
  cur_loc := loc;
  match x with
      Select (blk1, blk2) -> 
	scan_blk env blk1; 
	scan_blk env blk2
    | InfLoop body | Decl (_, _, body) -> scan_blk [] body
    | DoWith (body, _) -> scan_blk env body
    | Goto _ | Call _ | Set _ | Copy _ | Guard _ | UserSpec _ -> ()

let scan_fundef f fd = 
  if !debug then print_endline ("Scanning function: "^f);
  scan_blk [] fd.body

let scan_prog fundefs = 
  Hashtbl.iter scan_fundef fundefs

module Exp = 
struct
  type t = Newspeak.exp
  let compare = compare
end

module Env = Set.Make(Exp)

let find_bound e env =
  let env = Env.elements env in
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
      Lval (lv, _) | AddrOf lv -> scan2_lval env lv
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
    | (Guard b, loc)::tl -> 
	cur_loc := loc;
	let conds = Env.add b Env.empty in
	let env = Env.union conds env in
	  scan2_blk env tl
    | hd::tl ->
	let env = scan2_stmt env hd in
	  scan2_blk env tl
    | [] -> env

and scan2_stmt env (x, loc) =
  cur_loc := loc;
  match x with
    | Select (blk1, blk2) -> 
	let env = Env.union env (scan2_blk env blk1) in
	  Env.union env (scan2_blk env blk2)
    | Decl (_, _, body) -> scan2_blk env body
    | InfLoop body ->
	let _ = scan2_blk env body in 
	  env
    | DoWith (body, _) ->
	let _ = scan2_blk env body in
	  env
    | Goto _ -> Env.empty
    | Set (lv, e, _) -> 
	scan2_lval env lv;
	scan2_exp env e;
	env
    | Call _ | Copy _ | Guard _ | UserSpec _ -> env

(* propagates the list of conditions that are verified in each block *)
let scan2_fundef f fd = 
  if !debug then print_endline ("Scanning function: "^f);
  let _ = scan2_blk Env.empty fd.body in
    ()

let scan2_prog fundefs = 
  Hashtbl.iter scan2_fundef fundefs

let scan f =
  let prog = Newspeak.read f in
  let scanner = (new scanner :> Newspeak.visitor) in
(* TODO: try to merge together all 3 scanners *)
    if !debug then print_endline "First scanner";
    Newspeak.visit scanner prog;
    if !debug then print_endline "Second scanner";
    scan_prog prog.fundecs;
    if !debug then print_endline "Third scanner";
    scan2_prog prog.fundecs

let process () = List.iter scan !input

let _ = 
  StandardApplication.launch speclist anon_fun usage_msg process
