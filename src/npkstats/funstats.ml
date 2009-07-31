(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans
  
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
*)

open Newspeak

module GlbSet = Set.Make(String)
module LocSet = Set.Make(struct type t = int let compare = compare end)

type env =  {
  height: int;
  glb_read: GlbSet.t;
  glb_write: GlbSet.t;
  loc_read: LocSet.t;
  loc_write: LocSet.t
}

let init_env () = {
  height = 0;
  glb_read = GlbSet.empty;
  glb_write = GlbSet.empty;
  loc_read = LocSet.empty;
  loc_write = LocSet.empty
}

let write_env (g, l) env = 
  { 
    env with 
      glb_write = GlbSet.union g env.glb_write;
      loc_write = LocSet.union l env.loc_write 
  }
let read_env (g, l) env = 
  { 
    env with 
      glb_read = GlbSet.union g env.glb_read; 
      loc_read = LocSet.union l env.loc_read
  }

let join_env env1 env2 = {
  env1 with
    glb_read = GlbSet.union env1.glb_read env2.glb_read;
    glb_write = GlbSet.union env1.glb_write env2.glb_write;
    loc_read = LocSet.union env1.loc_read env2.loc_read;
    loc_write = LocSet.union env1.loc_write env2.loc_write
}

let global_is_used env = 
  not (GlbSet.is_empty env.glb_read) || not (GlbSet.is_empty env.glb_write)

let local_is_deref env =
  not (LocSet.is_empty env.loc_read) || not (LocSet.is_empty env.loc_write)

type funkind = 
    Empty
  | Pure
  | Globals of env
  | LocDeref of env
  | Other

let print_results funtbl =
  let empty_nb = ref 0 in
  let pure_nb = ref 0 in
  let global_nb = ref 0 in
  let deref_nb = ref 0 in
  let other_nb = ref 0 in
  let other = ref [] in
  let count f kind =
    let counter = 
      match kind with
	  Empty -> empty_nb
	| Pure -> pure_nb
	| Globals _ -> global_nb
	| LocDeref _ -> deref_nb
	| Other -> 
	    other := f::!other;
	    other_nb
    in
      incr counter
  in
      Hashtbl.iter count funtbl;
      if !empty_nb <> 0 
      then print_endline ("Empty functions: "^string_of_int !empty_nb);
      if !pure_nb <> 0 
      then print_endline ("Pure functions: "^string_of_int !pure_nb);
      if !global_nb <> 0 then begin
	print_endline ("Functions that read/update globals: "
		       ^string_of_int !global_nb)
      end;
      if !deref_nb <> 0 then begin
	print_endline ("Functions that deref locals: "^string_of_int !deref_nb)
      end;
      if !other_nb <> 0 then begin
  	print_endline "Remaining functions: ";
	List.iter print_endline !other
      end
  

let collect prog = 
  let funtbl = Hashtbl.create 100 in

  let rec process_lval env lv =
    match lv with
	(* Assumes local do not *)
	Local _ -> ((GlbSet.empty, LocSet.empty), (GlbSet.empty, LocSet.empty))
      | Global x -> 
	  ((GlbSet.singleton x, LocSet.empty), (GlbSet.empty, LocSet.empty))
      | Deref (Lval (Local x, _), _) -> 
	  ((GlbSet.empty, LocSet.singleton (env.height - x)), 
	   (GlbSet.empty, LocSet.empty))
      | Shift (lv, e) -> 
	  let (a, (glb1, loc1)) = process_lval env lv in
	  let (glb2, loc2) = process_exp env e in
	    (a, (GlbSet.union glb1 glb2, LocSet.union loc1 loc2))
      | _ -> raise Exit

  and process_exp env e =
    match e with
	Const _ -> (GlbSet.empty, LocSet.empty)
      | Lval (lv, _) -> 
	  let ((glb1, loc1), (glb2, loc2)) = process_lval env lv in
	    (GlbSet.union glb1 glb2, LocSet.union loc1 loc2)
      | AddrOf (lv, _) ->
	  let (_, v) = process_lval env lv in
	    v
      | UnOp ((Belongs _|Coerce _|PtrToInt _|IntToPtr _|Cast _|Not), e) -> 
	  process_exp env e
      | BinOp ((Eq _|Gt _|PlusI|MinusI|PlusPI|MultI|Mod|DivI), e1, e2) -> 
	  let (glb1, loc1) = process_exp env e1 in
	  let (glb2, loc2) = process_exp env e2 in
	    (GlbSet.union glb1 glb2, LocSet.union loc1 loc2)
      | _ -> raise Exit
  in

  let rec process_stmtkind env x =
    match x with
	Set (lv, e, _) -> 
	  let (a, r) = process_lval env lv in
	  let env = write_env a env in
	  let env = read_env r env in
	    read_env (process_exp env e) env
      | Decl (_, _, body) -> 
	  let env = { env with height = env.height + 1 } in
	  let env = process_blk env body in
	    { env with height = env.height - 1 }
      | Select (br1, br2) -> 
	  let env = process_blk env br1 in
	    process_blk env br2
      | Guard e -> write_env (process_exp env e) env
      | DoWith (body, _, action) -> 
	  let env = process_blk env body in
	    process_blk env action
      | Call FunId f -> begin
	  match process_fun f with
	      Empty | Pure -> env
	    | Globals env_f -> join_env env env_f
	    | LocDeref _ -> raise Exit
	    | Other -> raise Exit
	end
      | InfLoop body -> process_blk env body
      | Goto _ -> env
      | _ -> raise Exit
    
  and process_blk env x = 
    match x with
	(hd, _)::tl -> 
	  let env = process_stmtkind env hd in
	    process_blk env tl
      | [] -> env

  and process_fun f =
    try Hashtbl.find funtbl f 
    with Not_found -> 
      try
	let (_, blk) = Hashtbl.find prog.fundecs f in
	let env = init_env () in
	let kind =
	  match blk with
	      [] -> Empty
	    | _ -> 
		try 
		  let env = process_blk env blk in
		  let g = global_is_used env in
		  let l = local_is_deref env in
		    if not g && not l then Pure
		    else if not l then Globals env
		    else LocDeref env
		with Exit -> Other
	in
	  Hashtbl.add funtbl f kind;
	  kind
      with Not_found -> 
	Hashtbl.add funtbl f Other;
	Other
  in

  let process_fundec f _ = 
    let _ = process_fun f in
      ()
  in
  
    Hashtbl.iter process_fundec prog.fundecs;
    print_results funtbl
