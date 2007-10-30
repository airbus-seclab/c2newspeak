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

type t = {
  fundefs: (string, Npkil.funinfo) Hashtbl.t;
  globals: (string, Npkil.ginfo) Hashtbl.t;
  global_env: Csyntax.glbdecls;
  local_env: (string, int * Csyntax.typ * Newspeak.location) Hashtbl.t;
  mutable vcnt: int
}

let create glbdecls =
  {
    fundefs = Hashtbl.create 100;
    globals = Hashtbl.create 100;
    global_env = glbdecls;
    local_env = Hashtbl.create 100;
    vcnt = 0
  }

let extract_prog env fname =
  (fname, env.globals, env.fundefs)

let push env x t loc =
  env.vcnt <- env.vcnt + 1;
  Hashtbl.add env.local_env x (env.vcnt, t, loc)

let pop env x =
  env.vcnt <- env.vcnt - 1;
  Hashtbl.remove env.local_env x

let get_var env x =
  try
    let (n, t, _) = Hashtbl.find env.local_env x in
      (Npkil.Local (env.vcnt - n), t)
  with Not_found ->
    try
      let (t, _, _) = Hashtbl.find env.global_env x in
	(Npkil.Global x, t)
    with Not_found -> 
      Npkcontext.error "Env.get_var" ("Variable "^x^" not declared")

let get_locals env =
  let res = ref [] in
  let get_var x (i, t, loc) = res := (i, (t, x, loc))::!res in
    Hashtbl.iter get_var env.local_env;
    Hashtbl.clear env.local_env;
    let res = List.sort (fun (i, _) (j, _) -> compare i j) !res in
    let (_, res) = List.split res in
      res

let add_global env x data = Hashtbl.add env.globals x data

let get_ret_name () = "!return"

let get_ret_typ env =
  try
    let (_, t, _) = Hashtbl.find env.local_env (get_ret_name ()) in
      t
  with Not_found ->
    Npkcontext.error "Env.get_ret_typ" "Return variable not declared"

let get_ret_lbl () = 0
let get_brk_lbl () = 1

let update_funbody env f body =
  try
    let (ftyp, prev_body) = Hashtbl.find env.fundefs f in
      match prev_body with
	| None -> Hashtbl.replace env.fundefs f (ftyp, Some body)
	| Some _ -> 
	    Npkcontext.error "Compiler.update_fundef" 
		"Multiple definition of function body"
  with Not_found -> 
    Npkcontext.error "Compiler.update_fundef" "Function should be defined"
      
let update_ftyp env f ftyp =
  try
    let (prev_ftyp, _) = Hashtbl.find env.fundefs f in
      if ftyp <> prev_ftyp 
      then begin 
	Npkcontext.error "Compiler.update_ftyp" 
	  ("Different types for function "^f)
      end
  with Not_found -> Hashtbl.add env.fundefs f (ftyp, None)

