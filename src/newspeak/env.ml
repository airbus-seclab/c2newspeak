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

(* TODO: code cleanup: remove unused functions *)
type t = {
  fun_env: Csyntax.fundefs;
  global_env: Csyntax.glbdecls;
  local_env: (string, int * Csyntax.typ * Newspeak.location) Hashtbl.t;
  mutable vcnt: int
}

let create (glbdecls, fundefs) =
  {
    fun_env = fundefs;
    global_env = glbdecls;
    local_env = Hashtbl.create 100;
    vcnt = 0
  }

let push env x t loc =
  env.vcnt <- env.vcnt + 1;
  Hashtbl.add env.local_env x (env.vcnt, t, loc)

let pop env x =
  env.vcnt <- env.vcnt - 1;
  Hashtbl.remove env.local_env x

let get_var env x = Npkil.Local (env.vcnt - x)

let get_locals env =
  let res = ref [] in
  let get_var x (i, t, loc) = res := (i, (t, x, loc))::!res in
    Hashtbl.iter get_var env.local_env;
    Hashtbl.clear env.local_env;
    let res = List.sort (fun (i, _) (j, _) -> compare i j) !res in
    let (_, res) = List.split res in
      res

let get_ret_name () = "!return"

let get_ret_typ env =
  try
    let (_, t, _) = Hashtbl.find env.local_env (get_ret_name ()) in
      t
  with Not_found ->
    Npkcontext.error "Env.get_ret_typ" "Return variable not declared"

(* TODO: code cleanup *)
let get_ret env = (1, get_ret_typ env)

let get_ret_lbl () = 0
let get_brk_lbl () = 1

let get_ftyp env f =
  try
    let (ftyp, _, _) = Hashtbl.find env.fun_env f in
      ftyp
  with Not_found ->
    Npkcontext.error "Env.get_ftyp" 
      ("Unknown function "^f^" Please provide a prototype")
