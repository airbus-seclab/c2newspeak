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

(* set of all variables x such that (x, 0) points to a non-zero pointer *)
type t = VarSet.t

let universe () = VarSet.empty

let assign (lv, is_not_null) s = 
  match lv with
      State2.VariableStart x when is_not_null -> VarSet.add x s
    | _ -> universe ()

let join _ _ = invalid_arg "join: Not implemented yet"

let is_subset = VarSet.subset

let remove_variables variables x = VarSet.diff x (VarSet.of_list variables)

let split variables x = 
  let variables = VarSet.of_list variables in
    (VarSet.inter x variables, VarSet.diff x variables)

(* TODO: not intuitive that apply_set is in Subst2 rather than
   being VarSet.substitute
*)
let substitute = Subst2.apply_set

let restrict = VarSet.inter

let glue = VarSet.union

let print x = 
  print_endline ("variables not null: "^(VarSet.to_string x))

let is_not_null s lv =
  match lv with
      State2.VariableStart x -> VarSet.mem x s
    | _ -> false
