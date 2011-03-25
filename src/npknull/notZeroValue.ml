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

open State2.ValueSyntax

module Make(Subst: Transport.T) = 
struct
  (* set of all variables x such that (x, 0) points to a non-zero pointer *)
  type t = VarSet.t
      
  let universe () = VarSet.empty
    
  let is_not_null s e =
    match e with
	NotNull -> true
      | Lval VariableStart x -> VarSet.mem x s
      | _ -> false

  let assign (lv, e) s = 
    match (lv, e) with
	(VariableStart x, e) -> 
	  if (is_not_null s e) then VarSet.add x s
	  else VarSet.remove x s
      | (Variables x, _) -> VarSet.diff s x
	  
  let join = VarSet.inter
    
  let is_subset x1 x2 = VarSet.subset x2 x1
    
  let remove_variables variables x = VarSet.diff x (VarSet.of_list variables)
    
  let split variables x = 
    let variables = VarSet.of_list variables in
      (VarSet.inter x variables, VarSet.diff x variables)
	
  let substitute subst s = 
    let result = ref VarSet.empty in
    let changed = ref VarSet.empty in
    let substitute_variable x =
      let (y, is_start) = Subst.apply_variable_start subst x in
	if is_start then result := VarSet.union !result y
	else changed := VarSet.union !changed y
    in
      VarSet.iter substitute_variable s;
      VarSet.diff !result !changed
    
  let restrict = VarSet.inter
    
  let glue = VarSet.union
    
  let print x = 
    print_endline ("variables not null: "^(VarSet.to_string x))
      
  let satisfies s lv =
    match lv with
	VariableStart x -> VarSet.mem x s
      | _ -> false
end

