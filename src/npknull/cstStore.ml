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

open State2.PtrSyntax

(* TODO: could simplify further so that at this level the notion of offset does
   not exist anymore!! *)
module Make(Subst: Transport.T) =
struct
  (* TODO: create a module VarMap (this is used by store2 too) *)
  module VarMap = Map.Make(String)
    
  type t = string VarMap.t
      (* f(x) = y means that (x, 0) points to (y, 0) *)

  (* TODO: same code as in store2 *)
  let universe () = VarMap.empty

  let join store1 store2 = 
    let result = ref (universe ()) in
    let add_common_information x y =
      if VarMap.mem x store2 then result := VarMap.add x y !result
    in
      VarMap.iter add_common_information store1;
      !result

  let is_subset store1 store2 = 
    try
      let check_store1_has_information x y = 
	if (VarMap.find x store1 <> y) then raise Not_found
      in
	VarMap.iter check_store1_has_information store2;
	true
    with Not_found -> false
    

  let remove_variables variables store = 
    let result = ref store in
      List.iter (fun x -> result := VarMap.remove x !result) variables;
      !result

  let substitute subst store = 
    let result = ref (universe ()) in
    let substitute_info x y =
      let (x, has_no_delta1) = Subst.apply_variable_start subst x in
      let (y, has_no_delta2) = Subst.apply_variable_start subst y in
	if (VarSet.cardinal x = 1) && (VarSet.cardinal y = 1)
	  && has_no_delta1 && has_no_delta2 then begin
	    let x = VarSet.choose x in
	    let y = VarSet.choose y in
	      result := VarMap.add x y !result
	  end
    in
      VarMap.iter substitute_info store;
      !result
    

  let glue _ _ = universe ()

  let restrict _ _ = universe ()

  let print store = 
    let print_info x y = print_endline (x^" -> ("^y^", 0)") in
      VarMap.iter print_info store

(* TODO: should simplify the language to a language where src is just 
   is_not_null *)
  let assign args store = 
    match args with
	(Var (x, x_offset_is_zero), Var (y, y_offset_is_zero))
	   when x_offset_is_zero && y_offset_is_zero ->
	     VarMap.add x y store
      | _ -> universe ()

  let split variables store = 
    let reachable = ref VarMap.empty in
    let unreachable = ref store in
    let move_variable x = 
      try
	let y = VarMap.find x store in
	  reachable := VarMap.add x y !reachable;
	  unreachable := VarMap.remove x !unreachable
      with Not_found -> ()
    in
      List.iter move_variable variables;
      (!reachable, !unreachable)

  let eval_exp store e = 
    match e with
	Deref (Var (x, offset_is_zero)) when offset_is_zero -> begin
	  try
	    let y = VarMap.find x store in
	      Some (State2.VariableStart y)
	  with Not_found -> None
	end
      | _ -> None
end
