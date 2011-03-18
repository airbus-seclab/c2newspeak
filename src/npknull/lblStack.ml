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


module type Store =
sig
  type t
  val emptyset: unit -> t
  val join: t -> t -> t
end

module Make(Store : Store) =
struct
  type t = (Newspeak.lbl * Store.t) list ref
      
  let create () = ref []
    
  let push stk lbl = 
    stk := (lbl, Store.emptyset ())::!stk
    
  let add stk lbl s =
    let rec add x =
      match x with
	  ((lbl', _) as hd)::tl when lbl <> lbl' -> hd::(add tl)
	| (_, s')::tl -> (lbl, Store.join s s')::tl
	| [] -> 
	    invalid_arg ("Lblstack.add: Label "^(string_of_int lbl)
			 ^" not found")
    in
    let stk' = add !stk in
      stk := stk'

  let pop stk =
    match !stk with
	(_, s)::tl -> 
	  stk := tl;
	  s
      | [] -> invalid_arg ("Lblstack.pop: empty stack")

end
