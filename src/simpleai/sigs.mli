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

module type State =
sig
  type t
    
  (** [universe] is the state with no variable and no information. *)
  val universe: t
    
  (** [contains s1 s2] is true if all the states represented by [s2] are also
      represented by [s1]. *)
  val contains: t -> t -> bool
    
  val join: t -> t -> t
    
  (** [add_var x s] adds variable [x] to the state [s]. Initially variable [x] 
      may take any value. *)
  val add_var: Simple.vid -> t -> t
    
  (** [assign lv e s] performs the assignment of the value denoted by 
      expression [e] to the variable denoted by left value [lv].  *)
  val assign: Simple.lval -> Simple.exp -> t -> t
    
  (** [guard e s] splits stats [s] in a state that evaluates [e] to a value 
      different from 0. *)
  val guard: Simple.exp -> t -> t
    
  (** [implies s a] returns true if assertion [a] is checked by state [s]. *)
  val implies: t -> Simple.assertion -> bool
    
  (** [is_safe_binop s (op, e1, e2)] returns true if the evaluation of binary
      opertion [op] on arguments [e1] and [e2] is safe in state [s]. *)
  val is_safe_binop: t -> (Simple.binop * Simple.exp * Simple.exp) -> bool
    
  (** [to_string s] prints the informations in the *)
  val to_string: t -> string
end
