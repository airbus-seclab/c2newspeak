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

  
module Make(Store : Store):
sig
  type t
      (** Type [t] is a stack of labels. The constructor Do \{ Blk  \} 
	  with label : \{ Blk2 \  means that in blk we can do a goto to label 
	  and then we apply the code of Blk2.
	  Each label of the stack is associated to a store (the store just 
	  after all gotos to this label).
      *)
     
  val create: unit -> t
    (** create an empty stack  *)
    
  val push: t -> Newspeak.lbl -> unit
    (** Add a label. We add a label wen we encounter the Do of a Do...With *)
    
  val add: t -> Newspeak.lbl -> Store.t -> unit
    (** When we encounter a goto, we add the current store to the label.*)
    
  val pop: t -> Store.t
    (** Pop the top label of the stack. We use it when we encounter 
	the With. *)
end
