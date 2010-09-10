(*
  This file is part of npksolver, a solver for Newspeak,
  a minimal language framework well suited for static analysis.

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

  Copyright 2009, 2010 Etienne Millon <etienne.millon@eads.net>

 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)

(**
 * A 'box' is a mapping between lvalues and abstract values.
 * It is a non-relational abstract store.
 * Every vertex in the control-flow graph has an specific box value.
 *
 * The type t is purely functional, and can be copied without fear of sharing or
 * side effects.
 *)
type 'a t

(**
 * Equality test.
 *)
val equal : 'a t -> 'a t -> bool

(**
 * The 'top' value, meaning that no information is known.
 *)
val top : 'a Domain.t -> 'a t

(**
 * The 'bottom' value, meaning that the associated control-flow graph vertex is
 * unreachable (dead code).
 *)
val bottom : 'a t

(**
 * A box with a single variable with the specified type and abstract value.
 *)
val singleton : 'a Domain.t -> Prog.lval -> typ:Prog.typ -> 'a -> 'a t

(**
 * Variable-wise abstract join operation.
 *)
val join : 'a t -> 'a t -> 'a t

(**
 * Variable-wise abstract meet operation.
 *)
val meet : 'a t -> 'a t -> 'a t

(**
 * Variable-wise widening operation.
 *)
val widen : 'a t -> 'a t -> 'a t

(**
 * Guard evaluation.
 * 'guard lv f x' applies f to the abstract value of lv in x.
 *)
val guard : Prog.lval -> ('a -> 'a) -> 'a t -> 'a  t

(**
 * Set the abstract value for a lvalue.
 *)
val set_var : Prog.lval -> 'a -> 'a t -> 'a t

(**
 * Get abstract value of variables.
 * 'environment dom x' is a 'lookup' function.
 *)
val environment : 'a t -> (Prog.lval -> 'a)

(**
 * Returns the size of a variable.
 *)
val get_size : 'a t -> Prog.addr -> int

(**
 * Returns the type of a variable.
 *)
val typeof : 'a t -> Prog.addr -> Prog.typ

(**
 * Maps lvalues to abstract addresses.
 * Global variables are 'naturally' associated to Heap addresses,
 * and local variables are mapped onto Stack addresses by the mean of a
 * "stack pointer" related to a specific box value.
 * The stack pointer is incremented (resp. decremented) by the push (resp. pop)
 * function.
 *)
val addr_of : 'a t -> Prog.lval -> Prog.addr

(**
 * Increment the stack pointer.
 * "Pushes" the stack pointer to allocate space for a new variable of type
 * 'typ'.
 *)
val push : typ:Prog.typ -> 'a t -> 'a t

(**
 * Decrement the stack pointer.
 * The opposite of push. The value of the most recent variable is wiped out.
 *)
val pop : 'a t -> 'a t

(**
 * Pretty printer.
 *)
val to_string : 'a t -> string

(**
 * YAML dumper.
 * Returns "None" for a bottom value, and a dictionnary of values, otherwise.
 *)
val dump_yaml : 'a t -> Yaml.t option
