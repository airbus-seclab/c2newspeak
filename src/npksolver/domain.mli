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
 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)

type 'a update_method = (Prog.addr -> int) (* size mapping *)
                     -> Prog.lval          (* lvalue in program text *)
                     -> old_value:'a       (* old value of variable  *)
                     -> new_value:'a       (* new value evaluated    *)
                     -> (Prog.lval * 'a)   (* where to write what    *)

type 'a c_dom =
  { top       : 'a
  ; bottom    : 'a
  ; incl      : 'a -> 'a -> bool
  ; join      : 'a -> 'a -> 'a
  ; meet      : 'a -> 'a -> 'a
  ; widen     : 'a -> 'a -> 'a
  ; to_string : 'a -> string
  ; is_in_range : int -> int -> 'a -> bool
  ; eval  : (Prog.lval -> 'a) -> (Prog.lval -> Prog.addr) -> Prog.exp -> 'a
  ; guard : Prog.exp -> (Prog.lval * ('a -> 'a)) list
  ; update : 'a update_method
  }

val destructive_update : 'a update_method

type t

val pack : 'a c_dom -> t

type 't scope = 
  { bind : 'a. 'a c_dom -> 't }

val with_dom : t -> 'a scope -> 'a

val const : 'a c_dom -> int -> 'a
