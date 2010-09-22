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

(** Callback when updating a variable.  *)
type 'a update_check = (Prog.addr -> int) (* size mapping           *)
                     -> Newspeak.location (* location of update     *)
                     -> 'a                (* new value evaluated    *)
                     -> Alarm.t list      (* alarms to report       *)

type pointed_address =
  | Where_nowhere
  | Where_on_null
  | Where_on of Prog.addr * int Interval.t
  | Where_I_dont_know

(**
 * Unpacked abstract domain.
 *)
type 'a t =
  { top         : 'a
  ; bottom      : 'a
  ; join        : 'a -> 'a -> 'a
  ; meet        : 'a -> 'a -> 'a
  ; widen       : 'a -> 'a -> 'a
  ; to_string   : 'a -> string
  ; is_in_range : int -> int -> 'a -> bool
  ; eval        : (Prog.lval -> 'a)             (** Environment            *)
               -> (Prog.lval -> Prog.addr)      (** Abstract addr_of       *)
               -> Prog.exp                      (** Expression to evaluate *)
               -> 'a                            (** Abstract result        *)
                * Alarm.t list                  (** Alarms                 *)
  ; guard       : (Prog.lval -> 'a)             (** Environment            *)
               -> (Prog.lval -> Prog.addr)      (** Abstract addr_of       *)
               -> Prog.exp                      (** Expression to evaluate *)
               -> (Prog.lval * ('a -> 'a)) list (** List of (lv, f) pairs on
                                                  * which Box.guard will be
                                                  * called.
                                                  *)
  ; update : 'a update_check option
  ; top_array : int -> 'a                       (** Return an array with
                                                  * only `n` top elements *)
  ; where_does_it_point : 'a -> pointed_address
  }

(**
 * An evaluator for constant integers.
 * For example, const Range.com 0 = [0;0], etc.
 *)
val const : 'a t -> int -> 'a

(**
  * For scalar types, returns a logical zero (0/NULL), or an array filled with
  * zeroes.
  *)
val nil : 'a t -> typ:Prog.typ -> 'a

(**
 * Packed abstract domain.
 * t really means 'exists a. t = a c_dom'.
 * Packing/unpacking is made with pack and with_dom.
 *)
type packed_dom

(**
 * Pack an abstract domain.
 *)
val pack : 'a t -> packed_dom

(**
 * A wrapping scope.
 * 't scope is like "some piece of computation taking an abstract domain (c_dom) in
 * parameter and returning 't".
 *)
type 't scope =
  { bind : 'a. 'a t -> 't }

(**
 * Unpack an abstract domain.
 *)
val with_dom : packed_dom -> 'a scope -> 'a
