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

(** Npksolver alarms.  *)
type alarm_kind =
  | Array_OOB                   (** Array index out of bounds.            *)
  | Ptr_OOB                     (** Pointer offset out of bounds.         *)
  | Null_deref                  (** Null pointer dereference.             *)
  | Ptr_bad_deref               (** Invalid pointer dereference.          *)
  | Assertion_failed of string  (** Generic error with a specific reason. *)

type t = Newspeak.location * alarm_kind * string option

(**
 * Emit an alarm.
 *
 * An alarm emission is associated to a particular location in the
 * analyzed source code.
 * If the same alarm is emitted several times from the same location,
 * it will be printed only once.
 *)
val emit : t -> unit

(**
 * Combine alarms.
 *)
val combine : ?extra:t list
           -> ('a -> 'a -> 'a)
           -> ('a * t list) -> ('a * t list) -> ('a * t list)

(**
 * Return the meet of alarm sets.
 *)
val meet : t list -> t list -> t list
