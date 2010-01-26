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

type 'a update_check = (Prog.addr -> int) (* size mapping           *)
                     -> Newspeak.location (* location of update     *)
                     -> 'a                (* new value evaluated    *)
                     -> Alarm.t list      (* alarms to report       *)

type 'a c_dom =
  { top         : 'a
  ; bottom      : 'a
  ; incl        : 'a -> 'a -> bool
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
  }

type 't scope =
  { bind : 'a. 'a c_dom -> 't }

type t =
  { open_dom : 'a. 'a scope -> 'a }

let pack imp =
  { open_dom = fun scope -> scope.bind imp }

let with_dom p e =
  p.open_dom e

let const dom n =
  let empty_env _ =
    invalid_arg "empty_environment"
  in
  fst (dom.eval empty_env empty_env (Prog.Const (Prog.CInt n)))
