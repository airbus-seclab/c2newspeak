(*
  tHIS FIle is part of npksolver, a solver for Newspeak,
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

type 'a update_method = Prog.lval        (* lvalue in program text *)
                     -> old_value:'a     (* old value of variable  *)
                     -> new_value:'a     (* new value evaluated    *)
                     -> (Prog.lval * 'a) (* where to write what    *)

type 'a c_dom =
  { top       : 'a
  ; bottom    : 'a
  ; incl      : 'a -> 'a -> bool
  ; join      : 'a -> 'a -> 'a
  ; meet      : 'a -> 'a -> 'a
  ; widen     : 'a -> 'a -> 'a
  ; to_string : 'a -> string
  ; eval  : (Prog.lval -> 'a) -> Prog.exp -> 'a
  ; guard : Prog.exp -> (Prog.lval * ('a -> 'a)) list
  ; update : 'a update_method
  }

type 't scope = 
  { bind : 'a. 'a c_dom -> 't }

type t =
  { open_dom : 'a. 'a scope -> 'a }

let pack imp =
  { open_dom = fun scope -> scope.bind imp }

let with_dom p e =
  p.open_dom e

let do_nothing =
  { bind =  fun _ -> () }

let const dom n =
  let empty_env _ =
    invalid_arg "empty_environment"
  in
  dom.eval empty_env (Prog.Const n)

let destructive_update text_lv ~old_value ~new_value =
  ignore old_value;
  (text_lv, new_value)
