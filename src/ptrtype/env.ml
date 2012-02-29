(*
 * ptrtype: do finer typechecks on C pointers
 * Copyright (C) 2012 Etienne Millon
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * Etienne Millon <etienne.millon@eads.net>
 * EADS Innovation Works - SE/IT
 * 12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
 *)

type 'a t =
  { lvals : (Types.variable * (Newspeak.typ option * 'a)) list
  ; lbls  : Newspeak.lbl list
  }

let empty =
  { lvals = []
  ; lbls  = []
  }

exception Var_not_found of Types.variable

let get env k =
  try
    snd (List.assoc k env.lvals)
  with
    Not_found -> raise (Var_not_found k)

let get_npktype env k =
  try
    match fst (List.assoc k env.lvals) with
    | Some nt -> nt
    | None -> failwith ("No type hint on variable " ^ Types.string_of_variable k)
  with
    Not_found -> raise (Var_not_found k)

let add lv nt t env =
  { env with lvals = (lv, (nt, t))::env.lvals }

let add_lbl lbl env =
  { env with lbls = lbl::env.lbls}

let assert_lbl lbl env =
  if not (List.mem lbl env.lbls) then
    failwith ("No such label : " ^ Newspeak.string_of_lbl lbl)
