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
  { lvals : (Types.variable * 'a) list
  ; lbls  : Newspeak.lbl list
  ; funs  : (Newspeak.fid  * 'a) list
  }

let empty =
  { lvals = []
  ; lbls  = []
  ; funs  = []
  }

exception Var_not_found of Types.variable

let get env k =
  try
    List.assoc k env.lvals
  with
    Not_found -> raise (Var_not_found k)

let add lv t env =
  { env with lvals = (lv, t)::env.lvals }

let add_lbl lbl env =
  { env with lbls = lbl::env.lbls}

let assert_lbl lbl env =
  if not (List.mem lbl env.lbls) then
    failwith ("No such label : " ^ Newspeak.string_of_lbl lbl)

let add_fun env fid t =
  { env with funs = (fid, t)::env.funs}

exception Fun_not_found of Newspeak.fid

let get_fun env fid =
  try
    List.assoc fid env.funs
  with Not_found -> raise (Fun_not_found fid)
