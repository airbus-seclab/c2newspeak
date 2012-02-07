(*
 * ptrtype: do finer typechecks on C pointers
 * Copyright (C) 2011-2012 Etienne Millon
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

let hashtbl_keys h =
  Hashtbl.fold (fun k _v l -> k::l) h []

let hashtbl_values h =
  Hashtbl.fold (fun _k v l -> v::l) h []

let hashtbl_mapi f h =
  let h' = Hashtbl.create 0 in
  Hashtbl.iter (fun k v -> Hashtbl.add h' k (f k v)) h;
  h'

let hashtbl_map f h =
  let g _k x = f x in
  hashtbl_mapi g h

let list_of_option = function
  | None -> []
  | Some x -> [x]

let option_of_list = function
  | [] -> None
  | [x] -> Some x
  | _ -> failwith "option_of_list"

let option_map f = function
  | None -> None
  | Some x -> Some (f x)
