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

type t =
  | String of string
  | Int    of int
  | List   of t list
  | Dict   of (string * t) list

let rec render_data = function
  | String s -> "\"" ^ String.escaped s ^ "\""
  | Int n    -> string_of_int n
  | List ys  -> "[ " ^ (String.concat ", " (List.rev_map render_data ys)) ^ " ]"
  | Dict kvs ->
        "{"
      ^ (String.concat ", "
           (List.map
             (fun (k, v) ->
                k ^ ": " ^ render_data v
             )
             kvs
           )
        )
      ^ "}"

let render y =
  "---\n" ^ render_data y ^ "\n...\n"
