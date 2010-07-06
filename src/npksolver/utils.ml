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

let rec filter_list = function
  | [] -> []
  | Some h::t -> h::filter_list t
  | None  ::t ->    filter_list t

let rec filter_map f = function
  |  []  -> []
  | h::t -> begin
              match f h with
              | None   ->    filter_map f t
              | Some r -> r::filter_map f t
            end

let may f = function
  | None   -> None
  | Some x -> Some (f x)

let with_default d = function
  | None   -> d
  | Some x -> x

module Lift = struct
  type 'a lift = 'a option

  let maybe d f = function
    | None   -> d
    | Some x -> f x

  let bind f =
    maybe None f

  let (>>=) x f = bind f x

  let return x = Some x

  let bind2 f x y =
    x >>= fun x' ->
    y >>= fun y' ->
    f x' y'

  let liftM2 f =
    bind2 (fun x y -> return (f x y))

end
