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

module Signatures = struct
  module type MONAD = sig
    type 'a m
    val return : 'a -> 'a m
    val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
  end
  module type MONAD_S = sig
    type 'a m
    val liftM2 : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m
  end
end

module Monad (M:Signatures.MONAD) = struct
  open M

  type 'a m = 'a M.m

  let liftM2 f mx my =
    mx >>= fun x ->
    my >>= fun y ->
    return (f x y)
end

module Maybe = struct

  let rec cat_maybes = function
    | [] -> []
    | Some h::t -> h::cat_maybes t
    | None  ::t ->    cat_maybes t

  let fmap f = function
    | None   -> None
    | Some x -> Some (f x)

  let from_maybe d = function
    | None   -> d
    | Some x -> x

  let maybe d f = function
    | None   -> d
    | Some x -> f x

  let (>>=) x f = match x with
    | None   -> None
    | Some x -> f x

  let return x = Some x

  type 'a m = 'a option
end

module Arrow = struct

  let first  f (a, b) = (f a,   b)
  let second f (a, b) = (  a, f b)

end
