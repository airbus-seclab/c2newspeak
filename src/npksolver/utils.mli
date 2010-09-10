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

module Signatures : sig
  module type MONAD = sig
    type 'a m

    (**
     * Monad entry point.
     * return a = Some a
     *)
    val return : 'a -> 'a m

    (**
     * Monadic composition of option values.
     *  None    >>= f = None
     * (Some x) >>= f = f x
     *)
    val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
  end
  module type MONAD_S = sig
    type 'a m
    val liftM2 : ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m
  end
end
 
module Monad : functor (S:Signatures.MONAD)
  -> (Signatures.MONAD_S with type 'a m = 'a S.m)

module Maybe : sig

  (**
   * Filter out None elements, and keep others.
   *)
  val cat_maybes : 'a option list -> 'a list

  (**
   * Add a default value.
   *     from_maybe defval  None    = defval
   *     from_maybe defval (Some x) = x
   *)
  val from_maybe : 'a -> 'a option -> 'a

  (**
   * Apply a function - or not.
   *     fmap f  None    = None
   *     fmap f (Some x) = Some (f x)
   *)
  val fmap : ('a -> 'b) -> 'a option -> 'b option

  (**
    * Functional counterpart of pattern-matching.
    *     maybe d f  None    = d
    *     maybe d f (Some x) = f x
    *)
  val maybe : 'b -> ('a -> 'b) -> 'a option -> 'b

  type 'a m = 'a option

  val (>>=) : 'a option -> ('a -> 'b option) -> 'b option

  val return : 'a -> 'a option

end

module Arrow : sig
  val first  : ('a1 -> 'a2) -> ('a1 * 'b) -> ('a2 * 'b)
  val second : ('b1 -> 'b2) -> ('a * 'b1) -> ('a * 'b2)
end

