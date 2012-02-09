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

open Simple_types

let occurs n t = List.mem n (vars_of_typ t)

let occurs_check_failed ta tb =
  let sa = string_of_simple ta in
  let sb = string_of_simple tb in
  failwith ("Occurs check failed : cannot unify "^sa^" and "^sb)

let is_atomic_type = function
  | Int
  | Float -> true
  | _ -> false

let type_clash ta tb =
  let sa = string_of_simple ta in
  let sb = string_of_simple tb in
  print_endline ("Type clash between :\n  "^sa^"\n  "^sb);
  exit 1

let rec unify_now ta tb =
  let sta = shorten ta in
  let stb = shorten tb in
  match (sta, stb) with
  | ((Var ({contents = Unknown na} as ra)),
     (Var  {contents = Unknown nb})) ->
       begin
         if na <> nb then
           ra := Instanciated stb
       end
  | ((Var ({contents = Unknown n} as r)), t)
    ->
      begin
        if occurs n t then
          occurs_check_failed sta stb
        else
          r := Instanciated t
      end
  | (_, (Var ({contents = Unknown _}))) -> unify_now stb sta
  | _ when is_atomic_type sta && sta = stb -> ()

  | Ptr a, Ptr b
  | Array a, Array b -> unify_now a b

  | Fun (args_a, rets_a), Fun (args_b, rets_b) ->
      List.iter2 unify_now args_a args_b;
      List.iter2 unify_now rets_a rets_b
    
  | _ -> type_clash sta stb

let unify_queue = Queue.create ()

let unify_do show_steps tpk =
  Queue.iter (fun (a, b) ->
    if show_steps then begin
      Tyspeak.dump string_of_simple tpk;
      print_endline (String.make 50 '-')
    end;
    unify_now a b
  ) unify_queue;
  Queue.clear unify_queue

let unify a b =
  Queue.add (a, b) unify_queue
