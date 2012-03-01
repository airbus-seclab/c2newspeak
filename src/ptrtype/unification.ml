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

open Types

type 'a combine_result =
  | InBoth of 'a * 'a
  | OnlyL of 'a
  | OnlyR of 'a

module Int = struct
  type t = int
  let compare (x:int) y = Pervasives.compare x y
end

module IntMap = Map.Make (Int) 

let insert_or_update
   : ('a option -> 'a)
  -> IntMap.key
  -> 'a IntMap.t
  -> 'a IntMap.t
  = fun f k m ->
    let v =
      try
        Some (IntMap.find k m)
      with Not_found ->
        None
    in
    IntMap.add k (f v) m

let compare_lists a b =
  let add_l v = function
    | None -> OnlyL v
    | _ -> invalid_arg "add_l"
  in
  let add_r w = function
    | None -> OnlyR w
    | Some (OnlyL v) -> InBoth (v, w)
    | _ -> invalid_arg "add_r"
  in
  let after_a =
    List.fold_left (fun m (k,v) -> insert_or_update (add_l v) k m) IntMap.empty a
  in
  let after_b =
    List.fold_left (fun m (k,w) -> insert_or_update (add_r w) k m) after_a b
  in
  IntMap.bindings after_b

let occurs n t = List.mem n (vars_of_typ t)

let fail reason ta tb =
  let sa = string_of_simple ta in
  let sb = string_of_simple tb in
  Utils.error (Printf.sprintf "%s :\n  %s\n  %s\n" reason sa sb)

let occurs_check_failed ta tb =
  fail "Occurs check failed - cannot unify" ta tb

let type_clash ta tb =
  fail "Type clash between" ta tb

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
  | ((Var ({contents = Unknown {id = n}} as r)), t)
    ->
      begin
        if occurs n t then
          occurs_check_failed sta stb
        else
          r := Instanciated t
      end
  | (_, (Var ({contents = Unknown _}))) -> unify_now stb sta

  | Int, Int
  | Float, Float -> ()

  | Ptr a, Ptr b
  | Array a, Array b -> unify_now a b

  | Fun (args_a, rets_a), Fun (args_b, rets_b) ->
      List.iter2 unify_now args_a args_b;
      List.iter2 unify_now rets_a rets_b

  | Struct rfa, Struct rfb ->
      let fa = !rfa in
      let fb = !rfb in

      let new_a = ref [] in
      let new_b = ref [] in

      let unify_fields = function
        | _, InBoth (ta, tb) -> unify_now ta tb
        | k, OnlyL f -> new_b := (k,f) :: !new_b
        | k, OnlyR f -> new_a := (k,f) :: !new_a
      in

      List.iter unify_fields (compare_lists fa fb);
      let by_offset (x, _) (y, _) =
        compare x y
      in
      rfa := List.sort by_offset (!new_a @ !rfa);
      rfb := List.sort by_offset (!new_b @ !rfb)

  | _ -> type_clash sta stb

let unify_queue = Queue.create ()

let unify_do tpk =
  Queue.iter (fun (a, b) ->
    if !Options.show_steps then begin
      Tyspeak.dump string_of_simple tpk;
      print_endline (String.make 50 '-')
    end;
    unify_now a b
  ) unify_queue;
  Queue.clear unify_queue

let unify a b =
  Queue.add (a, b) unify_queue
