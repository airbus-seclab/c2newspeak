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

open Domain

open Utils.Maybe

module type STORE = sig
  type 'a t
  val empty : 'a Domain.t -> 'a t
  val singleton : 'a Domain.t -> Prog.addr -> 'a -> 'a t
  val equal : 'a t -> 'a t -> bool
  val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t option
  val replace : Prog.addr -> ('a -> 'a) -> 'a t -> 'a t option
  val map : (Prog.addr -> 'a -> 'b) -> 'a t -> 'b list
  val assoc : Prog.addr -> 'a t -> 'a
  val dom : 'a t -> 'a Domain.t
end

module VMap : STORE = struct
  open Utils.Maybe

  type 'a t =
    { dom : 'a Domain.t
    ; map : (Prog.addr, 'a) Pmap.t
    }

  let empty dom =
    { dom = dom
    ; map = Pmap.empty
    }

  let singleton dom addr x =
    { dom = dom
    ; map = Pmap.add addr x Pmap.empty
    }

  let assoc addr x =
    try
      Pmap.find addr x.map
    with Not_found -> x.dom.top

  let map f x =
    Pmap.foldi (fun k v l -> (f k v)::l) x.map []

  let equal a b =
    Pmap.equal (=) a.map b.map

  let merge f a b =
    let map = 
      Pmap.foldi (fun k v x ->
        x >>= fun m ->
        try
          let res = f v (Pmap.find k m) in
          if res = a.dom.bottom
            then None
            else Some (Pmap.add k res m)
        with Not_found ->
          Some (Pmap.add k v m)
      ) a.map (Some b.map)
    in
    fmap (fun m -> { a with map = m }) map

  let replace v f x =
    let sg = singleton x.dom v x.dom.top in
    merge (fun a1 _a2 -> f a1) x sg

  let dom x = x.dom

end

module S = VMap

type 'a box =
  { store : 'a S.t
  ; esp   : int
  ; typ   : (Prog.addr, Prog.typ) Pmap.t
  }

type 'a t = 'a box option

let equal a b = match (a, b) with
  | None, None -> true
  | Some a, Some b ->    a.esp = b.esp
                      && S.equal a.store b.store
  | _ -> false

(*
 * Initial value for typ.
 * The initial value, at Stack 0 is the return value of "main".
 *)
let initial_typ =
  Pmap.add (Prog.Stack 0) Prog.Int Pmap.empty

let top dom =
  Some { store = S.empty dom
       ; esp   = 0
       ; typ   = initial_typ
       }

let bottom = None

let join xo yo =
  match (xo, yo) with
    | None   , _      -> yo
    | _      , None   -> xo
    | Some x , Some y ->
        fmap ( fun s -> { x with store = s
                        ;        typ   = Pmap.merge x.typ y.typ
                        })
             (S.merge (S.dom x.store).join x.store y.store)

let meet xo yo =
  xo >>= fun x ->
  yo >>= fun y ->
  fmap (fun s -> { x with store = s
                 ;         typ  = Pmap.merge x.typ y.typ
                 })
       (S.merge ((S.dom x.store).meet) x.store y.store)

let widen xo yo =
  xo >>= fun x ->
  yo >>= fun y ->
  fmap (fun s -> { x with store = s
                 ;         typ  = Pmap.merge x.typ y.typ
                 })
       (S.merge ((S.dom x.store).widen) x.store y.store)

let typeof x addr = match x with
  | None -> invalid_arg "typeof : bottom"
  | Some x ->
      try
        Pmap.find addr x.typ
      with Not_found ->
        invalid_arg ( "typeof : cannot find variable '"
                    ^ Pcomp.Print.addr addr
                    ^ "'" )

let get_size x addr =
  let ty = typeof x addr in
  Pcomp.size_of_typ ty

let rec eval_with_box x e =
  let dom = S.dom x.store in
  dom.eval (environment (Some x))
           (addr_of     (Some x))
           e

and addr_convert ?(check=fun _ _ _ -> ()) x =
  let abort msg =
    print_endline msg;
    exit 4
  in
  let top_ptr_deref () =
    abort "Top pointer dereference, cannot continue analysis."
  in
  let null_ptr_deref loc =
    Alarm.emit (loc, Alarm.Null_deref, None);
    (* TODO
     * Instead of exiting, it is possible to recover : for example, ignore
     * the set_var or guard directive. However, it is not sound.
     *)
    abort "Cannot continue analysis."
  in
  let dom = S.dom x.store in
  let esp = x.esp in
  function
    | Prog.L n -> Prog.Stack (esp - n)
    | Prog.G s -> Prog.Heap  s
    | Prog.Shift (l, e, loc) ->
        let addr_base = addr_convert x l in
        check e addr_base loc;
        addr_base
    | Prog.Deref (e, _sz, loc) ->
        begin
          let (r, alrms) = eval_with_box x e in
          List.iter Alarm.emit alrms;
          match dom.where_does_it_point r with
            | Where_on_null -> null_ptr_deref loc
            | Where_I_dont_know -> top_ptr_deref ()
            | Where_nowhere -> invalid_arg "box ∷ addr_convert ∷ Not a pointer"
            | Where_on (addr, off) ->
                if not (Interval.(<=%) off (Interval.with_size (get_size (Some x) addr))) then
                  Alarm.emit (loc, Alarm.Ptr_bad_deref, None);
                addr (* FIXME it should actually return addr+off *)
        end

and environment bx v =
  let addr = addr_of bx v in
  match bx with
  | Some x -> S.assoc addr x.store
  | None -> invalid_arg "environment : bottom"

and addr_of_ck ?check xo l =
  match xo with
  | None -> invalid_arg "box.addr_of : no values"
  | Some x -> addr_convert ?check x l

and addr_of xo l = addr_of_ck xo l

let singleton dom v ~typ r =
  let zero_store =
    { esp = 0
    ; store = S.empty dom
    ; typ = Pmap.empty
    } in
  let addr = addr_convert zero_store v in
  Some { store = S.singleton dom addr r
       ; esp   = 0
       ; typ   = Pmap.add addr typ initial_typ
       }

let guard var f xo =
  let addr = addr_of xo var in
  xo >>= fun x ->
  fmap (fun s -> { x with store = s }) (S.replace addr f (x.store))

(*
 * TODO 
 * could it be written as :
 *     guard v (fun _ -> r) bx
 * (+ checks) ?
 *)
let set_var v r bx =
  bx >>= fun x ->
  let dom = S.dom x.store in
  let check e a loc =
    let (r, alrms) = eval_with_box x e in
    List.iter Alarm.emit alrms;
    let size = get_size bx a in
    if not (dom.is_in_range 0 size r) then
      Alarm.emit ( loc
                 , Alarm.Array_OOB
                 , Some (dom.to_string r^" </= [0;"^string_of_int size^"]"))
  in
  let addr = addr_of_ck ~check bx v in
  let merge_stores new_val old_val =
    match Pmap.find addr x.typ with
      | Prog.Ptr | Prog.Int -> new_val
      | Prog.Array _ -> old_val (* FIXME it's not updated at all *)
  in
    fmap (fun s -> { x with store = s })
         ( S.merge merge_stores
                   (S.singleton dom addr r)
                    x.store)

let push ~typ =
  fmap (fun x ->
          { x with esp = succ x.esp
          ; typ = Pmap.add (Prog.Stack (succ x.esp)) typ x.typ
          }
       )

let pop xo =
  xo >>= fun x ->
  let dom = S.dom x.store in
  S.replace (Prog.Stack x.esp) (fun _ -> dom.top) x.store
    >>= fun s' ->
    Some { x with store = s' ; esp = pred x.esp } (* XXX remove Local esp *)

let to_string xo =
  maybe "(bot)"
    (fun x -> String.concat ", " (S.map (fun v r ->
                Pcomp.Print.addr v^"="^(S.dom x.store).to_string r)
              x.store))
    xo

let dump_yaml = function
  | None -> None
  | Some x ->
      let dom = S.dom x.store in
      let values = 
        cat_maybes
          (S.map
            (fun v r ->
              if r = dom.top then None
              else Some ( Pcomp.Print.addr v, Yaml.String (dom.to_string r))
            ) x.store
          )
      in
      Some (Yaml.Dict values)
