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
 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)

open Domain

let may_cons h t = Utils.may (fun x -> h::x) t

let varcmp = Pervasives.compare

module type STORE = sig
  type 'a t
  val empty : 'a Domain.c_dom -> 'a t
  val singleton : 'a Domain.c_dom -> Prog.addr -> 'a -> 'a t
  val equal : 'a t -> 'a t -> bool
  val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t option
  val replace : Prog.addr -> ('a -> 'a) -> 'a t -> 'a t option
  val map : (Prog.addr -> 'a -> 'b) -> 'a t -> 'b list
  val assoc : Prog.addr -> 'a t -> 'a
end

module VMap : STORE = struct
  type 'a t =
    { dom : 'a Domain.c_dom
    ; map : (Prog.addr, 'a) Pmap.t
    }

  let empty_map () = Pmap.create varcmp

  let empty dom =
    { dom = dom
    ; map = empty_map ()
    }

  let singleton dom addr x =
    { dom = dom
    ; map = Pmap.add addr x (empty_map ())
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
      Pmap.foldi (fun k v ->
        Utils.Lift.bind (fun m ->
        try
          let res = f v (Pmap.find k m) in
          if res = a.dom.bottom
            then None
            else Some (Pmap.add k res m)
        with Not_found ->
          Some (Pmap.add k v m)
        )
      ) a.map (Some b.map)
    in
    Utils.may (fun m -> { a with map = m }) map

  let replace v f x =
    let sg = singleton x.dom v x.dom.top in
    merge (fun a1 _a2 -> f a1) x sg

end

open Utils.Lift

module S = VMap

type 'a box = { store : 'a S.t
              ; esp   : int
              ; size  : (Prog.addr, int) Pmap.t
              }

type 'a t = 'a box option

let equal a b = match (a, b) with
  | None, None -> true
  | Some a, Some b ->    a.esp = b.esp
                      && S.equal a.store b.store
  | _ -> false

let top dom =
  Some { store = S.empty dom
       ; esp = 0
       ; size = Pmap.add (Prog.Stack 0) 32 Pmap.empty
       }

let bottom = None

let bind2' f xo yo =
  xo >>= fun x ->
  yo >>= fun y ->
  f x.store y.store >>= fun s ->
  return { x with store = s
         ;        size  = Pmap.merge x.size y.size
         }

let join dom xo yo =
  match (xo, yo) with
    | None   , _      -> yo
    | _      , None   -> xo
    | Some x , Some y ->
        S.merge dom.join x.store y.store >>= fun s ->
        return
          { x with store = s
          ;        size  = Pmap.merge x.size y.size
          }

let meet  dom x y =
  bind2' (S.merge dom.meet) x y

let widen dom = bind2' (S.merge dom.widen)

let rec addr_convert ?(check=fun _ _ _ -> ()) esp =
  function
    | Prog.L n -> Prog.Stack (esp - n)
    | Prog.G s -> Prog.Heap  s
    | Prog.Shift (l, e, loc) ->
        let addr_base = addr_convert esp l in
        check e addr_base loc;
        addr_base

let addr_of_ck ?check xo l =
  match xo with
  | None -> invalid_arg "box.addr_of : no values"
  | Some x -> addr_convert ?check x.esp l

let addr_of xo l = addr_of_ck xo l

let get_size x addr = match x with
  | None -> invalid_arg "get_size : bottom"
  | Some x ->
      try
        Pmap.find addr x.size
      with Not_found ->
        invalid_arg ( "get_size : cannot find variable '"
                    ^ Pcomp.Print.addr addr
                    ^ "'" )

let singleton dom v ~size r =
  let addr = addr_convert 0 v in
  Some { store = S.singleton dom addr r
       ; esp = 0
       ; size = Pmap.add addr size
               (Pmap.add (Prog.Stack 0) 32 Pmap.empty)
       }

let guard var f xo =
  let addr = addr_of xo var in
  xo >>= fun x ->
  S.replace addr f (x.store) >>= fun s ->
  return { x with store = s }

let rec environment dom bx v =
  let addr = addr_of bx v in
  maybe dom.bottom
        (fun x -> S.assoc addr x.store)
        bx

(*
 * TODO 
 * could it be written as :
 *     guard v (fun _ -> r) bx
 * (+ checks) ?
 *)
let set_var dom v r bx =
  let check e a loc =
    let (r, alrms) = dom.eval (environment dom bx)
                              (addr_of bx)
                              e
    in
    List.iter Alarm.emit alrms;
    let size = get_size bx a in
    if not (dom.is_in_range 0 size r) then
      Alarm.emit ( loc
                 , Alarm.Array_OOB
                 , Some (dom.to_string r^" </= [0;"^string_of_int size^"]"))
  in
  let addr = addr_of_ck ~check bx v in
  bx >>= fun x ->
  (S.merge (fun a _ -> a)
           (S.singleton dom addr r)
            x.store)
  >>= fun s ->
  return { x with store = s }

let push _dom ~size =
  bind (fun x ->
      return { x with esp = succ x.esp
             ; size = Pmap.add (Prog.Stack (succ x.esp)) size x.size
             }
  )

let pop dom =
  bind (fun x ->
  bind (fun s' ->
      Some { store = s' ; esp = pred x.esp ; size = x.size } (* XXX remove Local esp *)
        ) (S.replace (Prog.Stack x.esp)
          (fun _ -> dom.top) x.store
      )
  )

let enter_function _fname x =
  x

let leave_function x =
  x

let caller _x =
  None

let to_string dom =
  maybe "(bot)"
    (fun x -> String.concat ", " (S.map (fun v r ->
                Pcomp.Print.addr v^"->"^dom.to_string r)
              x.store))

let yaml_dump dom =
  maybe "bottom: yes"
    (fun x -> "value: {" ^
      (String.concat ", "
        (Utils.filter_list
          (S.map
            (fun v r ->
              if r = dom.top then None
              else Some ( Pcomp.Print.addr v
                        ^ ": \""
                        ^ dom.to_string r
                        ^ "\""
                        )
            ) x.store
          )
        )
      )
      ^ "}"
    )
