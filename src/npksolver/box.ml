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
  val singleton : ?env:(Prog.lval -> 'a) -> 'a Domain.c_dom -> Prog.lval -> 'a -> 'a t
  val equal : 'a t -> 'a t -> bool
  val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t option
  val replace : Prog.lval -> ('a -> 'a) -> 'a t -> 'a t option
  val map : (Prog.lval -> 'a -> 'b) -> 'a t -> 'b list
  val assoc : (Prog.lval -> 'a) -> Prog.lval -> 'a t -> 'a
end

module VMap : STORE = struct
  type 'a t =
    { dom : 'a Domain.c_dom
    ; map : (Prog.var, 'a) Pmap.t
    }

  let empty_map () = Pmap.create varcmp

  let empty dom =
    { dom = dom
    ; map = empty_map ()
    }

  let singleton ?env dom lv x =
    { dom = dom
    ; map = Pmap.add (Pcomp.to_var ?env dom lv) x (empty_map ())
    }

  let assoc env lv x =
    try
      Pmap.find (Pcomp.to_var ~env x.dom lv) x.map
    with Not_found -> x.dom.top

  let map f x =
    Pmap.foldi (fun k v l -> (f (Pcomp.from_var k) v)::l) x.map []

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
              ; size  : (Prog.var, int) Pmap.t
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
       ; size = Pmap.empty
       }

let bottom = None

let update_store so x =
  so >>= fun s ->
  return { x with store = s }

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

let meet  dom = bind2'  (S.merge dom.meet)
let widen dom = bind2'  (S.merge dom.widen)

let singleton dom v ~size r =
  Some { store = S.singleton dom v r
       ; esp = 0
       ; size = Pmap.add (Pcomp.to_var dom v) size Pmap.empty
       }

let guard var f =
  bind (fun x ->
  update_store
    (S.replace var f (x.store)) x
  )

let adjust_esp esp =
    function
    | Prog.L n -> Prog.L (esp - n)
    | x -> x

let rec environment dom bx lv =
  maybe dom.bottom
        (fun x -> S.assoc (environment dom bx)
          (adjust_esp x.esp lv) x.store)
        bx

let set_var dom v r bx =
  bind (fun x ->
    update_store
    (S.merge (fun a _ -> a)
              (S.singleton ~env:(environment dom bx) dom (adjust_esp x.esp v) r)
              x.store
      ) x
  ) bx

let push _dom =
  bind (fun x ->
      return { x with esp = x.esp + 1 }
  )

let pop dom =
  bind (fun x ->
  bind (fun s' ->
      Some { store = s' ; esp = x.esp - 1 ; size = x.size }
        ) (S.replace (Prog.L x.esp)
          (fun _ -> dom.top) x.store
      )
  )

let get_size x var = match x with
  | None -> invalid_arg "get_size : bottom"
  | Some x ->
      try
        Pmap.find var x.size
      with Not_found ->
        invalid_arg ( "get_size : cannot find variable '"
                    ^ Pcomp.Print.var var
                    ^ "'" )

let to_string dom =
  maybe "(bot)"
    (fun x -> String.concat ", " (S.map (fun v r ->
                Pcomp.Print.lval v^"->"^dom.to_string r)
              x.store))

let yaml_dump dom =
  maybe "bottom: yes"
    (fun x -> "value: {" ^
      (String.concat ", "
        (Utils.filter_list
          (S.map
            (fun v r ->
              if r = dom.top then None
              else Some (
                  Pcomp.Print.lval v
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
