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

type var =
  | Local of int
  | Global of string

let var_from_prog = function
  | Prog.L n -> Local  n
  | Prog.G s -> Global s
  | _ -> invalid_arg "var_from_prog"
  
let prog_from_var = function
  | Local  n -> Prog.L n
  | Global s -> Prog.G s

module Alist : sig
  (** Association list *)
  type 'a t
  val empty : 'a Domain.c_dom -> 'a t
  val singleton : 'a Domain.c_dom -> Prog.lval -> 'a -> 'a t
  val equal : 'a t -> 'a t -> bool
  val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t option
  val replace : Prog.lval -> ('a -> 'a) -> 'a t -> 'a t option
  val map : (Prog.lval -> 'a -> 'b) -> 'a t -> 'b list
  val assoc : Prog.lval -> 'a t -> 'a
end = struct
  (** Invariants : - list is sorted according to varcmp
   *               - there are no "top" elements
   *)
  type 'a t = { dom: 'a Domain.c_dom
              ; lst: (var * 'a) list
              }

  let empty dom = { dom = dom ; lst = [] }

  let singleton dom k x =
    let k' = var_from_prog k in
    { lst = (k', x)::[] ; dom = dom }

  let equal a b =
    a.lst = b.lst

  let merge f x1 x2 =
    let dom = x1.dom in
    let rec merge_l x1 x2 =
      match (x1, x2) with
      | [], _  -> Some x2
      | _ , [] -> Some x1
      | (s1,r1)::t1, (s2,r2)::t2 ->
          match varcmp s1 s2 with
          | 0 -> begin
                   let r = f r1 r2 in
                   if r = dom.bottom then None
                   else if r = dom.top
                   then         merge_l t1 t2
                   else
                     may_cons (s1, r) (merge_l t1 t2)
                 end
          | x when x < 0 -> may_cons (s1,r1) (merge_l t1 x2)
          | _  (* > 0 *) -> may_cons (s2,r2) (merge_l x1 t2)
    in
    Utils.may (fun l -> { dom = dom ; lst = l })
              (merge_l x1.lst x2.lst)

  let replace var f x =
    let dom = x.dom in
    let rec repl_l = function
    | [] -> Some []
    | (s, r)::t -> match varcmp s (var_from_prog var) with
      | 0 -> begin
               let fr = f r in
               if fr = dom.bottom
                 then None
                 else
                   if fr = dom.top
                     then Some t
                     else Some ((s, fr)::t)
             end (* XXX may *)
      | x when x < 0 -> may_cons (s, r) (repl_l t)
      | _ (* > 0 *)  -> Some ((s, r)::t)
    in
    Utils.may (fun l -> { dom = dom ; lst = l })
              (repl_l x.lst)

  let map f x =
    List.map (fun (k, x) -> f (prog_from_var k) x) x.lst

  let assoc v x =
    let dom = x.dom in
    let rec assoc_l = function
    | (v', x)::_ when (var_from_prog v) = v'          -> x
    | (v', _)::t when varcmp (var_from_prog v) v' > 0 -> assoc_l t
    | _ -> dom.top
    in
    assoc_l x.lst

end

open Utils.Lift

type 'a box = { store : 'a Alist.t ; esp : int }

type 'a t = 'a box lift

let equal a b = match (a, b) with
  | None, None -> true
  | Some a, Some b ->    a.esp = b.esp
                      && Alist.equal a.store b.store
  | _ -> false

let top dom = Some {store = Alist.empty dom ; esp = 0}

let bottom = None

let update_store fs x =
  bind (fun s ->
    return { x with store = s }
  ) fs

let bind_store f x y =
  update_store (f x.store y.store) x

let bind2_l f x y =
  maybe y (fun x' ->
  maybe x (fun y' ->
    bind_store f x' y'
  ) y
  ) x

let bind2' f =
  bind2 (bind_store f)

let join  dom = bind2_l (Alist.merge dom.join)
let meet  dom = bind2'  (Alist.merge dom.meet)
let widen dom = bind2'  (Alist.merge dom.widen)

let singleton dom v r =
  Some { store = Alist.singleton dom v r ; esp = 0 }

let guard var f =
  bind (fun x ->
  update_store
    (Alist.replace var f (x.store)) x
  )

let adjust_esp esp =
    function
    | Prog.L n -> Prog.L (esp - n)
    | x -> x

let set_var dom v r =
  bind (fun x ->
    update_store
    (Alist.merge (fun a _ -> a)
                 (Alist.singleton dom (adjust_esp x.esp v) r)
                 x.store
      ) x
  )

let get_var dom v =
  maybe dom.bottom
    (fun x ->
       Alist.assoc (adjust_esp x.esp v)
                   x.store
    )

let push _dom =
  bind (fun x ->
      return { x with esp = x.esp + 1 }
  )

let pop dom =
  bind (fun x ->
  bind (fun s' ->
        Some { store = s' ; esp = x.esp - 1 }
        ) (Alist.replace (Prog.L x.esp)
          (fun _ -> dom.top) x.store
      )
  )

let to_string dom =
  maybe "(bot)"
    (fun x -> String.concat ", " (Alist.map (fun v r ->
                Pcomp.Print.lval v^"->"^dom.to_string r)
              x.store))

let yaml_dump dom =
  maybe "bottom: yes"
    (fun x -> "value: {" ^(String.concat ", " (Alist.map (fun v r ->
      Pcomp.Print.lval v ^": \""^dom.to_string r^"\"") x.store))
      ^"}"
    )
