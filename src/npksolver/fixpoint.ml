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

let eval_stmt dom stmt x = match stmt with
  | Cfg.Nop -> x
  | Cfg.Init vs ->
          List.fold_left (fun r v ->
              Box.meet dom
                       (Box.singleton dom (Prog.G v) (dom.from_val 0))
                       r
            ) x vs
  | Cfg.Pop  -> Box.pop  dom x
  | Cfg.Push -> Box.push dom x
  | Cfg.Guard e ->
            List.fold_left (fun x (v, f) ->
              Box.guard v f x
            ) x (dom.guard e)
  | Cfg.Set (v, e) ->
          begin
            let lookup v = Box.get_var dom v x in
            if x = Box.bottom
              then Box.bottom
              else Box.set_var dom v (dom.eval lookup e) x
          end

(* worklist algorithm *)
let f_horwitz dom v x =
  let succ i =
    try
      List.map (fun (dest,stmt) ->
       let g = eval_stmt dom stmt in
       let r =
         if Options.get Options.widening then
           fun xo -> Box.widen dom xo (g xo)
         else
           g
       in (dest, r)
      ) (Cfg.NodeMap.find i v)
    with Not_found -> []
  in
  let worklist = Queue.create () in
  Queue.add (Array.length x - 1) worklist;
  while (not (Queue.is_empty worklist)) do
    let i = Queue.take worklist in
    List.iter (fun (j, fij) ->
      let r = Box.join dom x.(j) (fij x.(i)) in
      if (not (Box.equal r x.(j))) then
        begin
          Queue.add j worklist;
          x.(j) <- r
        end
    ) (succ i)
  done;
  x

let compute_warn watchpoints dom results =
  let intvl dom a b =
    dom.join (dom.from_val a)
             (dom.from_val b)
  in
  begin
  if Options.get Options.verbose then
    Printf.fprintf stderr "Watchpoint list : %s\n"
      (String.concat "," (List.map (fun (_,x,_) -> string_of_int x)
      watchpoints))
  end;
  List.iter (function
  | loc, l, Prog.AFalse ->
      if results.(l) <> Box.bottom then
        print_endline (Newspeak.string_of_loc loc ^ ": Assert false")
  | loc, l, Prog.ABound (v, inf, sup) ->
      let r = Box.get_var dom v results.(l) in
      if not (dom.incl r (intvl dom inf sup)) then
        print_endline (Newspeak.string_of_loc loc ^ ": Bound check")
  | loc, l, Prog.AEq (v, i) ->
      let r = Box.get_var dom v results.(l) in
      begin
      if Options.get Options.verbose then
      Printf.fprintf stderr "eq check : r = %s, bound = {%d}\n" 
        (dom.to_string r) i
      end;
      if not (dom.incl r (dom.from_val i)) then
        print_endline (Newspeak.string_of_loc loc ^ ": Value is different")
  ) watchpoints


let solve wp (ln, v) =
  { Domain.bind = fun dom ->
  let x0 = Array.make (ln + 1) Box.bottom in
  x0.(ln) <- Box.top dom;
  let res = f_horwitz dom v x0 in
  if Options.get Options.solver then begin
    print_endline "---";
    Array.iteri (fun i r ->
        print_endline ("  - {id: "^ string_of_int i ^
        ", "^ Box.yaml_dump dom r^"}")
    ) res end;
  compute_warn wp dom res;
  Array.map (Box.to_string dom) res
  }
