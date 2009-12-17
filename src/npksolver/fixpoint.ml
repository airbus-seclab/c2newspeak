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

let eval_stmt dom loc stmt x = match stmt with
  | Cfg.Nop -> x
  | Cfg.Init vs ->
          let zero = const dom 0 in
          List.fold_left (fun r (v, size) ->
              Box.meet dom
                       (Box.singleton dom ~size (Prog.G v) zero)
                       r
            ) x vs
  | Cfg.Pop       -> Box.pop  dom x
  | Cfg.Push size -> Box.push dom x ~size
  | Cfg.Guard e ->
            let env = Box.environment dom x in
            List.fold_left (fun x (v, f) ->
              Box.guard v f x
            ) x (dom.guard env (Box.addr_of x) e)
  | Cfg.Set (lv, e, loc) ->
          begin
            let lookup = Box.environment dom x in
            if x = Box.bottom then Box.bottom
              else
              begin
                let new_value = dom.eval lookup (Box.addr_of x) e in
                begin
                  match dom.update with
                  | None -> ()
                  | Some check -> check (Box.get_size x) loc new_value
                end;
                Box.set_var dom lv new_value x
              end
          end
  | Cfg.Assert_true e ->
        let lookup = Box.environment dom x in
        let r = dom.eval lookup (Box.addr_of x) e in
        let zero = const dom 0 in
        if (dom.incl zero r) then
          Alarm.emit loc (Alarm.Assertion_failed (Pcomp.Print.exp e));
        x

(* worklist algorithm *)
let f_horwitz dom v x =
  let succ i =
    try
      List.map (fun (dest, stmt, loc) ->
       let g = eval_stmt dom loc stmt in
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

let solve dom (ln, v) =
  Domain.with_dom dom { Domain.bind = fun dom ->
  let x0 = Array.make (ln + 1) Box.bottom in
  x0.(ln) <- Box.top dom;
  let res = f_horwitz dom v x0 in
  if Options.get Options.solver then begin
    print_endline "---";
    Array.iteri (fun i r ->
        print_endline ("  - {id: "^ string_of_int i ^
        ", "^ Box.yaml_dump dom r^"}")
    ) res end;
  Array.map (Box.to_string dom) res
  }
