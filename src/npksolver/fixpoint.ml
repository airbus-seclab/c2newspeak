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

let new_value dom x vertices i =
    (* join ( f(origin) / (origin, dest, f) in v / dest = i) *)
    let from_vals = Utils.filter_map (fun (origin, dest, _, stmt) ->
      let f x = match stmt with
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
              begin
                match dom.guard e with
                | None -> x
                | Some (v, f) -> Box.guard v f x
              end
      | Cfg.Set (v, e) ->
              begin
                let lookup v = Box.get_var dom v x in
                if x = Box.bottom
                  then Box.bottom
                  else Box.set_var dom v (dom.eval lookup e) x
              end

      in
      if (dest = i) then (* FIXME style *)
                      begin
                        let xo = x.(origin) in
                        let r =
                          if Options.get Options.widening then
                            Box.widen dom xo (f xo)
                          else
                            f xo
                        in
                        Some r
                      end
                    else None
    ) vertices in
    List.fold_left (Box.join dom) x.(i) from_vals

(* roundrobin algorithm *)
let rec kleene ?(n=0) dom v x =
  let fx = Array.init (Array.length x) (new_value dom x v) in
  if fx = x then x, (n*Array.length x)
  else kleene ~n:(succ n) dom v fx

(* worklist algorithm *)
let f_worklist dom vertices x =
  let worklist = Queue.create () in
  let ops = ref 0 in
  Array.iteri (fun i _ ->
    Queue.add i worklist
  ) x;
  while (not (Queue.is_empty worklist)) do
    incr ops;
    let n = Queue.take worklist in
    let nv = new_value dom x vertices n in
    let ov = x.(n) in
    x.(n) <- nv;
    if (not (Box.equal nv ov)) then
      let successors = Utils.filter_map (fun (src, dst, _, _) ->
        if src == n then Some dst
                    else None
        ) vertices in
        List.iter (fun m -> Queue.add m worklist) successors
  done;
  (x, !ops)

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
  let (res, ops) =
    match Options.get_cc Options.fp_algo with
    | Options.Roundrobin -> kleene dom v x0
    | Options.Worklist   -> f_worklist dom v x0
  in
  if (Options.get Options.verbose) then
    prerr_endline ("FP computed in "^string_of_int ops^" iterations");
  if Options.get Options.solver then begin
    print_endline "---";
    Array.iteri (fun i r ->
        print_endline ("  - {id: "^ string_of_int i ^
        ", "^ Box.yaml_dump dom r^"}")
    ) res end;
  compute_warn wp dom res;
  Array.map (Box.to_string dom) res
  }
