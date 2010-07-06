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

let rec eval_stmt cs dom loc stmt x = match stmt with
  | Cfg.Nop -> x
  | Cfg.Init vs ->
          Pmap.foldi (fun v typ r ->
              Box.meet (Box.singleton
                           dom ~typ
                           (Prog.G v)
                           (const dom 0)
                       )
                       r
            ) vs x
  | Cfg.Pop      -> Box.pop  x
  | Cfg.Push typ -> Box.push x ~typ
  | Cfg.Guard e ->
            let env = Box.environment x in
            List.fold_left (fun x (v, f) ->
              Box.guard v f x
            ) x (dom.guard env (Box.addr_of x) e)
  | Cfg.Set (lv, e, loc) ->
          begin
            let lookup = Box.environment x in
            if x = Box.bottom then Box.bottom
              else
              begin
                let (new_value, alrms_eval) = dom.eval lookup (Box.addr_of x) e in
                let alrms_update =
                  match dom.update with
                  | None -> []
                  | Some check -> check (Box.get_size x) loc new_value
                in
                List.iter Alarm.emit (alrms_eval@alrms_update);
                Box.set_var lv new_value x
              end;
          end 
  | Cfg.Assert_true e ->
        let lookup = Box.environment x in
        let (r, alrms) = dom.eval lookup (Box.addr_of x) e in
        List.iter Alarm.emit alrms;
        let has_zero x =
          dom.join x (const dom 0) = x
        in
        if (has_zero r) then
          Alarm.emit (loc, (Alarm.Assertion_failed (Pcomp.Print.exp e)), None);
        x
  | Cfg.Call (f, id) ->
      Stack.push (f, id) cs;
      x

(* worklist algorithm *)
let f_horwitz dom funcs x =
  let callstack = Stack.create () in
  let succ f i =
      let (_,func) = Pmap.find f funcs in
      List.map (fun (dest, stmt, loc) ->
       let g = eval_stmt callstack dom loc stmt in
       let r =
         if Options.get Options.widening then
           fun xo -> Box.widen xo (g xo)
         else
           g
       in (dest, r)
      ) (Pmap.find (f, i) func)
  in
  let worklist = Queue.create () in
  let main = "main" in
  Queue.add (main, (Resultmap.size x main) - 1) worklist;
  while (not (Queue.is_empty worklist)) do
    let (f, i) = Queue.take worklist in
    let succ_nodes =
      if i = 0 then (* function exit *)
        begin
        try
          let exit = Stack.pop callstack in
          [exit, fun x -> x]
        with Stack.Empty ->
          []
        end
      else
        try
          succ f i
        with
          Not_found -> invalid_arg (f^"+"^string_of_int i^" has no successor")
    in
    List.iter (fun ((nextf, j), fij) ->
      let xj = Resultmap.get x nextf j in
      let xi = Resultmap.get x f     i in
      let r = Box.join xj (fij xi) in
      if (not (Box.equal r xj)) then
        begin
          Queue.add (nextf, j) worklist;
          Resultmap.set x nextf j r
        end
    ) succ_nodes;
  done;
  x

let solve dom funcs =
  let main = "main" in
  let (ln, _) = Pmap.find main funcs in
  let funcs' = Pmap.foldi (fun fname (ln, _) r -> (fname, ln + 1)::r) funcs [] in
  Domain.with_dom dom { Domain.bind = fun dom ->
  let x0 = Resultmap.make funcs' Box.bottom in
  Resultmap.set x0 main ln (Box.top dom);
  let res = f_horwitz dom funcs x0 in
  if Options.get Options.solver then
    begin
      let yaml = Resultmap.fold
                   (fun f n v rs ->
                      let value = 
                      match Box.dump_yaml v with
                        | None   -> ("bottom", Yaml.String "yes")
                        | Some d -> ("value" , d)
                      in
                      (Yaml.Dict
                         ["id"   , Yaml.String (f ^ ":" ^ string_of_int n)
                         ; value
                         ]
                      )::rs
                   ) res []
      in
      print_endline (Yaml.render (Yaml.List yaml))
    end;
  Resultmap.map Box.to_string res
  }
