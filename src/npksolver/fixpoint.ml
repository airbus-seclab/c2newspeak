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

type 'a worklist_action =
  | WA_ATF of ('a -> 'a) (* abstract transfer function *)
  | WA_Widen

let rec eval_stmt cs (dom:'a Domain.t) loc stmt
      : 'a Box.t worklist_action
      =
  match stmt with
  | Cfg.Nop -> WA_ATF (fun x -> x)
  | Cfg.Reloop -> WA_Widen
  | Cfg.Init vs ->
          WA_ATF (fun x ->
            Pmap.foldi (fun v typ r ->
                Box.meet (Box.singleton
                             dom ~typ
                             (Prog.G v)
                             (nil dom ~typ)
                         )
                         r
              ) vs x
          )
  | Cfg.Pop      -> WA_ATF (fun x -> Box.pop x)
  | Cfg.Push typ -> WA_ATF (fun x -> Box.push x ~typ)
  | Cfg.Guard e ->
          WA_ATF (fun x ->
            let env = Box.environment x in
            List.fold_left (fun x (v, f) ->
              Box.guard v f x
            ) x (dom.guard env (Box.addr_of x) e)
          )
  | Cfg.Set (lv, e, loc) ->
      WA_ATF (fun x ->
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
      )
  | Cfg.Assert_true e ->
      WA_ATF (fun x ->
        let lookup = Box.environment x in
        let (r, alrms) = dom.eval lookup (Box.addr_of x) e in
        List.iter Alarm.emit alrms;
        let has_zero x =
          dom.join x (const dom 0) = x
        in
        if (has_zero r) then
          Alarm.emit ( loc
                     , Alarm.Assertion_failed (Pcomp.Print.exp e)
                     , Some ("0 ∈ " ^ dom.to_string r)
                     );
        x
      )
  | Cfg.Call (f, id) ->
      WA_ATF (fun x -> Stack.push (f, id) cs; x)

(* worklist algorithm *)
let f_horwitz (dom:'abs Domain.t) funcs x =
  let callstack = Stack.create () in
  let succ f i =
      let (_,func) = Pmap.find f funcs in
      List.map (fun (dest, stmt, loc) ->
       if Options.get Options.verbose then begin
         Printf.eprintf "Refreshing %s:%d : \n" (fst dest) (snd dest);
         prerr_endline (Mkcfg.print_stmt stmt)
       end;
       let g = eval_stmt callstack dom loc stmt in
       (dest, g)
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
          [exit, WA_ATF (fun x -> x)]
        with Stack.Empty ->
          []
        end
      else
        try
          succ f i
        with
          Not_found -> invalid_arg (f^"+"^string_of_int i^" has no successor")
    in
    if Options.get Options.verbose then
      prerr_endline (Resultmap.to_string Box.to_string x);
    List.iter (fun ((nextf, j), fij) ->
      let xj = Resultmap.get x nextf j in
      let xi = Resultmap.get x f     i in
      let r = match fij with
        | WA_ATF f -> Box.join xj (f xi)
        | WA_Widen ->
            if Options.get Options.widening
              then Box.widen xj xi
              else Box.join  xj xi
      in
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
