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

type fp_algorithm =
  | Roundrobin
  | Worklist

let fp_alg_str = function
  | "rr" -> Roundrobin
  | "wl" -> Worklist
  | s    -> invalid_arg ("no such fixpoint algorithm : "^s)

type 'a control = 'a ref

type 'a c_control = (string -> 'a) * 'a control

let set c x =
  c := x

let get c =
  !c

let set_cc (f,r) s =
  set r (f s)

let get_cc (_,r) =
  get r

let widening = ref false
let verbose = ref false
let cfg_only = ref false
let graphviz = ref false
let solver = ref false

type opt_action =
  | Help
  | Set  of bool control
  | Call of (unit -> unit)
  | Carg of (string -> unit)

type opt = char       (* Short *)
         * string     (* Long  *)
         * string     (* Help string *)
         * opt_action

let parse_cmdline o handle argv =
  let display_help _ =
    print_endline ("Usage : " ^ argv.(0) ^ " file.npk");
    List.iter (fun (s, l, h, _) ->
      print_endline ("-"^(String.make 1 s)^" / --"^l^" : "^h)
    ) o in
  if (Array.length argv = 1) then
    display_help ();
  let missing =
  Array.fold_left (fun wait arg ->
    if ((String.sub arg 0 1) = "-") then
      begin
        if wait <> None then
          invalid_arg "Argument required";
        let m = List.find_all (fun (s, l, _, _) ->
                         arg = "--"^l || arg = "-"^String.make 1 s) o in
        match m with
        | (_,_,_,Help  )::[] -> display_help (); None
        | (_,_,_,Call f)::[] -> f () ; None
        | (_,_,_,Carg f)::[] -> Some f
        | (_,_,_,Set c)::[]  -> set c true; None
        | _::_::_ -> invalid_arg "Several matches"
        | [] -> invalid_arg "No such option"
      end
    else
      begin
        match wait with
        | None   -> handle arg ; None
        | Some f -> f arg ; None
      end
  ) (Some (fun _ -> ())) argv in
  match missing with None   -> ()
                   | Some _ -> invalid_arg "Argument required"

