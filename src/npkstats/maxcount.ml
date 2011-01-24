(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007, 2011  Charles Hymans, Olivier Levillain, Sarah Zennou
  
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

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org

  Olivier Levillain
  email: olivier.levillain@penjili.org

  Sarah Zennou
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: sarah(dot)zennou(at)@eads(dot)net
*)

open Lowspeak

type stats = {
  nb_vars: int;
  sz_vars: int;
  call_depth: int;
  loop_depth: int;
}

type t = bool * stats

let add_stats st1 st2 =
  { 
    nb_vars    = st1.nb_vars + st2.nb_vars;
    sz_vars    = st1.sz_vars + st2.sz_vars;
    call_depth = st1.call_depth + st2.call_depth;
    loop_depth = st1.loop_depth + st2.loop_depth;
  }

let max_stats st1 st2 =
  { 
    nb_vars = max st1.nb_vars st2.nb_vars;
    sz_vars = max st1.sz_vars st2.sz_vars;
    call_depth = max st1.call_depth st2.call_depth;
    loop_depth = max st1.loop_depth st2.loop_depth;
  }

let init_stats = { nb_vars = 0; sz_vars = 0; call_depth = 0; loop_depth = 0; }

let count_decl ptr_sz t st =
  let nb = st.nb_vars + 1 in
  let sz = st.sz_vars + ((Newspeak.size_of ptr_sz t) / 8) in 
    { st with nb_vars = nb; sz_vars = sz; }

let count_loop st = { st with loop_depth = st.loop_depth + 1 }

let count_call st = { st with call_depth = st.call_depth + 1 }

let rec_fun = ref ""

let count debug prog =
  let fid_addrof   = Lowspeak.collect_fid_addrof prog in
  let unknown_funs = ref [] in
  let exact 	   = ref true in
  let fun_tbl 	   = Hashtbl.create 100 in
  let current_loc  = ref Newspeak.unknown_loc in
  let fset 	   = ref [] in

  let rec process_call f =
    try Hashtbl.find fun_tbl f
    with Not_found -> 
      if debug then print_endline ("counting stack height of "^f);
      let height = 
        try 
          let declaration = Hashtbl.find prog.fundecs f in
            process_blk declaration.body init_stats
        with Not_found -> 
          if not (List.mem f !unknown_funs) then begin
            unknown_funs := f::!unknown_funs;
            prerr_endline ("Warning: function "^f^" body not defined")
          end;
          init_stats
      in
      let height = count_call height in
        Hashtbl.add fun_tbl f height;
        height
              
  and process_blk x info =
    match x with
        (stmt, loc)::blk -> 
          current_loc := loc;
          let info1 = process_stmt stmt info in
          let info2 = process_blk blk info in
            max_stats info1 info2
      | [] -> info
          
  and process_stmt x info =
    match x with
      | Decl (_, t, body) -> 
          let info = count_decl prog.ptr_sz t info in
            process_blk body info
      | DoWith (body, _) -> process_blk body info
      | Call (FunId f) -> 
          if List.mem f !fset then 
            begin
              rec_fun := f; 
              info
            end
          else 
            begin
	      let b = Hashtbl.mem prog.fundecs f in
              if b then fset := f::!fset;
              let info_f = process_call f in
                if b then fset := List.tl !fset;
                add_stats info info_f
            end
      | Call _ -> 
          let build_call f = (Call (FunId f), !current_loc)::[] in
          let alternatives = List.map build_call fid_addrof in
            if alternatives <> [] then exact := false;
            List.fold_left (process_alternatives info) init_stats alternatives
      | Select (lblk, rblk) -> 
          let stats = process_alternatives info init_stats lblk in
            process_alternatives info stats rblk
      | InfLoop body -> 
          let info = count_loop info in
            process_blk body info
      | _ -> info

  and process_alternatives info max_so_far body =
    let info' = process_blk body info in
      max_stats info' max_so_far
  in
    (* TODO: arguments of main not counted ! *)
  let stats = process_call "main" in
    (!exact, stats)
      
let print (exact, st) xout =
  let symb 	 = if exact then "" else "<= " in
  let nb 	 = string_of_int st.nb_vars in
  let sz 	 = string_of_int st.sz_vars in
  let call_depth = string_of_int st.call_depth in
  let loop_depth = string_of_int st.loop_depth in
  let s1 	 = "Maximum number of variables on the stack" in
  let s2 	 = "Maximum height of the stack (bytes)" in
  let s3 	 = "Maximum depth of function calls" in
  let s4 	 = "Maximum depth of imbricated loops" in
  let s5 	 = "At least one recursive function" in
    print_endline (s1^": "^symb^nb);
    print_endline (s2^": "^symb^sz);
    print_endline (s3^": "^symb^call_depth);
    print_endline (s4^": "^symb^loop_depth);
    if !rec_fun <> "" then
      print_endline (s5^": "^(!rec_fun));
    match xout with
        None -> ()
      | Some cout ->
          let s =
            List.fold_left (fun s (t, c, n) ->
                              s^"<stats type=\""^t^"\" class=\""^c^"\" val=\""^symb^n^"\"></stats>\n"
                           ) "" [("analysis", s1, nb) ; ("analysis", s2, sz) ; 
                                 ("Functions", s3, call_depth) ; ("Statements", s4, loop_depth)]
          in
          let s =
            if !rec_fun<> "" then
              s^"<stats type=\"warning\" class=\""^s5^"\" val=\""^(!rec_fun)^"\"></stats>\n"
            else s
          in
            output_string cout s

