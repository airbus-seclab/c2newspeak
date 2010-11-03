(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain
  
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
*)

open Npkil

module Nat = Newspeak.Nat

module StrSet = Set.Make(String)

module N = Newspeak

(*--------------*)
(* Linking time *)
(*--------------*)

(* TODO: should make these globals, local to linking, here they eat memory. *)
(* Association table global -> Newspeak.typ *)
let globals = Hashtbl.create 100
let funspecs = Hashtbl.create 100

let scalar_of_typ t =
  match t with
      Scalar t -> t
    | _ -> 
        Npkcontext.report_error "Linker.scalar_of_typ" "scalar type expected"

let get_glob_typ name =
  try
    let (t, _) = Hashtbl.find globals name in
      t
  with Not_found ->
    Npkcontext.report_error "Npklink.get_glob_typ" 
      ("type for global variable "^name^" not found")

let rec generate_typ t =
  match t with
      Scalar x -> Newspeak.Scalar x
    | Array (t, Some l) -> Newspeak.Array (generate_typ t, l)
    | Array (_, None) -> 
        Npkcontext.report_error "Link.generate_typ" "unknown array length"
    | Region (fields, sz) -> 
        Newspeak.Region (List.map generate_field fields, sz)

and generate_field (offs, t) = (offs, generate_typ t)

and generate_ftyp (args, ret) =
  let ret = 
    match ret with
        None -> None
      | Some t -> Some (generate_typ t)
  in
    (List.map generate_typ args, ret)

and generate_init_field (sz, sca, e) = 
  let e = generate_exp e in
    (sz, sca, e)

and generate_lv lv =
  match lv with
    | Global name -> N.Global name
    | Deref (e, sz) -> N.Deref (generate_exp e, sz)
    | Shift (lv', e) -> N.Shift (generate_lv lv', generate_exp e)
    | Local v -> N.Local v
        
and generate_exp e =
  match e with
    | Lval (lv, t) -> N.Lval (generate_lv lv, generate_typ t)
    | Const c -> N.Const c 
    | AddrOfFun (fid, ft) -> N.AddrOfFun (fid, generate_ftyp ft)
    | AddrOf (lv, sz) -> 
        let sz = 
          try Nat.to_int (generate_tmp_nat sz) 
          with Invalid_argument "Newspeak.Nat.to_int" -> Config.max_sizeof
        in
          if (sz > Config.max_sizeof) then begin
            Npkcontext.report_error "Link.generate_exp" 
              ("size too large: maximum allowed is "
               ^(string_of_int Config.max_sizeof)^" bits")
          end;
          N.UnOp (N.Focus sz, N.AddrOf (generate_lv lv))
    | UnOp (o, e) -> N.UnOp (generate_unop o, generate_exp e)
    | BinOp (o, e1, e2) -> N.BinOp (o, generate_exp e1, generate_exp e2)

(* TODO:
   Avoid redefinition of unop and binop. Use the same for npkil and newspeak
   just add a belongs_tmp to npkil !!! *)
and generate_unop o =
  match o with
      Belongs_tmp (l, u) -> 
        let u = Nat.sub (generate_tmp_nat u) Nat.one in
          Newspeak.Belongs (l, u)
    | Coerce r -> Newspeak.Coerce r
    | Not -> Newspeak.Not
    | BNot r -> Newspeak.BNot r
    | PtrToInt k -> Newspeak.PtrToInt k
    | IntToPtr k -> Newspeak.IntToPtr k
    | Cast (t1, t2) -> Newspeak.Cast (t1, t2)

and generate_tmp_nat x =
  match x with
      Known i -> i
    | Length name -> begin
        match get_glob_typ name with
            Newspeak.Array (_, len) -> Nat.of_int len
          | _ -> 
              Npkcontext.report_error "Npklink.generate_tmp_nat" 
                "array type expected"
      end
    | Mult (v, n) -> 
        let i = generate_tmp_nat v in
          Nat.mul_int n i

let generate_global name (t, loc, storage, used) =
  Npkcontext.set_loc loc;
  if used || (not !Npkcontext.remove_temp) then begin
    let t = generate_typ t in
      Hashtbl.add globals name (t, loc);
      match storage with
          Extern -> 
            Npkcontext.report_accept_warning "Link.generate_global" 
              ("extern global variable "^name) Npkcontext.ExternGlobal
        | _ -> ()
  end;
  Npkcontext.print_debug ("Global linked: "^name)

let generate_arg arg =
  match arg with
    | In    e -> N.In    (generate_exp e)
    | Out   l -> N.Out   (generate_lv l)
    | InOut l -> N.InOut (generate_lv l)

let translate_set (lv, e, t) =
  match (t, e) with
      (Scalar t, _) -> N.Set (generate_lv lv, generate_exp e, t)
    | (Region (_, n), Lval (lv', _)) -> 
        N.Copy (generate_lv lv, generate_lv lv', n)
    | _ -> 
        Npkcontext.report_error "Linker.translate_set" 
          "translate_set not implemented yet"

let rec generate_stmt (sk, loc) =
  let new_sk = 
    match sk with
        Set (lv, e, t) -> translate_set (lv, e, t)
      | Decl (name, t, b) -> 
          N.Decl (name, generate_typ t, List.map generate_stmt b)
      | Guard cond -> N.Guard (generate_exp cond)
      | Select (body1, body2) ->
          N.Select (generate_blk body1, generate_blk body2)
      | InfLoop b -> N.InfLoop (List.map generate_stmt b)
      | Call (args, ft, fn, rets) ->
          let args = List.map generate_arg args in
          let ft = generate_ftyp ft in
          let fn = generate_fn fn ft in
          let rets = match rets with
            | Some r -> Some (generate_lv r)
            | None   -> None
          in
          N.Call (args, ft, fn, rets)
      | Goto lbl -> N.Goto lbl
      | DoWith (body, lbl, action) ->
          let body = List.map generate_stmt body in
          let action = List.map generate_stmt action in
            N.DoWith (body, lbl, action)
      | UserSpec x -> N.UserSpec (List.map generate_token x)
  in 
    (new_sk, loc)

and generate_token x =
  match x with
      SymbolToken c -> N.SymbolToken c
    | IdentToken x -> N.IdentToken x
    | LvalToken (lv, t) -> N.LvalToken (generate_lv lv, generate_typ t)
    | CstToken c -> N.CstToken c

and generate_blk x = List.map generate_stmt x
    
and generate_fn fn ft =
  match fn with
    | FunId f -> N.FunId f
    | FunDeref e -> N.FunDeref (generate_exp e, ft)

and generate_body body = List.map generate_stmt body

let generate_fundecs fundecs =
  let funspecs = Hashtbl.create 100 in
  let add_fundec (name, (_rets, args, ftyp, body)) =
    let body = generate_body body in
    let ftyp = generate_ftyp ftyp in
      
      if Hashtbl.mem funspecs name then begin
        Npkcontext.report_error "Npklink.generate_funspecs" 
          ("function "^name^" declared twice")
      end;
      Hashtbl.add funspecs name
        {
          N.ret  = snd ftyp;
          N.args = List.combine args (fst ftyp) ;
          N.body = body ;
        } ;
      Npkcontext.print_debug ("Function linked: "^name)
  in
    List.iter add_fundec fundecs;
    funspecs      

(* TODO: optimization, this is probably not efficient to read the whole
   program and then again a second time!!! reprogram Npkil.read and write *)
let merge npkos =
  let glb_decls = Hashtbl.create 100 in
  let fnames = ref StrSet.empty in
  let init = ref [] in
  let fundefs = ref [] in

  let add_fname x = fnames := StrSet.add x !fnames in

  let add_fundef f body = fundefs := (f, body)::!fundefs in

  let add_global name (t, loc, storage, used) =
    Npkcontext.set_loc loc;
    try
      let (prev_t, prev_loc, prev_storage, prev_used) = 
        Hashtbl.find glb_decls name 
      in
        
      let t =
        try
          if (Npkil.is_mp_typ t prev_t) then t
          else prev_t
        with Npkil.Uncomparable -> 
          (* TODO: add the respective locations *)
          Npkcontext.report_error "Npklink.update_glob_link"
            ("different types for "^name^": '"
             ^(Npkil.string_of_typ prev_t)^"' and '"
             ^(Npkil.string_of_typ t)^"'")
      in
      let used = used || prev_used in
      let storage = 
        match (storage, prev_storage) with
            (Extern, Declared _) -> prev_storage
          | (Declared _, Extern) -> storage
          | (Extern, Extern) -> prev_storage
          | (Declared true, Declared true) -> 
              Npkcontext.report_error "Npklink.update_glob_link" 
                ("multiple declaration of "^name)
          | _ ->
              let info = 
                if prev_loc = loc then begin
                  let (file, _, _) = loc in
                    ", in file "^file^" variable "
                    ^name^" should probably be extern"
                end else begin
                  " (previous definition: "
                  ^(Newspeak.string_of_loc prev_loc)^")"
                end
              in
                Npkcontext.report_accept_warning "Npklink.update_glob_link"
                  ("multiple definitions of global variable "^name^info) 
                  Npkcontext.MultipleDef;             
                prev_storage
      in
        Hashtbl.replace glb_decls name (t, prev_loc, storage, used)
          
    with Not_found -> Hashtbl.add glb_decls name (t, loc, storage, used)
  in

  let merge npko =
    (* TODO: merge these two operations into one *)
    let prog = Npkil.read npko in
      List.iter add_fname prog.fnames;
      Hashtbl.iter add_global prog.globals;
      init := prog.init@(!init);
      Hashtbl.iter add_fundef prog.fundecs;
      prog.src_lang
  in
    match npkos with
        [] -> Npkcontext.report_error "Linker.merge" "empty file list"
      | hd::tl -> 
          let src_lang = merge hd in
          let check_merge x = 
            let _ = merge x in
              ()
          in
            List.iter check_merge tl;
            (StrSet.elements !fnames, glb_decls, !fundefs, src_lang, !init)

let link npkos =
  Npkcontext.forget_loc ();
    
  Npkcontext.print_debug "Linking files...";
  let (filenames, glb_decls, fun_decls, src_lang, init) = merge npkos in
    
    Npkcontext.print_debug "Globals...";
    Hashtbl.iter generate_global glb_decls;
    Npkcontext.forget_loc ();
    
    Npkcontext.print_debug "Functions...";
    let init = generate_blk init in
    let fundecs = generate_fundecs fun_decls in
        
    let prog = { 
      N.fnames = filenames;
      N.globals = globals;
      N.init = init;
      N.fundecs = fundecs;
      N.ptr_sz = Config.size_of_ptr;
      N.src_lang = src_lang;
    } 
    in
      
      Npkcontext.print_debug "File linked.";
      let prog_simpl = 
        if !Npkcontext.no_opt then prog
        else Newspeak.simplify !Npkcontext.opt_checks prog
      in
      Newspeak.write !Npkcontext.output_file prog_simpl;
      if !Npkcontext.verb_newspeak then begin
        print_endline "Newspeak output";
        print_endline "---------------";
        Newspeak.dump prog_simpl;
        print_newline ()
      end;
      if !Npkcontext.verb_lowspeak then begin
	print_endline "Lowspeak output";
	print_endline "---------------";
	Lowspeak.dump (Npk2lpk.translate prog_simpl);
	print_newline ()
      end


