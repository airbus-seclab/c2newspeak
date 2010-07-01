(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans
  
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
*)

open Newspeak

module L = Lowspeak

let tmp_var = Temps.to_string 0 (Temps.Misc "npk2lpk")

let new_id =
  let c = ref 0 in
  fun _ ->
    incr c;
    !c

let scalar_of_typ t =
  match t with
      Scalar t -> t
    | _ -> 
        Npkcontext.report_error "Npk2lpk.scalar_of_typ" "scalar type expected"

let default_args_ids fid n = 
  let rec create_args i =
    if i > n then []
    else (fid^".arg"^(string_of_int i))::(create_args (i+1))
  in
    create_args 1

let translate prog = 
  let fundecs = Hashtbl.create 100 in
  let globals = Hashtbl.create 100 in

  let env = Hashtbl.create 100 in
  let stack_height = ref 0 in
  let push id =
    incr stack_height;
    Hashtbl.add env id !stack_height
  in
  let pop id =
    decr stack_height;
    Hashtbl.remove env id
  in

  let rec translate_exp e =
    match e with
        Const c -> L.Const c
      | Lval (lv, t) -> L.Lval (translate_lval lv, scalar_of_typ t)
      | AddrOf lv -> L.AddrOf (translate_lval lv)
      | AddrOfFun (f, ft) -> L.AddrOfFun (f, ft)
      | UnOp (op, e) -> L.UnOp (op, translate_exp e)
      | BinOp (op, e1, e2) -> L.BinOp (op, translate_exp e1, translate_exp e2)

  and translate_lval lv =
    match lv with
        Local v -> 
          let x = 
            try Hashtbl.find env v
            with Not_found -> 
              Npkcontext.report_error "Hpk2npk.translate_lval"
                ("unknown local variable "^v)
          in
            L.Local (!stack_height - x)
      | Global x -> L.Global x
      | Deref (e, sz) -> L.Deref (translate_exp e, sz)
      | Shift (lv, e) -> L.Shift (translate_lval lv, translate_exp e)
  in

  let translate_set (lv, e, t) =
    match (t, e) with
	(Scalar t, _) -> L.Set (translate_lval lv, translate_exp e, t)
      | (Region (_, n), Lval (lv', _)) -> 
	  L.Copy (translate_lval lv, translate_lval lv', n)
      | _ -> 
	  Npkcontext.report_error "Hpk2npk.translate_set" 
	    "translate_set not implemented yet"
  in

  let translate_fn x =
    match x with
        FunId f -> L.FunId f
      | FunDeref (e, ft) -> L.FunDeref (translate_exp e, ft)
  in
    
  let translate_token x =
    match x with
        SymbolToken c -> L.SymbolToken c
      | IdentToken s -> L.IdentToken s
      | LvalToken (lv, t) -> L.LvalToken (translate_lval lv, t)
      | CstToken c -> L.CstToken c
  in

  let translate_assertion x = List.map translate_token x in

  (* TOOD: find a way to factor prefix_args and suffix_rets!! *)
  let prefix_args loc f ft args args_ids =
    let rec add args =
      match args with
        | (arg::args, t::args_t, x::args_ids) -> 
            push tmp_var;
            let set = begin match arg with
              | In    e -> Some (translate_set (Local tmp_var, e, t))
              | InOut l -> Some (translate_set (Local tmp_var, Lval (l, t), t))
              | Out   _ -> None
            end in
            let call = add (args, args_t, args_ids) in
            let copy_out = match arg with
              | In    _ -> None
              | Out   l
              | InOut l -> Some (translate_set (l, Lval (Local tmp_var, t), t))
            in
            pop tmp_var;
            let call_with_copyout = match copy_out with
            | None   -> (call, loc)::[]
            | Some c -> (call, loc)::(c, loc)::[]
            in
            let full_call = match set with
            | None   -> call_with_copyout
            | Some s -> (s, loc)::call_with_copyout
            in
            L.Decl (x, t, full_call)
        | ([], [], []) -> L.Call (translate_fn f)
        | _ -> Npkcontext.report_error "hpk2npk.prefix_args"
                 "Mismatching number of parameters"
    in
  let (args_t, _) = ft in
      add (args, args_t, args_ids)
  in

  let suffix_rets fid loc f ft (args, rets) args_ids =
    let add rets =
      match rets with
          (* TODO: should have one list instead of two here!!! *)
          (Some lv, Some t) -> 
            push tmp_var;
            let e = Lval (Local tmp_var, t) in
            let set = translate_set (lv, e, t) in
            let call = prefix_args loc f ft args args_ids in
            let x = Temps.to_string (new_id ()) (Temps.Value_of fid) in
              pop tmp_var;
              L.Decl (x, t, (call, loc)::(set, loc)::[])
        | _ -> prefix_args loc f ft args args_ids
    in
    let add_fst rets =
      match rets with
          (Some (Local v), Some _) 
            when Hashtbl.find env v = !stack_height -> 
              add(None, None)
        | _ -> add rets
    in
    let (_, rets_t) = ft in
    (* TODO: change ret_typ in ftyp so that it is a list *)
    let rets = (rets, rets_t) in
      add_fst rets
  in

  let rec translate_blk x = List.map translate_stmt x 
  
  and translate_stmt (x, loc) = (translate_stmtkind loc x, loc)

  and translate_stmtkind loc x = 
    match x with
        Call (args, ft, f, rets) -> 
          let (fid, args_ids) = 
            match f with
                FunId fid -> 
                  let args_ids = 
                    try
                      let { args = args } = Hashtbl.find prog.fundecs fid in
                        List.map fst args
                    with Not_found -> default_args_ids fid (List.length args)
                  in
                    (fid, args_ids)
              | FunDeref _ -> 
                  let fid = "fptr_call" in
                    (fid, default_args_ids fid (List.length args))
          in
          suffix_rets fid loc f ft (args, rets) args_ids
      | DoWith (body, lbl, action) -> 
          let body = translate_blk body in
          let action = translate_blk action in
            L.DoWith (body, lbl, action)
      | Goto lbl -> L.Goto lbl
      | Decl (x, t, body) -> 
          push x;
          let body = translate_blk body in
            pop x;
            L.Decl (x, t, body)
      | Set (lv, e, t) -> translate_set (lv, e, Scalar t)
      | Copy (dst, src, sz) -> L.Copy (translate_lval dst, translate_lval src, sz)
      | Select (body1, body2) -> 
          let body1 = translate_blk body1 in
          let body2 = translate_blk body2 in
            L.Select (body1, body2)
      | Guard e -> L.Guard (translate_exp e)
      | InfLoop body -> 
          let body = translate_blk body in
            L.InfLoop body
      | UserSpec x -> L.UserSpec (translate_assertion x) 
  in

  let translate_fundec f fd =
    let ret_ids = match fd.ret with
      | None -> []
      | Some _ -> ["!return"]
    in
    let arg_ids = List.map fst fd.args in
    let ft = (List.map snd fd.args, fd.ret) in
    List.iter push ret_ids;
    List.iter push arg_ids;
    let body = translate_blk fd.body in
    List.iter pop arg_ids;
    List.iter pop ret_ids;
    Hashtbl.add fundecs f (ft, body)
  in

  let translate_global x (t, loc) = Hashtbl.add globals x (t, loc) in
    
  let init = translate_blk prog.init in
    Hashtbl.iter translate_fundec prog.fundecs;
    Hashtbl.iter translate_global prog.globals;

    { 
      L.fnames = prog.fnames; 
      L.globals = globals;
      L.init = init;
      L.fundecs = fundecs;
      L.ptr_sz = prog.ptr_sz;
      L.src_lang = prog.src_lang;
    }
