open Cil
open Cilutils
open Npkcontext
open Npkutils


let local_vids = ref []
let code_to_duplicate = Hashtbl.create 100

class vistor_first_pass = object
  inherit nopCilVisitor

  method vglob g =
    update_loc (get_globalLoc g);
    DoChildren


  (* deletes CIL's gotos (this piece of code might be highly dependent
     on CIL and may have to change in the future. The code that should be
     duplicated is stored in a hash table and used when a Goto is
     encountered) *)
  method vstmt s =
    update_loc (get_stmtLoc s.skind);

    let add_code goto_stmts label =
      match label with
	| Label (s, _, false) as l ->
	    Hashtbl.add code_to_duplicate l goto_stmts
	| _ -> ()
    in

      match s.skind with
	| If (_, {bstmts = ({labels = l1}::_ as goto_stmts1)},
	      {bstmts = ({labels = l2}::_ as goto_stmts2)}, _) ->
	    List.iter (add_code goto_stmts1) l1;
	    List.iter (add_code goto_stmts2) l2;
	    DoChildren
	| If (_, {bstmts = ({labels = l}::_ as goto_stmts)}, _, _)
	| If (_, _, {bstmts = ({labels = l}::_ as goto_stmts)}, _) ->
	    List.iter (add_code goto_stmts) l;
	    DoChildren
	| _ -> DoChildren


  (* removes unused local vars *)
  method vfunc f =
    local_vids := [];
    let del_unused f =
      let rec is_used v l =
	match l with
	  | [] -> false
	  | a::_ when v.vid = a -> true
	  | _::r -> is_used v r
      in
      let rec new_locals locs =
	match locs with
	  | [] -> []
	  | v::r when is_used v !local_vids -> v::(new_locals r)
	  | _::r -> new_locals r
      in
	f.slocals <- new_locals f.slocals;
	f
	  
    in
      ChangeDoChildrenPost (f, del_unused)

  method vlval lv =
    match lv with
	Var {vtype = TFun _}, _ ->
	  DoChildren
      | Var v, _ ->
	  if not v.vglob then local_vids := (v.vid)::(!local_vids);
	  (* TODO: Handle somewhere else *)
	  (*	  glb_uses v; *)
	  DoChildren
      | _ -> DoChildren


  (* simplifies some exps (StartOf, pointer equality) and collects
     const string *)
  method vexpr e =
    match e with
      | Const CStr s ->
	  (* TODO: Handle this !!! *)
	  (* 	  glb_make_cstr s; *)
	  SkipChildren
      | StartOf lv  -> 
	  ChangeDoChildrenPost (AddrOf (addOffsetLval (Index (zero, NoOffset)) lv), fun x -> x)

      | BinOp (Ne as o, CastE(TInt _, e1), CastE(TInt _, e2), (TInt _ as t))
      | BinOp (Eq as o, CastE(TInt _, e1), CastE(TInt _, e2), (TInt _ as t))
	  when size_of t = pointer_size && isPtr e1 && isPtr e2 ->
	  ChangeDoChildrenPost (BinOp (o, e1, e2, t), fun x -> x)
	    
      | _ -> DoChildren

end


let rec visitInit visitor i =
  match i with
      SingleInit e -> SingleInit (visitCilExpr visitor e)
    | CompoundInit (t, i) ->
	let t = visitCilType visitor t in
	let i = List.map (visitOffsetInit visitor) i in
	  CompoundInit (t, i)

and visitOffsetInit visitor (o, i) =
  let o = visitCilOffset visitor o in
  let i = visitInit visitor i in
    (o, i)

(* let visit_glb_t visitor _ x = 
  x.gv_ctyp <- visitCilType visitor x.gv_ctyp;
  match x.gv_cinit with
      Some i -> x.gv_cinit <- Some (visitInit visitor i)
    | None -> ()

let visit_fun_spec_t visitor _ x = 
  x.body <- List.map (visitCilStmt visitor) x.body
*)




(* TODO: Is it necessary ? Should I remove or rewrite this code ? *)
let check_main_signature t =
  let (ret, args) = 
    match unrollType t with
	TFun (ret, Some args, _, []) -> (ret, args)
      | _ -> error ("Cil2newspeak.translate.explore: "
		    ^"main, should have a function type")
  in
    if (!verb_warnings) && (unrollType ret <> TInt (IInt, [])) 
    then print_warning "return type of 'main' is not 'int'";
    match args with
	[] -> ()
      | (_, arg1, [])::(_, arg2, [])::[] 
	  when (unrollType arg1 = TInt (IInt, []))
	    && (unrollTypeDeep arg2 = 
		TPtr (TPtr (TInt (IChar, []), []), []))
	    -> ()
      | _ -> error ("Invalid argument types for main: "
		    ^"authorized forms are main() and"
		    ^" main(int, char**)")


(* Exploration of Cil's "globals" and first pass *)
(* TODO: use something else than a tuple to return things *)
let translate f =
  update_loc locUnknown;
  let visitor = new vistor_first_pass in
    
  let rec explore glb_decls fun_defs proto_decls glist = 
    match glist with
      | [] -> (List.rev glb_decls, List.rev fun_defs, List.rev proto_decls)
      | g::r ->
	  (* TODO: Handle the list returned ! *)
	  let [g_modified] = visitCilGlobal visitor g in
	  let loc = get_globalLoc g in begin
	      update_loc loc;
	      match g_modified with
		| GType (t, _) ->
		    if !verb_warnings then print_warning ("skip typedef "^t.tname);
		    explore glb_decls fun_defs proto_decls r
		| GEnumTag (info, _) -> 
		    if !verb_warnings then print_warning ("skip enum "^info.ename);
		    explore glb_decls fun_defs proto_decls r
		| GCompTag (c, _) -> 
		    if !verb_warnings then print_warning ("skip composite typedef "^c.cname);
		    explore glb_decls fun_defs proto_decls r
		| GCompTagDecl (c, _) -> 
		    if !verb_warnings then print_warning ("skip composite declaration "^c.cname);
		    explore glb_decls fun_defs proto_decls r
		| GPragma (a, _) when !ignores_pragmas -> 
		    print_warning ("Directive ignored: "
				   ^"unknown #pragma "^(string_of_attribute a));
		    explore glb_decls fun_defs proto_decls r
		      
		| GVarDecl ({vname = name; vtype = TFun (ret,args,_,_)}, _) ->
		    explore glb_decls fun_defs (((name, ret, args), loc)::proto_decls) r
		      
		| GFun (f, _) -> 
		    if (f.svar.vname = "main") then check_main_signature f.svar.vtype;
		     explore glb_decls (f::fun_defs) proto_decls r
		      
		| GVarDecl (v, _) -> 
		    explore (((v, false, None), loc)::glb_decls) fun_defs proto_decls r
		      
		| GVar (v, {init = i}, _) -> 
		    explore (((v, true, i), loc)::glb_decls) fun_defs proto_decls r

		| _ -> error ("Cil2newspeak.translate.explore: global "
			      ^(string_of_global g)^" not supported")
	    end;	    
   in
    explore [] [] [] f.globals
