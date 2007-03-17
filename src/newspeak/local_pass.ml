open Cil
open Cilutils
open Npkcontext
open Npkutils
open Env




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

  (* remembers the local variables used *)
  method vlval lv =
    match lv with
      | Var v, _ ->
	  if not v.vglob
	  then local_vids := (v.vid)::(!local_vids);
	  DoChildren
      | _ -> DoChildren

  (* simplifies some exps (StartOf, pointer equality) *)
  method vexpr e =
    match e with
      | StartOf lv  -> 
	  ChangeDoChildrenPost (AddrOf (addOffsetLval (Index (zero, NoOffset)) lv), fun x -> x)

      | BinOp (Ne as o, CastE(TInt _, e1), CastE(TInt _, e2), (TInt _ as t))
      | BinOp (Eq as o, CastE(TInt _, e1), CastE(TInt _, e2), (TInt _ as t))
	  when size_of t = pointer_size && isPtr e1 && isPtr e2 ->
	  ChangeDoChildrenPost (BinOp (o, e1, e2, t), fun x -> x)
	    
      | _ -> DoChildren

end




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
(* TODO: factor out print_warnings by putting together the warnings and 
   selecting which should be verb_warnings or not *)
let first_pass f =
  update_loc locUnknown;
  let visitor = new vistor_first_pass in
    
  let rec explore g = 
    let loc = get_globalLoc g in
      update_loc loc;
      match visitCilGlobal visitor g with
	  | [GType (t, _)] ->
	      if !verb_warnings then print_warning ("skip typedef "^t.tname)
	  | [GEnumTag (info, _)] -> 
	      if !verb_warnings then print_warning ("skip enum "^info.ename)
	  | [GCompTag (c, _)] -> 
	      if !verb_warnings then print_warning ("skip composite typedef "^c.cname)
	  | [GCompTagDecl (c, _)] -> 
	      if !verb_warnings then print_warning ("skip composite declaration "^c.cname)
	  | [GPragma (a, _)] when !ignores_pragmas -> 
	      print_warning ("Directive ignored: "
			     ^"unknown #pragma "^(string_of_attribute a))
		      
	  | [GVarDecl ({vname = name; vtype = TFun (ret,args,_,_)}, _)] ->

	      (* TODO: Regroup all prototypes declaration (old
		 update_specs : a function to check / update from a
		 prototype and one from a definition) *)
	      let ret_type = match ret with
		| TVoid _ -> None
		| t -> Some (translate_typ t)
	      in

	      let rec translate_formals i l =
		match l with
		    [] -> []
		  | (n, t, _)::r ->
		      let name =
			if n="" then "arg" ^ (string_of_int i) else n
		      in
			(name, translate_typ t)::(translate_formals (i+1) r)
	      in
	      let formals = match args with
		  None ->
		    print_warning ("missing or incomplete prototype for "^name);
		    None
		| Some l -> Some (translate_formals 0 l)
	      in
		(* TODO: should be checked if already existing *)
		Hashtbl.add fun_specs name
		  {pname = name; prett = ret_type;
		   pargs = formals; plocs = None;
		   ploc = loc; pbody = None;}
		  
	  | [GFun (f, _)] -> 
	      (* Every defined function is kept *)
	      (* TODO: add a prototype *)
	      use_fun f.svar;
	      if (f.svar.vname = "main")
	      then check_main_signature f.svar.vtype;
	      fun_defs := (f, loc)::(!fun_defs)
(*		fun_specs := {pname = name; prett = ret_type;
			      pargs = formals; plocs = None;
			      ploc = loc; pbody = None;}::(!fun_specs)*)
		      
	  | [GVarDecl (v, _)] -> 
	      let name = glb_uniquename v in
		(* TODO: Should be checked of already existing *)
		Hashtbl.add glb_decls name
		  {gname = name; gtype = v.vtype; gloc = v.vdecl;
		   gdefd = false; ginit = None;}
		  
	  | [GVar (v, {init = i}, _)] -> 
	      let name = glb_uniquename v in
		(* TODO: Should be checked of already existing *)
		Hashtbl.add glb_decls name
		  {gname = name;
		   gtype = v.vtype; gloc = v.vdecl;
		   gdefd = true; ginit = i;}
		
	  | _ -> error ("Cil2newspeak.translate.explore: global "
			^(string_of_global g)^" not supported")
  in
    Hashtbl.clear glb_decls;
    fun_defs := [];
    Hashtbl.clear fun_specs;
    glb_used := String_set.empty;
    fun_called := String_set.empty;
    glb_cstr := String_set.empty;

    List.iter explore (List.rev f.globals)
