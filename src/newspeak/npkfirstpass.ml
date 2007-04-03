open Cil
open Cilutils
open Npkcontext
open Npkutils
open Npkenv




let local_vids = ref Int_set.empty
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
    local_vids := Int_set.empty;
    let del_unused f =
      let rec new_locals locs =
	match locs with
	  | [] -> []
	  | v::r when Int_set.mem v.vid !local_vids -> v::(new_locals r)
	  | _::r -> new_locals r
      in
	if !remove_temp then f.slocals <- new_locals f.slocals;
	f
	  
    in
      ChangeDoChildrenPost (f, del_unused)

  (* remembers the local variables used *)
  method vlval lv =
    match lv with
      | Var v, _ ->
	  if v.vglob
	  then register_var v
	  else local_vids := Int_set.add v.vid !local_vids;
	  DoChildren
      | _ -> DoChildren

  (* simplifies some exps (StartOf, pointer equality) *)
  method vexpr e =
    match e with
      | Const (CStr s) ->
	  register_cstr s;
	  SkipChildren

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
      | _ ->
	  error "Npkfirstpass.check_main_signature"
	    "main, should have a function type"
  in
    if (!verb_morewarns) && (unrollType ret <> TInt (IInt, [])) 
    then print_warning "Npkfirstpass.check_main_signature"
      "return type of 'main' is not 'int'";
    match args with
	[] -> ()
      | (_, arg1, [])::(_, arg2, [])::[] 
	  when (unrollType arg1 = TInt (IInt, []))
	    && (unrollTypeDeep arg2 = 
		TPtr (TPtr (TInt (IChar, []), []), []))
	    -> ()
      | _ ->
	  error "Npkfirstpass.check_main_signature: "
	    ("invalid argument types for main, "
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
    let new_g = visitCilGlobal visitor g in
      update_loc loc;
      match new_g with
	  | [GType (t, _)] ->
	      print_morewarn "Npkfirstpass.first_pass.explore"
		("skipping typedef "^t.tname)
	  | [GEnumTag (info, _)] -> 
	      print_morewarn "Npkfirstpass.first_pass.explore"
		("skipping enum "^info.ename)
	  | [GCompTag (c, _)] -> 
	      print_morewarn "Npkfirstpass.first_pass.explore"
		("skipping composite typedef "^c.cname)
	  | [GCompTagDecl (c, _)] -> 
	      print_morewarn "Npkfirstpass.first_pass.explore"
		("skipping composite declaration "^c.cname)
	  | [GPragma (a, _)] when !ignores_pragmas -> 
	      print_warning "Npkfirstpass.first_pass.explore"
		("ignoring directive: unknown #pragma "^(string_of_attribute a))
		      
	  | [GVarDecl ({vname = name; vtype = TFun (ret,args,_,_)}, _)] ->
	      update_fun_proto name ret args
		  
	  | [GFun (f, loc)] -> 
	      (* Every defined function is kept *)
	      use_fun f.svar;
	      if (f.svar.vname = "main")
	      then check_main_signature f.svar.vtype;
	      update_fun_def f;
		      
	  | [GVarDecl (v, _)] ->
	      update_glob_decl v
		  
	  | [GVar (v, {init = i}, _)] -> 
	      update_glob_def v i
		
	  | _ ->
	      error "Npkfirstpass.first_pass.explore"
		("global "^(string_of_global g)^" not supported")
  in
    init_env ();
    List.iter explore f.globals
