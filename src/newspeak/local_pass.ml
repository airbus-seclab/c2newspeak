open Cil
open Cilutils
open Npkcontext
open Npkutils
open Env

module String_set = 
  Set.Make (struct type t = string let compare = Pervasives.compare end)


(* List collected during the pass *)
let glb_decls = ref []
let fun_defs = ref []
let proto_decls = ref []
let glb_used = ref (String_set.empty)
let fun_called = ref (String_set.empty)
let glb_cstr = ref (String_set.empty)


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

  (* remembers the functions and global variables used *)
  method vlval lv =
    match lv with
	Var {vtype = TFun _; vname = n}, _ ->
	  fun_called := String_set.add n !fun_called;
	  DoChildren
      | Var v, _ ->
	  if not v.vglob
	  then local_vids := (v.vid)::(!local_vids)
	  else glb_used := String_set.add (glb_uniquename v) !glb_used;
	  DoChildren
      | _ -> DoChildren

  (* simplifies some exps (StartOf, pointer equality) and collects
     const string *)
  method vexpr e =
    match e with
      | Const CStr s ->
	  glb_cstr := String_set.add s !glb_cstr;
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
    
  let rec explore g = 
    let loc = get_globalLoc g in
      update_loc loc;
      let [g_modified] = visitCilGlobal visitor g in
	match g_modified with
	  | GType (t, _) ->
	      if !verb_warnings then print_warning ("skip typedef "^t.tname)
	  | GEnumTag (info, _) -> 
	      if !verb_warnings then print_warning ("skip enum "^info.ename)
	  | GCompTag (c, _) -> 
	      if !verb_warnings then print_warning ("skip composite typedef "^c.cname)
	  | GCompTagDecl (c, _) -> 
	      if !verb_warnings then print_warning ("skip composite declaration "^c.cname)
	  | GPragma (a, _) when !ignores_pragmas -> 
	      print_warning ("Directive ignored: "
			     ^"unknown #pragma "^(string_of_attribute a))
		      
	  | GVarDecl ({vname = name; vtype = TFun (ret,args,_,_)}, _) ->
	      proto_decls := ((name, ret, args), loc)::(!proto_decls)
	  
	  | GFun (f, _) -> 
	      (* Every defined function is kept *)
	      fun_called := String_set.add f.svar.vname !fun_called;
	      if (f.svar.vname = "main") then check_main_signature f.svar.vtype;      
	      fun_defs := f::(!fun_defs)
		      
	  | GVarDecl (v, _) -> 
	      glb_decls := ((v, false, None), loc)::(!glb_decls)
		      
	  | GVar (v, {init = i}, _) -> 
	      glb_decls := ((v, true, i), loc)::(!glb_decls)
		
	  | _ -> error ("Cil2newspeak.translate.explore: global "
			^(string_of_global g)^" not supported")
  in
    glb_decls := [];
    fun_defs := [];
    proto_decls := [];
    glb_used := String_set.empty;
    fun_called := String_set.empty;
    glb_cstr := String_set.empty;
    List.iter explore f.globals;
    (List.rev !glb_decls, List.rev !fun_defs, List.rev !proto_decls,
     String_set.elements !glb_used, String_set.elements !fun_called,
     String_set.elements !glb_cstr)
