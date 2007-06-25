open Newspeak

let input = ref ""
let output = ref "a.npk"
let main = ref "main"
let print = ref false

let anon_fun file =
  if !input = "" then input := file
  else invalid_arg "You can only strip one file at a time."

let usage_msg = Sys.argv.(0)^" [options] [-help|--help] file.npk"

let speclist = 
  [ ("--main", Arg.Set_string main,
    "Choose main function from which to strip");
    
    ("--newspeak", Arg.Set print,
    "Print output");
    
    ("-o", Arg.Set_string output, 
    "Choose name of output file, default is a.npk");
  ]

module StringSet = Set.Make(String)

let collect_used (globs, fundecs) =
  let used_gvars = ref StringSet.empty in
  let used_funs = Hashtbl.create 100 in

  let rec visit_lval x =
    match x with
	Local _ -> ()
      | Global x -> used_gvars := StringSet.add x !used_gvars
      | Deref (e, _) -> visit_exp e
      | Shift (lv, e) ->
	  visit_lval lv;
	  visit_exp e
    
  and visit_exp x = 
    match x with
	Const _ | AddrOfFun _ -> ()
      | Lval (lv, _) -> visit_lval lv
      | AddrOf (lv, _) -> visit_lval lv
      | UnOp (_, e) -> visit_exp e
      | BinOp (_, e1, e2) ->
	  visit_exp e1;
	  visit_exp e2
  in

  let rec visit_stmt (x, _) =
    match x with
	Set (lv, e, _) -> 
	  visit_lval lv;
	  visit_exp e
      | Copy (lv1, lv2, _) ->
	  visit_lval lv1;
	  visit_lval lv2
      | Decl (_, _, x) -> visit_blk x
      | Call fn -> visit_fn fn
      | ChooseAssert choices -> List.iter visit_choice choices
      | InfLoop x -> visit_blk x
      | Label _ | Goto _ -> () 

  and visit_blk x = List.iter visit_stmt x 
    
  and visit_choice (cond, body) = 
    List.iter visit_exp cond;
    visit_blk body

  and visit_fn x = 
    match x with
	FunId f -> visit_fun f
      | FunDeref _ -> 
	  invalid_arg ("Main.collect_used.visit_fn: "
			^"Function pointer called: can not strip")
	    
  and visit_fun f = 
    if not (Hashtbl.mem used_funs f) then begin
      let (ftyp, body) = Hashtbl.find fundecs f in
	Hashtbl.add used_funs f (ftyp, body);
	match body with
	    Some body -> visit_blk body
	  | None -> ()
    end
  in
    
    visit_fun !main;
    (!used_gvars, used_funs)
  
let filter_globals used_gvars globals = 
  let is_used (x, _, _) = StringSet.mem x used_gvars in
    List.filter is_used globals
      
let _ = 
  try 
    Arg.parse speclist anon_fun usage_msg;
    let (files, (globals, fundecs), ptr_sz) = Newspeak.read !input in
    let (used_gvars, stripped_fundecs) = collect_used (globals, fundecs) in
    let stripped_globals = filter_globals used_gvars globals in
    let stripped_npk = (stripped_globals, stripped_fundecs) in
      if !print then Newspeak.dump stripped_npk;
      Newspeak.write !output (files, stripped_npk, ptr_sz)
  with Invalid_argument s ->
    print_endline ("Fatal error: "^s);
    exit 0
