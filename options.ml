
type fp_algorithm =
  | Roundrobin
  | Worklist

let fp_alg_str = function
  | "rr" -> Roundrobin
  | "wl" -> Worklist
  | s    -> invalid_arg ("no such fixpoint algorithm : "^s)

let     widening = ref false
let set_widening _ =   widening := true
let get_widening _ = ! widening

let     verbose = ref false
let set_verbose _ =   verbose := true
let get_verbose _ = ! verbose

let     cfg_only = ref false
let set_cfg_only _ =   cfg_only := true
let get_cfg_only _ = ! cfg_only

let     graphviz = ref false
let set_graphviz _ =   graphviz := true
let get_graphviz _ = ! graphviz

let     fp_algo  = ref Worklist
let set_fp_algo x =   fp_algo := fp_alg_str x
let get_fp_algo _ = ! fp_algo

type opt_action =
  | Help
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

