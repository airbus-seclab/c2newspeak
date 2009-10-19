
let new_value x vertices i =
    (* join ( f(origin) / (origin, dest, f) in v / dest = i) *)
    let from_vals = Utils.filter_map (fun (origin, dest, (_,f)) ->
      if (dest = i) then
                      begin
                        let xo = x.(origin) in
                        let r = 
                          if Options.get_widening() then
                            Range.widen xo (f xo)
                          else
                            f xo
                        in
                        Some r
                      end
                    else None
    ) vertices in
    List.fold_left Range.join x.(i) from_vals

(* roundrobin algorithm *)
let rec kleene ?(n=0) v x =
  let fx = Array.init (Array.length x) (new_value x v) in
  if fx = x then x, (n*Array.length x)
  else kleene ~n:(succ n) v fx

(* worklist algorithm *)
let f_worklist vertices x =
  let worklist = Queue.create () in
  let ops = ref 0 in
  Array.iteri (fun i _ ->
  	Queue.add i worklist
  ) x;
  while (not (Queue.is_empty worklist)) do
  	incr ops;
  	let n = Queue.take worklist in
  	let nv = new_value x vertices n in
  	let ov = x.(n) in
    x.(n) <- nv;
    if (nv <> ov) then
      let successors = Utils.filter_map (fun (src, dst, _) ->
        if src == n then Some dst
                    else None
        ) vertices in
        List.iter (fun m -> Queue.add m worklist) successors
  done;
  (x, !ops)

let inline_print v =
  String.concat ", " (Array.to_list (Array.mapi
    (fun i x ->
      string_of_int i ^ "--> " ^ Range.to_string x
    ) v
  ))

let solve (ln, v) =
  let x0 = Array.make (ln + 1) Range.bottom in
  x0.(ln) <- Range.from_bounds 0 0;
  let (res, ops) =
    match Options.get_fp_algo () with
    | Options.Roundrobin -> kleene v x0
    | Options.Worklist   -> f_worklist v x0
  in
  if (Options.get_verbose ()) then
    prerr_endline ("FP computed in "^string_of_int ops^" iterations");
  res
