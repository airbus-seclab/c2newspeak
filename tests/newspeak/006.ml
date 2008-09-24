class visitor_006 =
object (this)
  inherit Newspeak.visitor

  method process_exp e =
    print_endline (Newspeak.string_of_loc (this#get_loc ()));
    false
end

let _ =
  try
    let (_, prog, _) = Newspeak.read Sys.argv.(1) in
      Newspeak.visit (new visitor_006 :> Newspeak.visitor) prog
  with Invalid_argument str -> print_endline str
