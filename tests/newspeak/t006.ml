class visitor_006 =
object (this)
  inherit Lowspeak.visitor

  method process_exp e =
    print_endline (Newspeak.string_of_loc this#get_loc);
    false
end

let _ =
  try
    let prog = Npk2lpk.translate (Newspeak.read Sys.argv.(1)) in
      Lowspeak.visit (new visitor_006 :> Lowspeak.visitor) prog
  with Invalid_argument str -> print_endline str
