let visitor_006 =
  { Lowspeak.visit_nop with
    Lowspeak.exp = (fun loc e ->
    print_endline (Newspeak.string_of_loc loc);
    false)
  }

let _ =
  try
    let prog = Npk2lpk.translate (Newspeak.read Sys.argv.(1)) in
      Lowspeak.visit visitor_006 prog
  with Invalid_argument str -> print_endline str
