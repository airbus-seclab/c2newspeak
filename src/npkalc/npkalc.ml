
let input = ref ""

let speclist = []

let anon_fun filename = input := filename

let usage_msg = ""

let process () = 
  if !input = "" then StandardMain.report_missing_file ()

let _ =
  StandardMain.launch speclist anon_fun usage_msg process
