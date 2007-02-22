type counter = int ref

let loop = ref 0
let array = ref 0
let pointer_deref = ref 0
let pointer_arith = ref 0
let fpointer = ref 0
let f_tbl = Hashtbl.create 10
let funct = ref 0

let to_string () = 
  let str = ref "" in
  let string_of_call f x = 
    str := !str^"Number of calls to "^f^": "^(string_of_int x)^"\n"
  in
    Hashtbl.iter string_of_call f_tbl;
    "Number of functions: "^(string_of_int (!funct))^"\n"^
    !str
    ^"Number of loops: "^(string_of_int (!loop))^"\n"
    ^"Number of array operations: "^(string_of_int (!array))^"\n"
    ^"Number of pointer deref : "
    ^(string_of_int (!pointer_deref))^"\n"
    ^"Number of pointer arithmetic (+) : "
    ^(string_of_int (!pointer_arith))^"\n"
    ^"Number of function pointer call : "
    ^(string_of_int (!fpointer))

let count x = incr x

let count_call f = 
  try 
    let n = Hashtbl.find f_tbl f in
      Hashtbl.replace f_tbl f (n+1)
  with Not_found -> ()

let add_counted_call f = Hashtbl.add f_tbl f 0
     
let args = ["--count-call", Arg.String add_counted_call, 
	    "count the number of function calls"]
