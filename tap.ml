
let tests_total = ref 0
let tests_run   = ref 0

let tap_output str =
  print_endline str

let test_plan n =
  tap_output ("1.."^string_of_int n);
  tests_total := n

let test_end _ =
  begin if (!tests_total <> !tests_run) then
    failwith ("TAP : Bad plan. Planned "^string_of_int !tests_total
             ^" tests, run "^string_of_int !tests_run^".")
  end;
  tests_total := 0;
  tests_run   := 0

let test_ok msg =
  incr tests_run;
  tap_output ("ok "^string_of_int !tests_run^" - "^msg)

let test_not_ok msg reason =
  incr tests_run;
  tap_output ("not ok "^string_of_int !tests_run^" - "^msg
             ^(match reason with
              | None -> ""
              | Some r -> " # "^r))

let test_condition msg condition reason =
  if condition then
    test_ok msg
  else
    test_not_ok msg reason

let assert_equal ?printer actual expected msg =
  test_condition msg (expected = actual)
    (match printer with None -> None
      | Some p -> Some ("Expected : "^p expected
                      ^ " but got : "^p actual))

let assert_equal_int msg expected actual =
  assert_equal ~printer:string_of_int msg expected actual

let assert_true what msg =
  test_condition msg what None

let assert_false what msg =
  assert_true (not what) msg 

let assert_equal_string expected actual msg =
  test_condition msg (String.compare expected actual = 0)
      (Some ("Expected : "^expected
            ^" but got : "^actual))

