
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

let assert_equal msg expected actual =
  test_condition msg (expected = actual)
      (Some ("Expected : "^string_of_int expected
            ^" but got : "^string_of_int actual))

let assert_true msg what =
  test_condition msg what None

let assert_false msg what =
  assert_true msg (not what)

let assert_equal_string msg expected actual =
  test_condition msg (String.compare expected actual = 0)
      (Some ("Expected : "^expected
            ^" but got : "^actual))

