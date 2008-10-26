Warning: conditional expression accepted in 433.c line 28
Warning: cast to void accepted in 433.c line 28
Newspeak output
---------------
433.c
main() {
  (433.c:27#1072)^int32 x;
  (433.c:28#1077)^choose {
    | ! (0-_int32 ==_int32 0) -->
    | (0-_int32 ==_int32 0) -->
  }
}


