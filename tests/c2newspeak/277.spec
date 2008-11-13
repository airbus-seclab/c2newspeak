Newspeak output
---------------
277.c
main() {
  (277.c:28#6)^int32 x;
  (277.c:27#5)^do {
    (277.c:31#2)^do {
      (277.c:30#2)^choose {
        | (0-_int32 ==_int32 0) -->
          (277.c:31#2)^goto lbl1;
        | ! (0-_int32 ==_int32 0) -->
          (277.c:30#2)^goto lbl0;
      }
    } with lbl1: {
    }
    (277.c:32#4)^{
      int32 !tmp-1073741822;
      (277.c:32#4)^g();
      (277.c:32#4)^choose {
        | ! (0-_int32 ==_int32 0) -->
        | (0-_int32 ==_int32 0) -->
      }
    }
  } with lbl0: {
  }
}


