Newspeak output
---------------
277.c
main() {
  (277.c:28#6)^int32 x;
  (277.c:30#2)^do {
    (277.c:30#2)^do {
      (277.c:31#2)^do {
        (277.c:30#2)^choose {
          | (0-_int32 ==_int32 0) -->
            (277.c:31#2)^goto lbl4;
          | ! (0-_int32 ==_int32 0) -->
            (277.c:30#2)^goto lbl3;
        }
      } with lbl4: {
      }
      (277.c:32#4)^{
        int32 !tmp-1073741822;
        (277.c:32#4)^g();
        (277.c:32#4)^choose {
          | ! (0-_int32 ==_int32 0) -->
          | (0-_int32 ==_int32 0) -->
        }
      }
      (277.c:34#4)^goto lbl2;
    } with lbl3: {
    }
  } with lbl2: {
  }
}


