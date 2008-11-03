Newspeak output
---------------
198.c
main() {
  (198.c:27#6)^int32 x;
  (198.c:28#2)^do {
    (198.c:29#4)^choose {
      | ! (0-_int32 ==_int32 0) -->
        (198.c:30#6)^goto lbl1;
      | (0-_int32 ==_int32 0) -->
    }
  } with lbl1: {
  }
  (198.c:28#2)^do {
    (198.c:28#2)^while (1) {
      (198.c:28#2)^choose {
        | (10 > 0-_int32) -->
        | ! (10 > 0-_int32) -->
          (198.c:28#2)^goto lbl2;
      }
      (198.c:28#2)^do {
        (198.c:29#4)^choose {
          | ! (0-_int32 ==_int32 0) -->
            (198.c:30#6)^goto lbl1;
          | (0-_int32 ==_int32 0) -->
        }
      } with lbl1: {
      }
    }
  } with lbl2: {
  }
}


