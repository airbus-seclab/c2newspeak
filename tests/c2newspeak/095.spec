Newspeak output
---------------
095.c
main() {
  (095.c:27#6)^int32 x;
  (095.c:28#2)^do {
    (095.c:28#2)^while (1) {
      (095.c:28#2)^choose {
        | ! (0 > 0-_int32) -->
          (095.c:28#2)^choose {
            | (10 > 0-_int32) -->
            | ! (10 > 0-_int32) -->
              (095.c:28#2)^goto lbl2;
          }
        | (0 > 0-_int32) -->
          (095.c:28#2)^goto lbl2;
      }
    }
  } with lbl2: {
  }
}


