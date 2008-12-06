Newspeak output
---------------
095.c
void main(void) {
  (095.c:27#1072)^int32 x;
  (095.c:26#1052)^do {
    (095.c:28#1077)^while (1) {
      (095.c:28#1077)^choose {
        | ! (0 > 0-_int32) -->
          (095.c:28#1077)^choose {
            | (10 > 0-_int32) -->
            | ! (10 > 0-_int32) -->
              (095.c:28#1077)^goto lbl0;
          }
        | (0 > 0-_int32) -->
          (095.c:28#1077)^goto lbl0;
      }
    }
  } with lbl0: {
  }
}


