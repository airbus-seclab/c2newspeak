Newspeak output
---------------
222.c
void main(void) {
  (222.c:27#1072)^int32 x;
  (222.c:26#1052)^do {
    (222.c:28#1077)^while (1) {
      (222.c:28#1077)^choose {
        | ! (0 > 0-_int32) -->
        | (0 > 0-_int32) -->
          (222.c:28#1077)^goto lbl0;
      }
    }
  } with lbl0: {
  }
}


