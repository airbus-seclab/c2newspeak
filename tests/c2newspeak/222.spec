Newspeak output
---------------
222.c
void main(void) {
  (222.c:27#6)^int32 x;
  (222.c:28#2)^do {
    (222.c:28#2)^while (1) {
      (222.c:28#2)^choose {
        | ! (0 > 0-_int32) -->
        | (0 > 0-_int32) -->
          (222.c:28#2)^goto lbl1;
      }
    }
  } with lbl1: {
  }
}


