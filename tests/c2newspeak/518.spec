Newspeak output
---------------
518.c
main() {
  (518.c:30#6)^int32 x;
  (518.c:31#2)^do {
    (518.c:31#2)^while (1) {
      (518.c:31#2)^choose {
        | ! (0-_int32 ==_int32 0) -->
        | (0-_int32 ==_int32 0) -->
          (518.c:31#2)^goto lbl1;
      }
    }
  } with lbl1: {
  }
}

