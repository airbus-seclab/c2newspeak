Newspeak output
---------------
037.c
main() {
  (037.c:30#1132)^int32 y;
  (037.c:29#1112)^do {
    (037.c:31#1137)^choose {
      | ! (0-_int32 ==_int32 2) & ! (0-_int32 ==_int32 1) -->
        (037.c:31#1137)^goto lbl0;
      | (0-_int32 ==_int32 2) -->
        (037.c:31#1137)^goto lbl0;
      | (0-_int32 ==_int32 1) -->
        (037.c:31#1137)^goto lbl0;
    }
  } with lbl0: {
  }
}


