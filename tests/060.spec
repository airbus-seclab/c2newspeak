Newspeak output
---------------
main() {
  (30)^int32;
  (29)^do {
    (31)^do {
      (31)^while (1) {
        (31)^choose {
          | (0-_int32 > 0) -->
          | ! (0-_int32 > 0) -->
            (31)^goto lbl2;
        }
        (32)^goto lbl0;
      }
    } with lbl2: {
    }
    (34)^0- =(int32) 2;
  } with lbl0: {
  }
}


