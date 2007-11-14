Newspeak output
---------------
main() {
  (30)^int4;
  (29)^do {
    (31)^do {
      (31)^while (1) {
        (31)^choose {
          | (0-_int4 > 0) -->
          | ! (0-_int4 > 0) -->
            (31)^goto lbl1;
        }
        (32)^goto lbl0;
      }
    } with lbl1: {
    }
    (34)^0- =(int4) 2;
  } with lbl0: {
  }
}


