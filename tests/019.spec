Newspeak output
---------------
main() {
  (31)^int4 x;
  (35)^do {
    (32)^do {
      (32)^choose {
        | ! (2 ==_int4 0-_int4) -->
          (32)^goto lbl1;
        | (0-_int4 ==_int4 2) -->
          (32)^goto lbl2;
      }
    } with lbl2: {
    }
    (34)^0- =(int4) 1;
  } with lbl1: {
  }
}


