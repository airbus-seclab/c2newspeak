Newspeak output
---------------
main() {
  (30)^int4;
  (31)^int4;
  (32)^do {
    (33)^do {
      (32)^choose {
        | ! (1 ==_int4 1-_int4) -->
          (32)^goto lbl1;
        | (1-_int4 ==_int4 1) -->
          (32)^goto lbl2;
      }
    } with lbl2: {
    }
    (34)^do {
      (35)^do {
        (34)^goto lbl2;
      } with lbl2: {
      }
      (36)^0- =(int4) 4;
    } with lbl1: {
    }
  } with lbl1: {
  }
}


