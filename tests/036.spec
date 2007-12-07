Newspeak output
---------------
main() {
  (30)^int4;
  (31)^int4;
  (32)^do {
    (33)^do {
      (32)^choose {
        | ! (1-_int4 ==_int4 1) -->
          (32)^goto lbl2;
        | (1-_int4 ==_int4 1) -->
          (32)^goto lbl3;
      }
    } with lbl3: {
    }
    (34)^do {
      (35)^do {
        (34)^goto lbl3;
      } with lbl3: {
      }
      (36)^0- =(int4) 4;
    } with lbl2: {
    }
  } with lbl2: {
  }
}


