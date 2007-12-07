Newspeak output
---------------
main() {
  (31)^int4;
  (32)^do {
    (33)^do {
      (32)^choose {
        | ! (0-_int4 ==_int4 1) & ! (0-_int4 ==_int4 2) -->
          (32)^goto lbl2;
        | (0-_int4 ==_int4 1) -->
          (32)^goto lbl3;
        | (0-_int4 ==_int4 2) -->
          (32)^goto lbl3;
      }
    } with lbl3: {
    }
    (35)^0- =(int4) 1;
  } with lbl2: {
  }
}


