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
      (43)^do {
        (39)^do {
          (35)^do {
            (34)^choose {
              | ! (0-_int4 ==_int4 2) & ! (0-_int4 ==_int4 3) -->
                (34)^goto lbl3;
              | (0-_int4 ==_int4 2) -->
                (34)^goto lbl5;
              | (0-_int4 ==_int4 3) -->
                (34)^goto lbl4;
            }
          } with lbl5: {
          }
          (36)^0- =(int4) 2;
          (37)^goto lbl2;
        } with lbl4: {
        }
        (40)^0- =(int4) 3;
        (41)^goto lbl2;
      } with lbl3: {
      }
      (44)^0- =(int4) 4;
    } with lbl2: {
    }
  } with lbl2: {
  }
}


