Newspeak output
---------------
main() {
  (-1)^int4 x;
  (-1)^int4 y;
  (46)^do {
    (32)^do {
      (32)^choose {
        | (1-_int4 <> 1) -->
          (32)^goto lbl1;
        | (1-_int4 == 1) -->
          (32)^goto lbl2;
      }
    } with lbl2: {
    }
    (44)^do {
      (41)^do {
        (37)^do {
          (34)^do {
            (34)^choose {
              | (0-_int4 <> 2) & (0-_int4 <> 3) -->
                (34)^goto lbl2;
              | (0-_int4 == 2) -->
                (34)^goto lbl4;
              | (0-_int4 == 3) -->
                (34)^goto lbl3;
            }
          } with lbl4: {
          }
          (36)^0- =(int4) 2;
          (37)^goto lbl1;
        } with lbl3: {
        }
        (40)^0- =(int4) 3;
        (41)^goto lbl1;
      } with lbl2: {
      }
      (44)^0- =(int4) 4;
    } with lbl1: {
    }
  } with lbl1: {
  }
}


