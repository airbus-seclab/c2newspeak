getIndex() {
  (57)^int4 found;
  (57)^int4 x;
  (28)^do {
    (32)^1- =(int4) 0;
    (33)^0- =(int4) 0;
    (34)^do {
      (34)^while (1) {
        (35)^choose {
          | ! (0 == t + (belongs[0,9] 0-_int4 * 1)_int1) -->
            (36)^[2-_ptr]4 =(int4) 0-_int4;
            (37)^1- =(int4) 1;
          | (0 == t + (belongs[0,9] 0-_int4 * 1)_int1) -->
        }
        (34)^choose {
          | (10 > 0-_int4) -->
            (34)^choose {
              | (0 == 1-_int4) -->
              | ! (0 == 1-_int4) -->
                (34)^goto lbl1;
            }
          | ! (10 > 0-_int4) -->
            (34)^goto lbl1;
        }
      }
    } with lbl1: {
    }
    (41)^choose {
      | ! (0 == 1-_int4) -->
        (42)^3- =(int4) 1;
        (42)^goto lbl0;
      | (0 == 1-_int4) -->
        (44)^3- =(int4) 0;
        (44)^goto lbl0;
    }
  } with lbl0: {
  }
}

main() {
  (-1)^int4 i;
  (-1)^int4 c;
  (52)^{
    int4 value_of_getIndex;
    (52)^{
      ptr ptr;
      (52)^0- =(ptr) &_4(3-);
      (57)^{
        int4 found;
        (57)^int4 x;
        (28)^do {
          (32)^1- =(int4) 0;
          (33)^0- =(int4) 0;
          (34)^do {
            (34)^while (1) {
              (35)^choose {
                | ! (0 == t + (belongs[0,9] 0-_int4 * 1)_int1) -->
                  (36)^[2-_ptr]4 =(int4) 0-_int4;
                  (37)^1- =(int4) 1;
                | (0 == t + (belongs[0,9] 0-_int4 * 1)_int1) -->
              }
              (34)^choose {
                | (10 > 0-_int4) -->
                  (34)^choose {
                    | (0 == 1-_int4) -->
                    | ! (0 == 1-_int4) -->
                      (34)^goto lbl1;
                  }
                | ! (10 > 0-_int4) -->
                  (34)^goto lbl1;
              }
            }
          } with lbl1: {
          }
          (41)^choose {
            | ! (0 == 1-_int4) -->
              (42)^3- =(int4) 1;
              (42)^goto lbl0;
            | (0 == 1-_int4) -->
              (44)^3- =(int4) 0;
              (44)^goto lbl0;
          }
        } with lbl0: {
        }
      }
    }
    (52)^1- =(int4) 0-_int4;
  }
  (54)^choose {
    | ! (0 == 0-_int4) -->
      (55)^t + (belongs[0,9] 1-_int4 * 1) =(int1) 0;
    | (0 == 0-_int4) -->
  }
}

int1[10] t = 0;
