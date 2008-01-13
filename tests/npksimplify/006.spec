getIndex() {
  (29)^int32;
  (30)^int32;
  (28)^do {
    (32)^1- =(int32) 0;
    (33)^0- =(int32) 0;
    (34)^do {
      (34)^while (1) {
        (35)^choose {
          | ! (0 ==_int32 t + (belongs[0,9] 0-_int32 * 8)_int8) -->
            (36)^[2-_ptr]32 =(int32) 0-_int32;
            (37)^1- =(int32) 1;
          | (0 ==_int32 t + (belongs[0,9] 0-_int32 * 8)_int8) -->
        }
        (34)^choose {
          | (10 > 0-_int32) -->
            (34)^choose {
              | (0 ==_int32 1-_int32) -->
              | ! (0 ==_int32 1-_int32) -->
                (34)^goto lbl2;
            }
          | ! (10 > 0-_int32) -->
            (34)^goto lbl2;
        }
      }
    } with lbl2: {
    }
    (41)^choose {
      | ! (0 ==_int32 1-_int32) -->
        (42)^3- =(int32) 1;
        (42)^goto lbl0;
      | (0 ==_int32 1-_int32) -->
        (44)^3- =(int32) 0;
        (44)^goto lbl0;
    }
  } with lbl0: {
  }
}

main() {
  (49)^int32;
  (50)^int32;
  (52)^int32;
  (52)^{
    int32;
    (52)^{
      ptr;
      (52)^0- =(ptr) &_32(4-);
      (29)^{
        int32;
        (30)^int32;
        (28)^do {
          (32)^1- =(int32) 0;
          (33)^0- =(int32) 0;
          (34)^do {
            (34)^while (1) {
              (35)^choose {
                | ! (0 ==_int32 t + (belongs[0,9] 0-_int32 * 8)_int8) -->
                  (36)^[2-_ptr]32 =(int32) 0-_int32;
                  (37)^1- =(int32) 1;
                | (0 ==_int32 t + (belongs[0,9] 0-_int32 * 8)_int8) -->
              }
              (34)^choose {
                | (10 > 0-_int32) -->
                  (34)^choose {
                    | (0 ==_int32 1-_int32) -->
                    | ! (0 ==_int32 1-_int32) -->
                      (34)^goto lbl2;
                  }
                | ! (10 > 0-_int32) -->
                  (34)^goto lbl2;
              }
            }
          } with lbl2: {
          }
          (41)^choose {
            | ! (0 ==_int32 1-_int32) -->
              (42)^3- =(int32) 1;
              (42)^goto lbl0;
            | (0 ==_int32 1-_int32) -->
              (44)^3- =(int32) 0;
              (44)^goto lbl0;
          }
        } with lbl0: {
        }
      }
    }
    (52)^1- =(int32) 0-_int32;
  }
  (52)^1- =(int32) 0-_int32;
  (54)^choose {
    | ! (0 ==_int32 0-_int32) -->
      (55)^t + (belongs[0,9] 2-_int32 * 8) =(int8) 0;
    | (0 ==_int32 0-_int32) -->
  }
}

int8[10] t = 0;
