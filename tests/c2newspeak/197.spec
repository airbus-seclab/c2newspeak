Newspeak output
---------------
197.c
void main(void) {
  (197.c:27#6)^int32 x;
  (197.c:28#6)^int32 y;
  (197.c:29#2)^do {
    (197.c:29#2)^do {
      (197.c:30#4)^choose {
        | ! (1-_int32 ==_int32 0) -->
          (197.c:31#6)^goto lbl2;
        | (1-_int32 ==_int32 0) -->
      }
      (197.c:33#4)^choose {
        | ! (0-_int32 ==_int32 0) -->
          (197.c:34#6)^goto lbl1;
        | (0-_int32 ==_int32 0) -->
      }
    } with lbl2: {
    }
    (197.c:29#2)^while (1) {
      (197.c:29#2)^choose {
        | (10 > 1-_int32) -->
        | ! (10 > 1-_int32) -->
          (197.c:29#2)^goto lbl1;
      }
      (197.c:29#2)^do {
        (197.c:30#4)^choose {
          | ! (1-_int32 ==_int32 0) -->
            (197.c:31#6)^goto lbl3;
          | (1-_int32 ==_int32 0) -->
        }
        (197.c:33#4)^choose {
          | ! (0-_int32 ==_int32 0) -->
            (197.c:34#6)^goto lbl1;
          | (0-_int32 ==_int32 0) -->
        }
      } with lbl3: {
      }
    }
  } with lbl1: {
  }
}


