Newspeak output
---------------
205.c
main() {
  (205.c:27#1072)^int32 x;
  (205.c:29#1078)^do {
    (205.c:31#1117)^do {
      (205.c:30#1093)^do {
        (205.c:29#1078)^choose {
          | ! (0-_int32 ==_int32 1) -->
            (205.c:29#1078)^goto lbl3;
          | (0-_int32 ==_int32 1) -->
            (205.c:29#1078)^goto lbl4;
        }
      } with lbl4: {
      }
      (205.c:30#1101)^0- =(int32) 0;
      (205.c:30#1108)^goto lbl2;
    } with lbl3: {
    }
    (205.c:31#1126)^0- =(int32) 1;
  } with lbl2: {
  }
}


