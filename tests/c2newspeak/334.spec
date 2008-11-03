Warning: goto statements are error-prone, they should be avoided at all costs in 334.c line 30
Newspeak output
---------------
334.c
main() {
  (334.c:27#6)^int32 x;
  (334.c:29#2)^do {
    (334.c:29#2)^while (1) {
      (334.c:29#2)^choose {
        | ! (0-_int32 ==_int32 0) -->
        | (0-_int32 ==_int32 0) -->
          (334.c:29#2)^goto lbl2;
      }
      (334.c:28#1)^do {
        (334.c:30#4)^goto lbl4;
        (334.c:31#4)^0- =(int32) 0;
      } with lbl4: {
      }
    }
  } with lbl2: {
  }
}


