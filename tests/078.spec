Newspeak output
---------------
078.c
main() {
  (078.c:27#1072)^int32;
  (078.c:28#1081)^int32;
  (078.c:35#1170)^int32;
  (078.c:28#1081)^1- =(int32) 0;
  (078.c:29#1090)^2- =(int32) 0;
  (078.c:30#1099)^do {
    (078.c:30#1099)^while (1) {
      (078.c:30#1099)^do {
        (078.c:30#1099)^choose {
          | (10 > 2-_int32) -->
          | ! (10 > 2-_int32) -->
            (078.c:30#1099)^goto lbl2;
        }
        (078.c:31#1120)^1- =(int32) coerce[-2147483648,2147483647] (1 - 1-_int32);
        (078.c:32#1135)^choose {
          | ! (1-_int32 ==_int32 0) -->
            (078.c:33#1150)^goto lbl1;
          | (1-_int32 ==_int32 0) -->
        }
        (078.c:35#1170)^0- =(int32) 2-_int32;
        (078.c:35#1170)^2- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
      } with lbl1: {
      }
    }
  } with lbl2: {
  }
}


