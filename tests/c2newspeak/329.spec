Newspeak output
---------------
329.c
main() {
  (329.c:29#1089)^int32 !tmp-1073741823;
  (329.c:29#1089)^do {
    (329.c:29#1089)^do {
      (329.c:29#1089)^{
        int32 value_of_f;
        (329.c:29#1089)^f();
        (329.c:29#1089)^1- =(int32) 0-_int32;
      }
      (329.c:30#1106)^do {
        (329.c:29#1089)^choose {
          | (0-_int32 ==_int32 1) -->
            (329.c:30#1106)^goto lbl4;
          | ! (0-_int32 ==_int32 1) -->
            (329.c:29#1089)^goto lbl3;
        }
      } with lbl4: {
      }
      (329.c:31#1118)^goto lbl2;
    } with lbl3: {
    }
  } with lbl2: {
  }
}

