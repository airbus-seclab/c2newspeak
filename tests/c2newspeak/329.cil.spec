Newspeak output
---------------
329.c
main() {
  int32 tmp;
  (329.c:29#1089)^{
    int32 value_of_f;
    (329.c:29#1089)^f();
    (329.c:29#1089)^1- =(int32) 0-_int32;
  }
  (329.c:29#1089)^do {
    (329.c:30#1106)^do {
      (329.c:29#1089)^choose {
        | ! (0-_int32 ==_int32 1) -->
          (329.c:29#1089)^goto lbl2;
        | (0-_int32 ==_int32 1) -->
          (329.c:29#1089)^goto lbl3;
      }
    } with lbl3: {
    }
  } with lbl2: {
  }
}


