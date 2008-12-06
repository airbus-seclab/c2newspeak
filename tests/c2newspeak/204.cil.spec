Newspeak output
---------------
204.c
void main(void) {
  (204.c:27#1072)^int32 x;
  (204.c:31#1110)^do {
    (204.c:30#1093)^do {
      (204.c:29#1078)^choose {
        | ! (0-_int32 ==_int32 1) -->
          (204.c:29#1078)^goto lbl1;
        | (0-_int32 ==_int32 1) -->
          (204.c:29#1078)^goto lbl2;
      }
    } with lbl2: {
    }
    (204.c:30#1101)^0- =(int32) 0;
  } with lbl1: {
  }
  (204.c:31#1119)^0- =(int32) 1;
}


