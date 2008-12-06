Newspeak output
---------------
204.c
void main(void) {
  (204.c:27#6)^int32 x;
  (204.c:29#2)^do {
    (204.c:30#2)^do {
      (204.c:29#2)^choose {
        | (0-_int32 ==_int32 1) -->
          (204.c:30#2)^goto lbl3;
        | ! (0-_int32 ==_int32 1) -->
          (204.c:29#2)^goto lbl2;
      }
    } with lbl3: {
    }
    (204.c:30#10)^0- =(int32) 0;
  } with lbl2: {
  }
  (204.c:31#11)^0- =(int32) 1;
}


