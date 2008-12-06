Newspeak output
---------------
205.c
void main(void) {
  (205.c:27#6)^int32 x;
  (205.c:29#2)^do {
    (205.c:29#2)^do {
      (205.c:30#2)^do {
        (205.c:29#2)^choose {
          | (0-_int32 ==_int32 1) -->
            (205.c:30#2)^goto lbl3;
          | ! (0-_int32 ==_int32 1) -->
            (205.c:29#2)^goto lbl2;
        }
      } with lbl3: {
      }
      (205.c:30#10)^0- =(int32) 0;
      (205.c:30#17)^goto lbl1;
    } with lbl2: {
    }
    (205.c:31#11)^0- =(int32) 1;
  } with lbl1: {
  }
}


