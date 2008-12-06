Newspeak output
---------------
060.c
void main(void) {
  (060.c:30#6)^int32 x;
  (060.c:29#5)^do {
    (060.c:31#2)^do {
      (060.c:31#2)^while (1) {
        (060.c:31#2)^choose {
          | (0-_int32 > 0) -->
          | ! (0-_int32 > 0) -->
            (060.c:31#2)^goto lbl1;
        }
        (060.c:32#4)^goto lbl0;
      }
    } with lbl1: {
    }
    (060.c:34#2)^0- =(int32) 2;
  } with lbl0: {
  }
}


