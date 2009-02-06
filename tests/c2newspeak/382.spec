Newspeak output
---------------
382.c
void main(void) {
  (382.c:29#6)^int32 i;
  (382.c:31#2)^0- =(int32) 0;
  (382.c:32#2)^do {
    (382.c:33#2)^do {
      (382.c:32#2)^choose {
       -->
        (382.c:32#2)^guard((0 ==_int32 0));
        (382.c:33#2)^goto lbl2;
       -->
        (382.c:32#2)^guard(! (0 ==_int32 0));
        (382.c:32#2)^goto lbl1;
      }
    } with lbl2: {
    }
    (382.c:34#4)^0- =(int32) 1;
    (382.c:35#4)^0- =(int32) 2;
  } with lbl1: {
  }
}


