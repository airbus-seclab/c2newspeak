Newspeak output
---------------
278.c
int32 g(void) {
  (278.c:28#6)^int32 x;
  (278.c:31#8)^{
    int32 y;
    (278.c:30#2)^do {
      (278.c:30#2)^while (1) {
        (278.c:30#2)^choose {
         -->
          (278.c:30#2)^guard(! (1-_int32 ==_int32 0));
         -->
          (278.c:30#2)^guard((1-_int32 ==_int32 0));
          (278.c:30#2)^goto lbl1;
        }
        (278.c:32#4)^goto lbl1;
        (278.c:33#4)^0- =(int32) 1;
      }
    } with lbl1: {
    }
  }
  (278.c:36#2)^1- =(int32) 0-_int32;
}


