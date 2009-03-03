Newspeak output
---------------
279.c
int32 g(void) {
  (279.c:28#6)^int32 x;
  (279.c:30#2)^{
    int32 !tmp0;
    (279.c:30#2)^do {
      (279.c:30#2)^while (1) {
        (279.c:30#2)^f();
        (279.c:30#2)^choose {
         -->
          (279.c:30#2)^guard(! (0-_int32 ==_int32 0));
         -->
          (279.c:30#2)^guard((0-_int32 ==_int32 0));
          (279.c:30#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
  (279.c:33#2)^1- =(int32) 0-_int32;
}


