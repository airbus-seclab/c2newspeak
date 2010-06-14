Newspeak output
---------------
279.c
int32 g(void) {
  (279.c:28#1081)^int32 x;
  int32 tmp;
  (279.c:30#1087)^do {
    (279.c:30#1087)^while (1) {
      (279.c:30#1087)^tmp <- f();
      (279.c:30#1087)^choose {
       -->
        (279.c:30#1087)^guard(! (tmp_int32 ==_int32 0));
       -->
        (279.c:30#1087)^guard((tmp_int32 ==_int32 0));
        (279.c:30#1087)^goto lbl1;
      }
    }
  } with lbl1: {
  }
  (279.c:33#1108)^!return =(int32) x_int32;
}


