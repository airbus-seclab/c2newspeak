Newspeak output
---------------
int32 (279.c:27#4)^g(void) {
  (279.c:28#6)^int32 x;
  (279.c:30#2)^{
    int32 tmp_cir!0;
    (279.c:30#2)^do {
      (279.c:30#2)^while (1) {
        (279.c:30#2)^tmp_cir!0: int32 <- f();
        (279.c:30#2)^choose {
         -->
          (279.c:30#2)^guard(! (tmp_cir!0_int32 ==_int32 0));
         -->
          (279.c:30#2)^guard((tmp_cir!0_int32 ==_int32 0));
          (279.c:30#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
  (279.c:33#2)^!return =(int32) x_int32;
}


