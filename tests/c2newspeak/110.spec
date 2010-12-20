Newspeak output
---------------
void main(void) {
  (110.c:29#2)^int32 tmp_cir!0;
  (110.c:29#2)^tmp_cir!0: int32 <- f();
  (110.c:29#2)^choose {
   -->
    (110.c:29#2)^guard(! (tmp_cir!0_int32 ==_int32 0));
   -->
    (110.c:29#2)^guard((tmp_cir!0_int32 ==_int32 0));
  }
}


