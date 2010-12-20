Newspeak output
---------------
void main(void) {
  (570.c:29#2)^ptr tmp_cir!0;
  (570.c:29#2)^tmp_cir!0: ptr <- f();
  (570.c:29#2)^choose {
   -->
    (570.c:29#2)^guard(! ([tmp_cir!0_ptr]8_int8 ==_int32 0));
   -->
    (570.c:29#2)^guard(([tmp_cir!0_ptr]8_int8 ==_int32 0));
  }
}


