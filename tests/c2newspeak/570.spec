Newspeak output
---------------
570.c
void main(void) {
  (570.c:29#2)^ptr !tmp0;
  (570.c:29#2)^!tmp0 <- f();
  (570.c:29#2)^choose {
   -->
    (570.c:29#2)^guard(! ([!tmp0_ptr]8_int8 ==_int32 0));
   -->
    (570.c:29#2)^guard(([!tmp0_ptr]8_int8 ==_int32 0));
  }
}


