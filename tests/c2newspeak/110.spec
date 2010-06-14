Newspeak output
---------------
110.c
void main(void) {
  (110.c:29#2)^int32 !tmp0;
  (110.c:29#2)^!tmp0 <- f();
  (110.c:29#2)^choose {
   -->
    (110.c:29#2)^guard(! (!tmp0_int32 ==_int32 0));
   -->
    (110.c:29#2)^guard((!tmp0_int32 ==_int32 0));
  }
}


