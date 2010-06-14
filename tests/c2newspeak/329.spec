Newspeak output
---------------
329.c
void main(void) {
  (329.c:29#2)^int32 !tmp0;
  (329.c:29#2)^!tmp0 <- f();
  (329.c:29#2)^do {
    (329.c:29#2)^choose {
     -->
      (329.c:29#2)^guard((!tmp0_int32 ==_int32 1));
      (329.c:30#2)^goto lbl1;
     -->
      (329.c:29#2)^guard(! (!tmp0_int32 ==_int32 1));
      (329.c:29#2)^goto lbl1;
    }
  } with lbl1: {
  }
}


