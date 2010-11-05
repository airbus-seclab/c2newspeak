Newspeak output
---------------
375.c
void main(void) {
  (375.c:29#6)^int32 x;
  (375.c:29#9)^int32 y;
  (375.c:32#4)^int32 tmp_cir!0;
  (375.c:31#2)^do {
    (375.c:31#2)^while (1) {
      (375.c:32#4)^tmp_cir!0 <- f();
      (375.c:32#4)^choose {
       -->
        (375.c:32#4)^choose {
         -->
          (375.c:32#4)^guard(! (tmp_cir!0_int32 ==_int32 0));
         -->
          (375.c:32#4)^guard((tmp_cir!0_int32 ==_int32 0));
          (375.c:32#4)^guard(! (x_int32 ==_int32 0));
        }
        (375.c:33#6)^y =(int32) 1;
        (375.c:34#6)^goto lbl1;
       -->
        (375.c:32#4)^guard((tmp_cir!0_int32 ==_int32 0));
        (375.c:32#4)^guard((x_int32 ==_int32 0));
      }
    }
  } with lbl1:
}


