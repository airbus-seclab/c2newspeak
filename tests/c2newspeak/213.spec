Newspeak output
---------------
213.c
void main(void) {
  (213.c:29#2)^int32 tmp_cir!0;
  (213.c:29#2)^tmp_cir!0 <- f();
  (213.c:28#5)^do {
    (213.c:29#2)^choose {
     -->
      (213.c:29#2)^guard(! (tmp_cir!0_int32 ==_int32 0));
      (213.c:30#4)^goto lbl0;
     -->
      (213.c:29#2)^guard((tmp_cir!0_int32 ==_int32 0));
    }
  } with lbl0: {
  }
}


