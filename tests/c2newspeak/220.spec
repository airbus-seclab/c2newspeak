Newspeak output
---------------
220.c
void main(void) {
  (220.c:31#2)^int32 tmp_cir!0;
  (220.c:29#6)^int32 x;
  (220.c:28#5)^do {
    (220.c:30#2)^choose {
     -->
      (220.c:30#2)^guard(! (x_int32 ==_int32 0));
      (220.c:30#9)^goto lbl0;
     -->
      (220.c:30#2)^guard((x_int32 ==_int32 0));
    }
    (220.c:31#2)^tmp_cir!0 <- f();
    (220.c:31#2)^choose {
     -->
      (220.c:31#2)^guard(! (tmp_cir!0_int32 ==_int32 0));
      (220.c:31#11)^goto lbl0;
     -->
      (220.c:31#2)^guard((tmp_cir!0_int32 ==_int32 0));
    }
  } with lbl0:
}


