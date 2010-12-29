Newspeak output
---------------
void (677.c:30#5)^main(void) {
  (677.c:31#2)^int32 tmp_cir!0;
  (677.c:31#2)^tmp_cir!0: int32 <- f();
  (677.c:31#2)^choose {
   -->
    (677.c:31#2)^guard(! (tmp_cir!0_int32 ==_int32 0));
    (677.c:31#2)^guard(! (x_int32 ==_int32 0));
    (677.c:32#4)^x =(int32) 1;
   -->
    (677.c:31#2)^choose {
     -->
      (677.c:31#2)^guard(! (tmp_cir!0_int32 ==_int32 0));
      (677.c:31#2)^guard((x_int32 ==_int32 0));
     -->
      (677.c:31#2)^guard((tmp_cir!0_int32 ==_int32 0));
    }
  }
}

int32 x;

