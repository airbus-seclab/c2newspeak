Newspeak output
---------------
512.c
void main(void) {
  int32 tmp;
  (512.c:29#1076)^choose {
   -->
    (512.c:29#1076)^guard(! (x_int32 ==_int32 0));
    (512.c:29#1076)^tmp =(int32) 1;
   -->
    (512.c:29#1076)^guard((x_int32 ==_int32 0));
    (512.c:29#1076)^tmp =(int32) 0;
  }
  (512.c:29#1076)^choose {
   -->
    (512.c:29#1076)^guard(! (tmp_int32 ==_int32 0));
   -->
    (512.c:29#1076)^guard((tmp_int32 ==_int32 0));
  }
}

int32 x;

