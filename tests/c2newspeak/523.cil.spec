Newspeak output
---------------
523.c
void main(void) {
  (523.c:27#1072)^int32 x;
  int32 tmp;
  (523.c:28#1099)^choose {
   -->
    (523.c:28#1099)^guard(! (x_int32 ==_int32 0));
    (523.c:28#1099)^tmp =(int32) 1;
   -->
    (523.c:28#1099)^guard((x_int32 ==_int32 0));
    (523.c:28#1099)^tmp =(int32) 0;
  }
  (523.c:28#1099)^x =(int32) tmp_int32;
}


