Newspeak output
---------------
225.c
void main(void) {
  (225.c:27#1073)^int32 x;
  int32 tmp;
  int32 tmp___0;
  (225.c:29#1081)^tmp =(int32) x_int32;
  (225.c:29#1081)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (225.c:29#1081)^choose {
   -->
    (225.c:29#1081)^guard(! (tmp_int32 ==_int32 0));
    (225.c:29#1081)^tmp___0 =(int32) 1;
   -->
    (225.c:29#1081)^guard((tmp_int32 ==_int32 0));
    (225.c:29#1081)^choose {
     -->
      (225.c:29#1081)^guard(! (x_int32 ==_int32 0));
      (225.c:29#1081)^tmp___0 =(int32) 1;
     -->
      (225.c:29#1081)^guard((x_int32 ==_int32 0));
      (225.c:29#1081)^tmp___0 =(int32) 0;
    }
  }
  (225.c:29#1081)^x =(int32) tmp___0_int32;
}


