Newspeak output
---------------
194.c
void main(void) {
  (194.c:27#1073)^int32 x;
  int32 tmp;
  (194.c:29#1081)^choose {
   -->
    (194.c:29#1081)^guard(! (x_int32 ==_int32 0));
    (194.c:29#1081)^choose {
     -->
      (194.c:29#1081)^guard(! (x_int32 ==_int32 0));
      (194.c:29#1081)^choose {
       -->
        (194.c:29#1081)^guard(! (x_int32 ==_int32 0));
        (194.c:29#1081)^tmp =(int32) 1;
       -->
        (194.c:29#1081)^guard((x_int32 ==_int32 0));
        (194.c:29#1081)^tmp =(int32) 0;
      }
     -->
      (194.c:29#1081)^guard((x_int32 ==_int32 0));
      (194.c:29#1081)^tmp =(int32) 0;
    }
   -->
    (194.c:29#1081)^guard((x_int32 ==_int32 0));
    (194.c:29#1081)^tmp =(int32) 0;
  }
  (194.c:29#1081)^x =(int32) tmp_int32;
}


