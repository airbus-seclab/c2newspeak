Newspeak output
---------------
224.c
void main(void) {
  (224.c:27#1073)^int32 x;
  int32 tmp;
  (224.c:29#1081)^choose {
   -->
    (224.c:29#1081)^guard(! (x_int32 ==_int32 0));
    (224.c:29#1081)^choose {
     -->
      (224.c:29#1081)^guard(! (x_int32 ==_int32 0));
      (224.c:29#1081)^tmp =(int32) 1;
     -->
      (224.c:29#1081)^guard((x_int32 ==_int32 0));
      (224.c:29#1081)^tmp =(int32) 0;
    }
   -->
    (224.c:29#1081)^guard((x_int32 ==_int32 0));
    (224.c:29#1081)^tmp =(int32) 0;
  }
  (224.c:29#1081)^x =(int32) tmp_int32;
}


