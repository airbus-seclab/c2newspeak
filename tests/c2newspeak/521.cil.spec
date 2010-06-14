Newspeak output
---------------
521.c
void main(void) {
  (521.c:27#1072)^int32 x;
  (521.c:28#1081)^int32 y;
  (521.c:29#1090)^int32 z;
  int32 tmp;
  (521.c:31#1096)^choose {
   -->
    (521.c:31#1096)^guard(! (x_int32 ==_int32 0));
    (521.c:31#1096)^tmp =(int32) 1;
   -->
    (521.c:31#1096)^guard((x_int32 ==_int32 0));
    (521.c:31#1096)^choose {
     -->
      (521.c:31#1096)^guard(! (y_int32 ==_int32 0));
      (521.c:31#1096)^tmp =(int32) 1;
     -->
      (521.c:31#1096)^guard((y_int32 ==_int32 0));
      (521.c:31#1096)^tmp =(int32) 0;
    }
  }
  (521.c:31#1096)^z =(int32) tmp_int32;
}


