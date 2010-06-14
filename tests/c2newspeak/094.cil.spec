Newspeak output
---------------
094.c
void main(void) {
  (094.c:27#1072)^int32 x;
  int32 tmp;
  (094.c:28#1077)^choose {
   -->
    (094.c:28#1077)^guard(! (0 > x_int32));
    (094.c:28#1077)^choose {
     -->
      (094.c:28#1077)^guard((10 > x_int32));
      (094.c:28#1077)^tmp =(int32) 1;
     -->
      (094.c:28#1077)^guard(! (10 > x_int32));
      (094.c:28#1077)^tmp =(int32) 0;
    }
   -->
    (094.c:28#1077)^guard((0 > x_int32));
    (094.c:28#1077)^tmp =(int32) 0;
  }
  (094.c:28#1077)^x =(int32) tmp_int32;
}


