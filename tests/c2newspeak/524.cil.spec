Newspeak output
---------------
524.c
void main(void) {
  (524.c:27#1072)^int32 x;
  (524.c:28#1081)^int32 y;
  int32 tmp;
  (524.c:29#1108)^y =(int32) 1;
  (524.c:29#1108)^choose {
   -->
    (524.c:29#1108)^guard(! (x_int32 ==_int32 0));
    (524.c:29#1108)^tmp =(int32) 1;
   -->
    (524.c:29#1108)^guard((x_int32 ==_int32 0));
    (524.c:29#1108)^tmp =(int32) 0;
  }
  (524.c:29#1108)^x =(int32) tmp_int32;
}


