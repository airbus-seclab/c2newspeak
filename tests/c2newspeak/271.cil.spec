Newspeak output
---------------
271.c
void main(void) {
  (271.c:27#1072)^int32 x;
  (271.c:28#1081)^int32 y;
  (271.c:29#1086)^choose {
   -->
    (271.c:29#1086)^guard(! (x_int32 ==_int32 0));
    (271.c:29#1086)^y =(int32) 0;
   -->
    (271.c:29#1086)^guard((x_int32 ==_int32 0));
    (271.c:29#1086)^y =(int32) 1;
  }
}


