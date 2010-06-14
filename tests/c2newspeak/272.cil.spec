Newspeak output
---------------
272.c
void main(void) {
  (272.c:27#1072)^int32 x;
  (272.c:28#1081)^int32 y;
  (272.c:29#1086)^choose {
   -->
    (272.c:29#1086)^guard(! (x_int32 ==_int32 0));
    (272.c:29#1086)^y =(int32) 0;
   -->
    (272.c:29#1086)^guard((x_int32 ==_int32 0));
    (272.c:29#1086)^y =(int32) 1;
  }
}


