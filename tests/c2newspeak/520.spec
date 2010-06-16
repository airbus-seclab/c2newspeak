Newspeak output
---------------
520.c
void main(void) {
  (520.c:27#6)^int32 x;
  (520.c:28#6)^int32 y;
  (520.c:29#6)^int32 z;
  (520.c:31#2)^choose {
   -->
    (520.c:31#2)^guard(! (x_int32 ==_int32 0));
    (520.c:31#2)^z =(int32) ! (y_int32 ==_int32 0);
   -->
    (520.c:31#2)^guard((x_int32 ==_int32 0));
    (520.c:31#2)^z =(int32) 0;
  }
}


