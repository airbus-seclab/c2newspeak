Newspeak output
---------------
521.c
void main(void) {
  (521.c:27#6)^int32 x;
  (521.c:28#6)^int32 y;
  (521.c:29#6)^int32 z;
  (521.c:31#2)^choose {
   -->
    (521.c:31#2)^guard(! (2-_int32 ==_int32 0));
    (521.c:31#2)^0- =(int32) 1;
   -->
    (521.c:31#2)^guard((2-_int32 ==_int32 0));
    (521.c:31#2)^0- =(int32) ! (1-_int32 ==_int32 0);
  }
}


