Newspeak output
---------------
271.c
void main(void) {
  (271.c:27#6)^int32 x;
  (271.c:28#6)^int32 y;
  (271.c:29#2)^choose {
   -->
    (271.c:29#2)^guard(! (1-_int32 ==_int32 0));
    (271.c:29#2)^0- =(int32) 0;
   -->
    (271.c:29#2)^guard((1-_int32 ==_int32 0));
    (271.c:29#2)^0- =(int32) 1;
  }
}


