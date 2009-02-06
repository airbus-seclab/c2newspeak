Newspeak output
---------------
272.c
void main(void) {
  (272.c:27#6)^int32 x;
  (272.c:28#6)^int32 y;
  (272.c:29#2)^choose {
   -->
    (272.c:29#2)^guard(! (1-_int32 ==_int32 0));
    (272.c:29#2)^0- =(int32) 0;
   -->
    (272.c:29#2)^guard((1-_int32 ==_int32 0));
    (272.c:29#2)^0- =(int32) 1;
  }
}


