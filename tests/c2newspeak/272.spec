Newspeak output
---------------
void (272.c:26#5)^main(void) {
  (272.c:27#6)^int32 x;
  (272.c:28#6)^int32 y;
  (272.c:29#2)^choose {
   -->
    (272.c:29#2)^guard(! (x_int32 ==_int32 0));
    (272.c:29#2)^y =(int32) 0;
   -->
    (272.c:29#2)^guard((x_int32 ==_int32 0));
    (272.c:29#2)^y =(int32) 1;
  }
}


