Newspeak output
---------------
void (223.c:26#5)^main(void) {
  (223.c:27#6)^int32 x;
  (223.c:28#2)^choose {
   -->
    (223.c:28#2)^guard((10 > x_int32));
    (223.c:29#4)^x =(int32) 1;
   -->
    (223.c:28#2)^guard(! (10 > x_int32));
  }
}


