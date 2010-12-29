Newspeak output
---------------
void (094.c:26#5)^main(void) {
  (094.c:27#6)^int32 x;
  (094.c:28#2)^choose {
   -->
    (094.c:28#2)^guard(! (0 > x_int32));
    (094.c:28#2)^x =(int32) (10 > x_int32);
   -->
    (094.c:28#2)^guard((0 > x_int32));
    (094.c:28#2)^x =(int32) 0;
  }
}


