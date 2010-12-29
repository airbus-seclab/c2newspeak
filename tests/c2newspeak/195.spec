Newspeak output
---------------
void (195.c:26#5)^main(void) {
  (195.c:27#6)^int32 x;
  (195.c:28#6)^int32 y;
  (195.c:30#2)^y =(int32) 1;
  (195.c:31#2)^choose {
   -->
    (195.c:31#2)^guard(! (x_int32 ==_int32 0));
    (195.c:31#2)^x =(int32) ! (x_int32 ==_int32 0);
   -->
    (195.c:31#2)^guard((x_int32 ==_int32 0));
    (195.c:31#2)^x =(int32) 0;
  }
}


