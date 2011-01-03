Newspeak output
---------------
void (681.c:26#5)^main(void) {
  (681.c:27#6)^int32 x;
  (681.c:28#2)^choose {
   -->
    (681.c:28#2)^guard(! (x_int32 ==_int32 0));
    (681.c:28#15)^x =(int32) 0;
   -->
    (681.c:28#2)^guard((x_int32 ==_int32 0));
  }
}


