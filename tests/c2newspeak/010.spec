Newspeak output
---------------
void (010.c:32#5)^main(void) {
  (010.c:33#6)^int32 x;
  (010.c:34#2)^x =(int32) 0;
  (010.c:35#2)^do {
    (010.c:35#2)^while (1) {
      (010.c:35#2)^choose {
       -->
        (010.c:35#2)^guard((10 > x_int32));
       -->
        (010.c:35#2)^guard(! (10 > x_int32));
        (010.c:35#2)^goto lbl1;
      }
      (010.c:36#4)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
    }
  } with lbl1:
}


