Newspeak output
---------------
010.c
void main(void) {
  (010.c:33#1224)^int32 x;
  (010.c:34#1229)^x =(int32) 0;
  (010.c:32#1204)^do {
    (010.c:35#1238)^while (1) {
      (010.c:35#1238)^choose {
       -->
        (010.c:35#1238)^guard((10 > x_int32));
       -->
        (010.c:35#1238)^guard(! (10 > x_int32));
        (010.c:35#1238)^goto lbl0;
      }
      (010.c:36#1259)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
    }
  } with lbl0: {
  }
}


