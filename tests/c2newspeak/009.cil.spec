Newspeak output
---------------
009.c
int32 main(void) {
  (009.c:32#1195)^int32 x;
  (009.c:31#1176)^do {
    (009.c:33#1200)^choose {
     -->
      (009.c:33#1200)^guard((10 > x_int32));
      (009.c:34#1218)^!return =(int32) 1;
      (009.c:34#1218)^goto lbl0;
     -->
      (009.c:33#1200)^guard(! (10 > x_int32));
    }
    (009.c:36#1234)^!return =(int32) 0;
  } with lbl0: {
  }
}


