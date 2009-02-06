Newspeak output
---------------
009.c
int32 main(void) {
  (009.c:32#6)^int32 x;
  (009.c:31#4)^do {
    (009.c:33#2)^choose {
     -->
      (009.c:33#2)^guard((10 > 0-_int32));
      (009.c:34#4)^1- =(int32) 1;
      (009.c:34#4)^goto lbl0;
     -->
      (009.c:33#2)^guard(! (10 > 0-_int32));
    }
    (009.c:36#2)^1- =(int32) 0;
  } with lbl0: {
  }
}


