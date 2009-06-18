Newspeak output
---------------
t240.adb
void t240(void) {
  (t240.adb:7#7)^int32 x;
  (t240.adb:9#7)^int32 i;
  (t240.adb:9#7)^0- =(int32) 4;
  (t240.adb:9#7)^do {
    (t240.adb:9#7)^while (1) {
      (t240.adb:9#7)^choose {
       -->
        (t240.adb:9#7)^guard((0-_int32 > 8));
        (t240.adb:9#7)^goto lbl1;
       -->
        (t240.adb:9#7)^guard(! (0-_int32 > 8));
      }
      (t240.adb:10#12)^1- =(int32) 3;
      (t240.adb:9#7)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl1: {
  }
  (t240.adb:12#7)^{
    int32 j;
    (t240.adb:12#7)^0- =(int32) 16;
    (t240.adb:12#7)^do {
      (t240.adb:12#7)^while (1) {
        (t240.adb:12#7)^choose {
         -->
          (t240.adb:12#7)^guard((12 > 0-_int32));
          (t240.adb:12#7)^goto lbl2;
         -->
          (t240.adb:12#7)^guard(! (12 > 0-_int32));
        }
        (t240.adb:13#12)^2- =(int32) 8;
        (t240.adb:12#7)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 - 1);
      }
    } with lbl2: {
    }
    (t240.adb:15#8)^2- =(int32) 58;
  }
}


