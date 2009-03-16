Newspeak output
---------------
t240.adb
void t240(void) {
  (t240.adb:7#136)^int32 x;
  (t240.adb:9#159)^int32 i;
  (t240.adb:9#159)^0- =(int32) 4;
  (t240.adb:9#159)^do {
    (t240.adb:9#159)^while (1) {
      (t240.adb:9#159)^choose {
       -->
        (t240.adb:9#159)^guard((0-_int32 > 8));
        (t240.adb:9#159)^goto lbl1;
       -->
        (t240.adb:9#159)^guard(! (0-_int32 > 8));
      }
      (t240.adb:10#182)^1- =(int32) 3;
      (t240.adb:9#159)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl1: {
  }
  (t240.adb:12#212)^{
    int32 j;
    (t240.adb:12#212)^0- =(int32) 16;
    (t240.adb:12#212)^do {
      (t240.adb:12#212)^while (1) {
        (t240.adb:12#212)^choose {
         -->
          (t240.adb:12#212)^guard((12 > 0-_int32));
          (t240.adb:12#212)^goto lbl2;
         -->
          (t240.adb:12#212)^guard(! (12 > 0-_int32));
        }
        (t240.adb:13#245)^2- =(int32) 8;
        (t240.adb:12#212)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 - 1);
      }
    } with lbl2: {
    }
    (t240.adb:15#275)^2- =(int32) 58;
  }
}


