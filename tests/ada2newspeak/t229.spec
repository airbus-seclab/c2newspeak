Newspeak output
---------------
t229.adb
void t229(void) {
  (t229.adb:9#161)^int32 x;
  (t229.adb:10#194)^int32 y;
  (t229.adb:9#161)^1- =(int32) 3;
  (t229.adb:12#217)^choose {
   -->
    (t229.adb:12#217)^guard((1-_int32 ==_int32 3));
    (t229.adb:13#231)^0- =(int32) 3;
    (t229.adb:13#231)^0- =(int32) 2;
   -->
    (t229.adb:12#217)^choose {
     -->
      (t229.adb:12#217)^guard((1-_int32 ==_int32 4));
      (t229.adb:14#265)^0- =(int32) 4;
      (t229.adb:14#265)^0- =(int32) 5;
     -->
      (t229.adb:12#217)^choose {
       -->
        (t229.adb:12#217)^guard((1-_int32 ==_int32 5));
        (t229.adb:15#299)^0- =(int32) 5;
        (t229.adb:15#299)^0- =(int32) 8;
       -->
        (t229.adb:12#217)^choose {
         -->
          (t229.adb:12#217)^guard((1-_int32 ==_int32 6));
          (t229.adb:16#333)^0- =(int32) 7;
          (t229.adb:16#333)^0- =(int32) 8;
         -->
          (t229.adb:12#217)^guard(! (1-_int32 ==_int32 6));
          (t229.adb:12#217)^guard(! (1-_int32 ==_int32 5));
          (t229.adb:12#217)^guard(! (1-_int32 ==_int32 4));
          (t229.adb:12#217)^guard(! (1-_int32 ==_int32 3));
        }
      }
    }
  }
}


