Newspeak output
---------------
t229.adb
void t229(void) {
  (t229.adb:9#7)^int32 x;
  (t229.adb:10#7)^int32 y;
  (t229.adb:9#7)^1- =(int32) 3;
  (t229.adb:12#8)^choose {
   -->
    (t229.adb:12#8)^guard((1-_int32 ==_int32 3));
    (t229.adb:13#22)^0- =(int32) 3;
    (t229.adb:13#30)^0- =(int32) 2;
   -->
    (t229.adb:12#8)^choose {
     -->
      (t229.adb:12#8)^guard((1-_int32 ==_int32 4));
      (t229.adb:14#22)^0- =(int32) 4;
      (t229.adb:14#30)^0- =(int32) 5;
     -->
      (t229.adb:12#8)^choose {
       -->
        (t229.adb:12#8)^guard((1-_int32 ==_int32 5));
        (t229.adb:15#22)^0- =(int32) 5;
        (t229.adb:15#30)^0- =(int32) 8;
       -->
        (t229.adb:12#8)^choose {
         -->
          (t229.adb:12#8)^guard((1-_int32 ==_int32 6));
          (t229.adb:16#22)^0- =(int32) 7;
          (t229.adb:16#30)^0- =(int32) 8;
         -->
          (t229.adb:12#8)^guard(! (1-_int32 ==_int32 6));
          (t229.adb:12#8)^guard(! (1-_int32 ==_int32 5));
          (t229.adb:12#8)^guard(! (1-_int32 ==_int32 4));
          (t229.adb:12#8)^guard(! (1-_int32 ==_int32 3));
        }
      }
    }
  }
}


