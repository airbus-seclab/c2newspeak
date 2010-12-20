Newspeak output
---------------
void t229(void) {
  (t229.adb:9#7)^int32 x;
  (t229.adb:10#7)^int32 y;
  (t229.adb:9#7)^x =(int32) 3;
  (t229.adb:12#8)^choose {
   -->
    (t229.adb:12#8)^guard((x_int32 ==_int32 3));
    (t229.adb:13#22)^y =(int32) 3;
    (t229.adb:13#30)^y =(int32) 2;
   -->
    (t229.adb:12#8)^choose {
     -->
      (t229.adb:12#8)^guard((x_int32 ==_int32 4));
      (t229.adb:14#22)^y =(int32) 4;
      (t229.adb:14#30)^y =(int32) 5;
     -->
      (t229.adb:12#8)^choose {
       -->
        (t229.adb:12#8)^guard((x_int32 ==_int32 5));
        (t229.adb:15#22)^y =(int32) 5;
        (t229.adb:15#30)^y =(int32) 8;
       -->
        (t229.adb:12#8)^choose {
         -->
          (t229.adb:12#8)^guard((x_int32 ==_int32 6));
          (t229.adb:16#22)^y =(int32) 7;
          (t229.adb:16#30)^y =(int32) 8;
         -->
          (t229.adb:12#8)^guard(! (x_int32 ==_int32 6));
          (t229.adb:12#8)^guard(! (x_int32 ==_int32 5));
          (t229.adb:12#8)^guard(! (x_int32 ==_int32 4));
          (t229.adb:12#8)^guard(! (x_int32 ==_int32 3));
        }
      }
    }
  }
}


