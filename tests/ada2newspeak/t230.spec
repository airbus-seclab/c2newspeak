Newspeak output
---------------
void t230(void) {
  (t230.adb:7#7)^int32 x;
  (t230.adb:8#7)^int32 y;
  (t230.adb:7#7)^x =(int32) 0;
  (t230.adb:10#8)^choose {
   -->
    (t230.adb:10#8)^guard((x_int32 ==_int32 0));
    (t230.adb:11#24)^y =(int32) 5;
   -->
    (t230.adb:10#8)^choose {
     -->
      (t230.adb:10#8)^guard((x_int32 ==_int32 1));
      (t230.adb:11#24)^y =(int32) 5;
     -->
      (t230.adb:10#8)^choose {
       -->
        (t230.adb:10#8)^guard((x_int32 ==_int32 2));
        (t230.adb:12#24)^y =(int32) 3;
       -->
        (t230.adb:10#8)^guard(! (x_int32 ==_int32 2));
        (t230.adb:10#8)^guard(! (x_int32 ==_int32 1));
        (t230.adb:10#8)^guard(! (x_int32 ==_int32 0));
      }
    }
  }
}


