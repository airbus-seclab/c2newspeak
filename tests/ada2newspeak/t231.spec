Newspeak output
---------------
void t231(void) {
  (t231.adb:7#7)^int32 x;
  (t231.adb:8#7)^int32 y;
  (t231.adb:7#7)^x =(int32) 3;
  (t231.adb:10#8)^y =(int32) 1;
  (t231.adb:11#8)^choose {
   -->
    (t231.adb:11#8)^guard((x_int32 ==_int32 0));
    (t231.adb:12#27)^y =(int32) 0;
   -->
    (t231.adb:11#8)^choose {
     -->
      (t231.adb:11#8)^guard((x_int32 ==_int32 2));
      (t231.adb:13#27)^y =(int32) 5;
     -->
      (t231.adb:11#8)^guard(! (x_int32 ==_int32 2));
      (t231.adb:11#8)^guard(! (x_int32 ==_int32 0));
      (t231.adb:14#27)^y =(int32) -2;
    }
  }
  (t231.adb:16#8)^y =(int32) 8;
}


