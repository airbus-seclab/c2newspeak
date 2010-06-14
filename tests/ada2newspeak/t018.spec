Newspeak output
---------------
t018.adb
void t018(void) {
  (t018.adb:6#7)^uint3 x1;
  (t018.adb:7#7)^uint3 x2;
  (t018.adb:8#7)^uint3 x3;
  (t018.adb:6#7)^x1 =(uint3) 1;
  (t018.adb:7#7)^x2 =(uint3) 6;
  (t018.adb:8#7)^x3 =(uint3) 3;
  (t018.adb:10#5)^choose {
   -->
    (t018.adb:10#5)^guard((0 ==_uint3 x1_uint3));
   -->
    (t018.adb:10#5)^guard(! (0 ==_uint3 x1_uint3));
    (t018.adb:12#8)^choose {
     -->
      (t018.adb:12#8)^guard((2 ==_uint3 x2_uint3));
     -->
      (t018.adb:12#8)^guard(! (2 ==_uint3 x2_uint3));
      (t018.adb:14#8)^choose {
       -->
        (t018.adb:14#8)^guard((4 ==_uint3 x3_uint3));
       -->
        (t018.adb:14#8)^guard(! (4 ==_uint3 x3_uint3));
      }
    }
  }
}


