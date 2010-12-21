Newspeak output
---------------
void t020(void) {
  (t020.adb:5#6)^uint2 a;
  (t020.adb:6#6)^uint2 b;
  (t020.adb:7#6)^uint3 c;
  (t020.adb:5#6)^a =(uint2) 0;
  (t020.adb:6#6)^b =(uint2) 1;
  (t020.adb:7#6)^c =(uint3) 3;
  (t020.adb:9#5)^choose {
   -->
    (t020.adb:9#5)^guard(! (1 > 1));
    (t020.adb:10#10)^a =(uint2) 0;
   -->
    (t020.adb:9#5)^guard((1 > 1));
    (t020.adb:11#8)^choose {
     -->
      (t020.adb:11#8)^guard((1 ==_uint2 b_uint2));
      (t020.adb:12#10)^b =(uint2) 3;
      (t020.adb:13#10)^c =(uint3) 4;
     -->
      (t020.adb:11#8)^guard(! (1 ==_uint2 b_uint2));
    }
  }
}


