Newspeak output
---------------
t017.adb
void t017(void) {
  (t017.adb:5#114)^uint2 a;
  (t017.adb:6#134)^uint2 b;
  (t017.adb:7#154)^uint3 c;
  (t017.adb:5#114)^2- =(uint2) 0;
  (t017.adb:6#134)^1- =(uint2) 1;
  (t017.adb:7#154)^0- =(uint3) 3;
  (t017.adb:9#180)^choose {
   -->
    (t017.adb:9#180)^guard(! (0 > 1));
    (t017.adb:10#200)^2- =(uint2) 0;
   -->
    (t017.adb:9#180)^guard((0 > 1));
    (t017.adb:11#215)^choose {
     -->
      (t017.adb:11#215)^guard((1 ==_uint2 1-_uint2));
      (t017.adb:12#236)^1- =(uint2) 3;
      (t017.adb:13#251)^0- =(uint3) 4;
     -->
      (t017.adb:11#215)^guard(! (1 ==_uint2 1-_uint2));
    }
  }
}


