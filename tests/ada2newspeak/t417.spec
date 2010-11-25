Newspeak output
---------------
t417.adb
void t417(void) {
  (t417.adb:4#9)^uint2 data;
  (t417.adb:5#9)^uint1 res;
  (t417.adb:7#6)^uint1 z;
  (t417.adb:7#6)^z =(uint1) 0;
  (t417.adb:7#6)^z =(uint1) 0;
  (t417.adb:8#8)^choose {
   -->
    (t417.adb:8#8)^guard((data_uint2 ==_uint2 0));
    (t417.adb:9#15)^data =(uint2) z_uint1;
   -->
    (t417.adb:8#8)^guard(! (data_uint2 ==_uint2 0));
  }
  (t417.adb:7#6)^z =(uint1) 1;
  (t417.adb:8#8)^choose {
   -->
    (t417.adb:8#8)^guard((data_uint2 ==_uint2 0));
    (t417.adb:9#15)^data =(uint2) z_uint1;
   -->
    (t417.adb:8#8)^guard(! (data_uint2 ==_uint2 0));
  }
}


