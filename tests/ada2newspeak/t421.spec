Newspeak output
---------------
t421.adb
void t421(void) {
  (t421.adb:7#9)^uint4 data;
  (t421.adb:8#9)^uint4 res;
  (t421.adb:10#6)^uint4 z;
  (t421.adb:10#6)^z =(uint4) 5;
  (t421.adb:12#8)^choose {
   -->
    (t421.adb:12#8)^guard((data_uint4 ==_uint4 5));
    (t421.adb:13#15)^data =(uint4) belongs[5,15] z_uint4;
   -->
    (t421.adb:12#8)^guard(! (data_uint4 ==_uint4 5));
  }
  (t421.adb:10#6)^z =(uint4) 10;
  (t421.adb:12#8)^choose {
   -->
    (t421.adb:12#8)^guard((data_uint4 ==_uint4 5));
    (t421.adb:13#15)^data =(uint4) belongs[5,15] z_uint4;
   -->
    (t421.adb:12#8)^guard(! (data_uint4 ==_uint4 5));
  }
  (t421.adb:10#6)^z =(uint4) 15;
  (t421.adb:12#8)^choose {
   -->
    (t421.adb:12#8)^guard((data_uint4 ==_uint4 5));
    (t421.adb:13#15)^data =(uint4) belongs[5,15] z_uint4;
   -->
    (t421.adb:12#8)^guard(! (data_uint4 ==_uint4 5));
  }
}


