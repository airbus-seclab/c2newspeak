Newspeak output
---------------
t420.adb
void t420(void) {
  (t420.adb:9#9)^uint4 data;
  (t420.adb:10#9)^uint4 res;
  (t420.adb:12#6)^uint4 z;
  (t420.adb:12#6)^z =(uint4) 5;
  (t420.adb:14#7)^choose {
   -->
    (t420.adb:14#7)^guard((data_uint4 ==_uint4 5));
    (t420.adb:17#15)^data =(uint4) belongs[5,15] z_uint4;
   -->
    (t420.adb:14#7)^guard(! (data_uint4 ==_uint4 5));
  }
  (t420.adb:12#6)^z =(uint4) 10;
  (t420.adb:14#7)^choose {
   -->
    (t420.adb:14#7)^guard((data_uint4 ==_uint4 5));
    (t420.adb:17#15)^data =(uint4) belongs[5,15] z_uint4;
   -->
    (t420.adb:14#7)^guard(! (data_uint4 ==_uint4 5));
  }
}


