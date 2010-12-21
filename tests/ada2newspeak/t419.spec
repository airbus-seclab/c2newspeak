Newspeak output
---------------
void t419(void) {
  (t419.adb:7#9)^uint4 data;
  (t419.adb:10#6)^uint4 z;
  (t419.adb:10#6)^z =(uint4) 5;
  (t419.adb:11#8)^choose {
   -->
    (t419.adb:11#8)^guard((data_uint4 ==_uint4 5));
    (t419.adb:12#15)^data =(uint4) belongs[5,15] z_uint4;
   -->
    (t419.adb:11#8)^guard(! (data_uint4 ==_uint4 5));
  }
  (t419.adb:10#6)^z =(uint4) 10;
  (t419.adb:11#8)^choose {
   -->
    (t419.adb:11#8)^guard((data_uint4 ==_uint4 5));
    (t419.adb:12#15)^data =(uint4) belongs[5,15] z_uint4;
   -->
    (t419.adb:11#8)^guard(! (data_uint4 ==_uint4 5));
  }
  (t419.adb:10#6)^z =(uint4) 15;
  (t419.adb:11#8)^choose {
   -->
    (t419.adb:11#8)^guard((data_uint4 ==_uint4 5));
    (t419.adb:12#15)^data =(uint4) belongs[5,15] z_uint4;
   -->
    (t419.adb:11#8)^guard(! (data_uint4 ==_uint4 5));
  }
}


