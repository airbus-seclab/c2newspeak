Newspeak output
---------------
t058.adb
void t058(void) {
  (t058.adb:3#29)^int32 y;
  (t058.adb:5#51)^int32 !tmp-1073741822;
  (t058.adb:5#51)^{
    int32 t100.arg1;
    (t058.adb:5#51)^0- =(int32) 10;
    (t058.adb:5#51)^t100();
  }
  (t058.adb:5#51)^choose {
   -->
    (t058.adb:5#51)^guard((0-_int32 > 15));
    (t058.adb:7#80)^{
      int32 !tmp-1073741821;
      (t058.adb:7#80)^{
        int32 t100.arg1;
        (t058.adb:7#80)^0- =(int32) 2;
        (t058.adb:7#80)^t100();
      }
      (t058.adb:7#80)^2- =(int32) 0-_int32;
    }
   -->
    (t058.adb:5#51)^guard(! (0-_int32 > 15));
    (t058.adb:9#108)^{
      int32 !tmp-1073741820;
      (t058.adb:9#108)^{
        int32 t100.arg1;
        (t058.adb:9#108)^0- =(int32) -2;
        (t058.adb:9#108)^t100();
      }
      (t058.adb:9#108)^2- =(int32) 0-_int32;
    }
  }
}


