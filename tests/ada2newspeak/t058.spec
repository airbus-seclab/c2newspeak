Newspeak output
---------------
t058.adb
void t058(void) {
  (t058.adb:3#6)^int32 y;
  (t058.adb:9#10)^int32 !tmp0;
  (t058.adb:9#10)^{
    int32 t100.arg1;
    (t058.adb:9#10)^0- =(int32) 10;
    (t058.adb:9#10)^t100();
  }
  (t058.adb:5#5)^choose {
   -->
    (t058.adb:5#5)^guard((0-_int32 > 15));
    (t058.adb:7#10)^{
      int32 !tmp1;
      (t058.adb:7#10)^{
        int32 t100.arg1;
        (t058.adb:7#10)^0- =(int32) 2;
        (t058.adb:7#10)^t100();
      }
      (t058.adb:7#10)^2- =(int32) 0-_int32;
    }
   -->
    (t058.adb:5#5)^guard(! (0-_int32 > 15));
    (t058.adb:9#10)^{
      int32 !tmp2;
      (t058.adb:9#10)^{
        int32 t100.arg1;
        (t058.adb:9#10)^0- =(int32) -2;
        (t058.adb:9#10)^t100();
      }
      (t058.adb:9#10)^2- =(int32) 0-_int32;
    }
  }
}


