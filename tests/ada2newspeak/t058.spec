Newspeak Object output
----------------------
t058.adb
Global used

Global variables

Function definitions
t058() {
  do {
    int32 y;
    int32 !tmp-1073741822;
    {
      int32 t100.arg1;
      0- =(int32) belongs[-2147483648,2147483648-1] 10;
      t100();
    }
    choose {
    --> assert((0-_int32 > 15));
        int32 !tmp-1073741821;
        {
          int32 t100.arg1;
          0- =(int32) belongs[-2147483648,2147483648-1] 2;
          t100();
        }
        2- =(int32) belongs[-2147483648,2147483648-1] 0-_int32;
    --> assert((15 > 0-_int32));
        int32 !tmp-1073741820;
        {
          int32 t100.arg1;
          0- =(int32) belongs[-2147483648,2147483648-1] coerce[-2147483648,2147483647] (0 - 2);
          t100();
        }
        2- =(int32) belongs[-2147483648,2147483648-1] 0-_int32;
    }
  } with lbl0: {
  }
}



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


