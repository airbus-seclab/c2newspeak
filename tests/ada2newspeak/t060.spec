Newspeak Object output
----------------------
t060.adb
Global used

Global variables

Function definitions
t060() {
  do {
    int32 x;
    uint1 y;
    {
      int32 !tmp-1073741821;
      {
        int32 value_of_t102.b;
        {
          int32 t102.b.arg1;
          0- =(int32) belongs[-2147483648,2147483648-1] 2;
          t102.b();
        }
        1- =(int32) 0-_int32;
      }
      2- =(int32) belongs[-2147483648,2147483648-1] 0-_int32;
    }
    0- =(uint1) 1;
  } with lbl0: {
  }
}



Newspeak output
---------------
t060.adb
t060() {
  (t060.adb:3#38)^int32 x;
  (t060.adb:4#52)^uint1 y;
  (t060.adb:6#69)^{
    int32 !tmp-1073741821;
    (t060.adb:6#69)^{
      int32 value_of_t102.b;
      (t060.adb:6#69)^{
        int32 t102.b.arg1;
        (t060.adb:6#69)^0- =(int32) 2;
        (t060.adb:6#69)^t102.b();
      }
      (t060.adb:6#69)^1- =(int32) 0-_int32;
    }
    (t060.adb:6#69)^2- =(int32) 0-_int32;
  }
  (t060.adb:7#86)^0- =(uint1) 1;
}


