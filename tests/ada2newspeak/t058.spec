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
      int32 value_of_t058a;
      {
        int32 t058a.arg1;
        0- =(int32) 10;
        t058a();
      }
      1- =(int32) 0-_int32;
    }
    choose {
    --> assert((0-_int32 > 15));
        int32 value_of_t058a;
        {
          int32 t058a.arg1;
          0- =(int32) 2;
          t058a();
        }
        2- =(int32) 0-_int32;
    --> assert((15 > 0-_int32));
        int32 value_of_t058a;
        {
          int32 t058a.arg1;
          0- =(int32) (0 - 2);
          t058a();
        }
        2- =(int32) 0-_int32;
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t058.adb
t058() {
  (t058.adb:3#29)^int32 y;
  (t058.adb:5#51)^int32 !tmp-1073741822;
  (t058.adb:5#51)^{
    int32 value_of_t058a;
    (t058.adb:5#51)^{
      int32 t058a.arg1;
      (t058.adb:5#51)^0- =(int32) 10;
      (t058.adb:5#51)^t058a();
    }
    (t058.adb:5#51)^1- =(int32) 0-_int32;
  }
  (t058.adb:5#51)^choose {
    | (0-_int32 > 15) -->
      (t058.adb:7#81)^int32 value_of_t058a;
      (t058.adb:7#81)^{
        int32 t058a.arg1;
        (t058.adb:7#81)^0- =(int32) 2;
        (t058.adb:7#81)^t058a();
      }
      (t058.adb:7#81)^2- =(int32) 0-_int32;
    | ! (0-_int32 > 15) -->
      (t058.adb:9#110)^int32 value_of_t058a;
      (t058.adb:9#110)^{
        int32 t058a.arg1;
        (t058.adb:9#110)^0- =(int32) -2;
        (t058.adb:9#110)^t058a();
      }
      (t058.adb:9#110)^2- =(int32) 0-_int32;
  }
}


