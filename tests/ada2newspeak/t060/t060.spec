Newspeak Object output
----------------------
t060/t060.adb
Global used

Global variables

Function definitions
t060() {
  do {
    int32 x;
    uint1 y;
    {
      int32 value_of_t060a.b;
      {
        int32 t060a.b.arg1;
        0- =(int32) 2;
        t060a.b();
      }
      2- =(int32) 0-_int32;
    }
    0- =(uint1) 1;
  } with lbl0: {
  }
}



Newspeak output
---------------
t060/t060.adb
t060() {
  (t060.adb:3#40)^int32 x;
  (t060.adb:4#54)^uint1 y;
  (t060.adb:6#71)^{
    int32 value_of_t060a.b;
    (t060.adb:6#71)^{
      int32 t060a.b.arg1;
      (t060.adb:6#71)^0- =(int32) 2;
      (t060.adb:6#71)^t060a.b();
    }
    (t060.adb:6#71)^2- =(int32) 0-_int32;
  }
  (t060.adb:7#89)^0- =(uint1) 1;
}


