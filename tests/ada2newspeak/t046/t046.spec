Newspeak Object output
----------------------
t046/t046.adb
Global used

Global variables

Function definitions
t046() {
  do {
    uint1 !tmp-1073741823;
    {
      uint1 value_of_t046a.z;
      {
        int32 t046a.z.arg1;
        0- =(int32) 42;
        t046a.z();
      }
      1- =(uint1) 0-_uint1;
    }
    choose {
    --> assert(0-_uint1);
    --> assert(! 0-_uint1);
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t046/t046.adb
t046() {
  (t046.adb:6#90)^uint1 !tmp-1073741823;
  (t046.adb:6#90)^{
    uint1 value_of_t046a.z;
    (t046.adb:6#90)^{
      int32 t046a.z.arg1;
      (t046.adb:6#90)^0- =(int32) 42;
      (t046.adb:6#90)^t046a.z();
    }
    (t046.adb:6#90)^1- =(uint1) 0-_uint1;
  }
  (t046.adb:6#90)^choose {
    | 0-_uint1 -->
    | ! 0-_uint1 -->
  }
}


