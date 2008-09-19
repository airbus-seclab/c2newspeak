Newspeak Object output
----------------------
t046.adb
Global used

Global variables

Function definitions
t046() {
  do {
    uint1 !tmp-1073741823;
    {
      uint1 value_of_t093.z;
      {
        int32 t093.z.arg1;
        0- =(int32) belongs[-2147483648,2147483648-1] 42;
        t093.z();
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
t046.adb
t046() {
  (t046.adb:6#88)^uint1 !tmp-1073741823;
  (t046.adb:6#88)^{
    uint1 value_of_t093.z;
    (t046.adb:6#88)^{
      int32 t093.z.arg1;
      (t046.adb:6#88)^0- =(int32) 42;
      (t046.adb:6#88)^t093.z();
    }
    (t046.adb:6#88)^1- =(uint1) 0-_uint1;
  }
  (t046.adb:6#88)^choose {
    | 0-_uint1 -->
    | ! 0-_uint1 -->
  }
}


