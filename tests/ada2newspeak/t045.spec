Newspeak Object output
----------------------
t045.adb
Global used

Global variables

Function definitions
t045() {
  do {
    uint1 !tmp-1073741823;
    {
      uint1 value_of_t092.z;
      {
        int32 t092.z.arg1;
        0- =(int32) belongs[-2147483648,2147483648-1] 42;
        t092.z();
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
t045.adb
t045() {
  (t045.adb:6#61)^uint1 !tmp-1073741823;
  (t045.adb:6#61)^{
    uint1 value_of_t092.z;
    (t045.adb:6#61)^{
      int32 t092.z.arg1;
      (t045.adb:6#61)^0- =(int32) 42;
      (t045.adb:6#61)^t092.z();
    }
    (t045.adb:6#61)^1- =(uint1) 0-_uint1;
  }
  (t045.adb:6#61)^choose {
    | 0-_uint1 -->
    | ! 0-_uint1 -->
  }
}


