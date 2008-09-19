Newspeak Object output
----------------------
t047.adb
Global used

Global variables

Function definitions
t047() {
  do {
    uint1 !tmp-1073741823;
    {
      uint1 value_of_t094.z;
      {
        int32 t094.z.arg1;
        0- =(int32) belongs[-2147483648,2147483648-1] 42;
        t094.z();
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
t047.adb
t047() {
  (t047.adb:7#140)^uint1 !tmp-1073741823;
  (t047.adb:7#140)^{
    uint1 value_of_t094.z;
    (t047.adb:7#140)^{
      int32 t094.z.arg1;
      (t047.adb:7#140)^0- =(int32) 42;
      (t047.adb:7#140)^t094.z();
    }
    (t047.adb:7#140)^1- =(uint1) 0-_uint1;
  }
  (t047.adb:7#140)^choose {
    | 0-_uint1 -->
    | ! 0-_uint1 -->
  }
}


