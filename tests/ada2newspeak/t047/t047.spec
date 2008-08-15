Newspeak Object output
----------------------
t047/t047.adb
Global used

Global variables

Function definitions
t047() {
  do {
    uint1 !tmp-1073741823;
    {
      uint1 value_of_t047a.z;
      {
        int32 t047a.z.arg1;
        0- =(int32) 42;
        t047a.z();
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
t047/t047.adb
t047() {
  (t047.adb:7#142)^uint1 !tmp-1073741823;
  (t047.adb:7#142)^{
    uint1 value_of_t047a.z;
    (t047.adb:7#142)^{
      int32 t047a.z.arg1;
      (t047.adb:7#142)^0- =(int32) 42;
      (t047.adb:7#142)^t047a.z();
    }
    (t047.adb:7#142)^1- =(uint1) 0-_uint1;
  }
  (t047.adb:7#142)^choose {
    | 0-_uint1 -->
    | ! 0-_uint1 -->
  }
}


